;;; cil-compiler.lisp: Lisp CIL compiler (A2 instruction list architecture)
;;;
;;; Pure-functional compiler: S-expression in -> instruction list out.
;;; Each compile-* function returns a flat list of CIL instructions.
;;; No .NET API calls. The C# CilAssembler walks the list and calls ILGenerator.

(defpackage :dotcl.cil-compiler
  (:use :cl)
  (:export #:compile-toplevel #:compile-toplevel-eval
           #:*cross-compiling* #:*compile-file-mode* #:*concatenate-build*
           #:*emit-source-lines* #:*emit-frame-locals*
           #:compile-file-concatenated))

;; %INLINE-CS-SPLICED is the dispatch symbol for the dotcl-cs:inline-cs
;; macro. It needs to be reachable
;; via Startup.Sym (which
;; checks CL -> DOTCL-INTERNAL -> cross-package bridge) so that the
;; runtime LOAD-SYM resolution and the cross-compiled handler
;; registration converge on the same Symbol instance. CL is locked,
;; so we use DOTCL-INTERNAL (also universally searched by Startup.Sym).
#-dotcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "DOTCL-INTERNAL")
    (make-package "DOTCL-INTERNAL"))
  (intern "%INLINE-CS-SPLICED" "DOTCL-INTERNAL"))
(in-package :dotcl.cil-compiler)

;;; ============================================================
;;; Compilation context (dynamic variables)
;;; ============================================================

(defvar *cross-compiling* nil
  "T when running in SBCL as cross-compiler; NIL in self-hosted mode.")

(defvar *emit-source-lines* nil
  "When non-NIL, a function (fn form) -> source-line-or-NIL. COMPILE-EXPR calls it
   per compiled cons form and, on a non-NIL line, prepends a (:line N) marker so the
   assembler can emit a debug sequence point. NIL (default, incl. cross-compile)
   disables it entirely; output is unchanged. Set by COMPILE-FILE under
   DOTCL_EMIT_PDB. Forms produced by macroexpansion aren't in the source map, so
   funcall returns NIL for them and they get no marker (they read as #line hidden).")

(defvar *emit-frame-locals* nil
  "When true, every compiled function body opens a runtime DebugFrame at entry and
   each user lexical binding stores itself there, so the in-process debugger
   (sldb's frame locals, the :bt/:locals commands) can read a running frame's
   variables by name via DOTCL:FRAME-LOCALS. NIL by default (incl. cross-compile):
   no (:frame-enter)/(:frame-set) instructions are emitted and the output is
   unchanged. Independent of *EMIT-SOURCE-LINES*, which drives the Portable PDB
   for out-of-process debuggers; this one is for the CL-native debugger.
   User-visible as DOTCL:*EMIT-FRAME-LOCALS* (the same symbol; Startup imports it).")


(defvar *backing-arm* :both
  "Which arm a hoisted element access emits.

   :BOTH   the null test and both arms, for an access that is not inside a
           body the caller unswitched.
   :FAST   the bare ldelem only. Legal only where the caller has already
           tested every hoisted buffer in scope and branched.
   :HELPER the per-element helper only, for the copy the caller emits for the
           case where some buffer was NIL.

   The test is loop-invariant, so leaving it at the access is what costs: on
   the array-walk kernel the per-access form turned a 7-instruction single
   block with a fall-through back-edge into 11 instructions across 4 blocks,
   because the test also stopped the JIT hoisting the data base and the length
   out of the loop. Testing once and emitting the body twice puts the fast copy
   back to the original block.")

(defvar *compile-file-mode* nil
  "T when compiling via compile-file. Controls eval-when behavior per CLHS 3.2.3.1:
   :compile-toplevel -> eval at compile time, :load-toplevel -> emit CIL for load.")

(defvar *concatenate-build* nil
  "T only while compile-file'ing a project-core CONCATENATED build (the single
   file produced by asdf::concatenate-files / concatenate-source-op from a
   system's :components).  A normal multi-file ASDF load-op interleaves
   compile+load per component, so a toplevel (require ...) / (use-package ...) in
   an earlier component takes effect before later components compile.  The concat
   build compiles everything as one unit and loses that, so here we evaluate such
   toplevel module/package setup forms at compile time (see compile-form).  Not
   set for ordinary user compile-file; those keep standard CL semantics.")

(defun compile-file-concatenated (input output)
  "compile-file INPUT to OUTPUT as a project-core CONCATENATED build:
   binds *concatenate-build* so compile-form evaluates toplevel module/package
   setup forms at compile time.  Must be defined here (cross-compiled) and called
   by name from the C# build driver; binding *concatenate-build* from
   reader-read code would bind a different Symbol instance than the one
   compile-form reads (Startup.Sym vs Reader identity), so the dynamic binding
   would not connect.  Function calls resolve by name, so calling this works."
  (let ((*concatenate-build* t))
    (compile-file input :output-file output)))

(defvar *dotnet-type-references* nil
  "When non-NIL, an EQUAL hash table that COMPILE-FORM fills with every .NET
   type name written as a literal string in a type position of a DOTNET: call
   (see %NOTE-DOTNET-TYPE-REFERENCES). The project build reads it to root those
   types for the trimmer: a fasl is loaded at run time, so the trimmer never
   sees what it calls through reflection.")

(defun compile-file-concatenated-collecting (input output)
  "COMPILE-FILE-CONCATENATED that also returns the .NET type names the unit
   names literally (a fresh list of strings). Called by name from the C# build
   driver for the same symbol-identity reason as COMPILE-FILE-CONCATENATED."
  (let ((*dotnet-type-references* (make-hash-table :test #'equal))
        (names '()))
    (compile-file-concatenated input output)
    (maphash (lambda (k v) (declare (ignore v)) (push k names))
             *dotnet-type-references*)
    names))

(defun %note-dotnet-type-name (x)
  (when (stringp x)
    (setf (gethash x *dotnet-type-references*) t)))

(defun %note-dotnet-type-list (x)
  "Note each literal string of a type-argument list written as (LIST \"A\" ...)
   or '(\"A\" ...)."
  (let ((items (cond ((and (consp x) (symbolp (car x))
                           (string= (symbol-name (car x)) "LIST"))
                      (cdr x))
                     ((and (consp x) (eq (car x) 'quote) (consp (cdr x))
                           (listp (cadr x)))
                      (cadr x)))))
    (when (listp items)
      (dolist (i items) (%note-dotnet-type-name i)))))

(defun %note-dotnet-type-references (expr)
  "Record the literal type names of a DOTNET: call EXPR. Only the argument
   positions that take a type are looked at, so a member name such as
   \"Append\" is never taken for a type. The compiler's own lowering of a typed
   call, %DOTNET-CALL-DIRECT, carries the resolved full name first."
  (let ((op (car expr)) (args (cdr expr)))
    (when (and (symbolp op) (listp args))
      (let ((name (symbol-name op))
            (pkg (symbol-package op)))
        (cond
          ((string= name "%DOTNET-CALL-DIRECT")
           (%note-dotnet-type-name (car args)))
          ((not (and pkg (string= (package-name pkg) "DOTNET"))))
          ((member name '("NEW" "STATIC" "%SET-STATIC" "RESOLVE-TYPE" "NEW-ARRAY"
                          "MAKE-ARRAY" "MAKE-DELEGATE" "MEMBERS" "CLASS-FOR-TYPE"
                          "ENUM-OR")
                   :test #'string=)
           (%note-dotnet-type-name (car args)))
          ((member name '("CAST" "BOX" "IS-INSTANCE-OF" "EXCEPTION-TYPEP")
                   :test #'string=)
           (%note-dotnet-type-name (cadr args)))
          ((string= name "MAKE-GENERIC-TYPE")
           (%note-dotnet-type-name (car args))
           (%note-dotnet-type-list (cadr args)))
          ((member name '("STATIC-GENERIC" "CALL-OUT-GENERIC") :test #'string=)
           (%note-dotnet-type-name (car args))
           (%note-dotnet-type-list (caddr args)))
          ((string= name "INVOKE-GENERIC")
           (%note-dotnet-type-list (caddr args))))))))

;;; ------------------------------------------------------------
;;; define-compile-state: closure-boundary reset registry
;;; ------------------------------------------------------------
;;; compile-closure-body must rebind every per-compilation state variable to a
;;; fresh value so an inner closure does not inherit the enclosing body's
;;; compile context. Declaring such a variable with DEFINE-COMPILE-STATE
;;; (instead of plain defvar) makes it participate in that reset
;;; automatically. The registry is populated at LOAD time by the expansion's
;;; %register-closure-fresh call (a runtime registration, not a compile-time
;;; side effect): cross-compile's eval-when :compile-toplevel handling is
;;; best-effort, but a load-time registry works identically in the
;;; SBCL-hosted cross-compiler (source load populates it) and the self-hosted
;;; compiler (the compiled registration call runs at load). Inherited state
;;; (*specials*, *symbol-macros*, *global-specials*, ...) and config knobs
;;; must stay plain defvar: they are intentionally NOT reset at closure
;;; boundaries.
;;;
;;; Which scoped table belongs in the registry follows from how its entries are
;;; keyed:
;;;
;;; - KEY-VERIFIED tables: entries pinned to a slot key handed out by *LOCALS*
;;;   (long / small-int / numeric-array / native-double / native-single /
;;;   native-decimal / dotnet-typed). These travel together in the *CSTATE*
;;;   pack (see its section below), which is registered as ONE entry. A closure
;;;   body compiles with a fresh *LOCALS*, so an outer key can never be
;;;   re-derived there and every inherited entry is already dead: resetting
;;;   the whole pack preserves behavior and keeps one rule for the family.
;;;
;;; - NAME-KEYED declaration tables: the user's type declarations
;;;   (*fixnum-locals*, *double-float-locals*, *single-float-locals*,
;;;   *decimal-locals*). These stay plain defvar and are NOT reset: a declared
;;;   variable captured by the closure keeps its declared type inside the body,
;;;   which is what lets the body use the native path on it. An inner binding of
;;;   the same name is handled where that binding is compiled (the shadowed
;;;   entries are dropped there), not at the closure boundary.

(defvar *closure-fresh-state* '()
  "Alist of (variable-symbol . fresh-init-value) rebound (via progv) around
   every closure body compilation. Populated by define-compile-state.")

(defun %register-closure-fresh (name fresh-value)
  "Register NAME as closure-boundary-reset state with fresh value FRESH-VALUE.
   Idempotent: re-registration replaces the existing entry (re-load safe)."
  (let ((entry (assoc name *closure-fresh-state*)))
    (if entry
        (setf (cdr entry) fresh-value)
        (push (cons name fresh-value) *closure-fresh-state*)))
  name)

(defmacro define-compile-state (name init &key (fresh-init nil fresh-init-p) doc)
  "Declare a per-compilation special variable that automatically participates
   in the closure-boundary reset: compile-closure-body rebinds it to
   FRESH-INIT (default: INIT) for the dynamic extent of each closure body
   compilation. DOC becomes the variable's documentation string."
  `(progn
     (defvar ,name ,init ,@(when doc (list doc)))
     (%register-closure-fresh ',name ,(if fresh-init-p fresh-init init))))

(defun call-with-fresh-closure-state (overrides thunk)
  "Call THUNK with every variable in *closure-fresh-state* dynamically rebound
   (progv) to its registered fresh value; the closure-boundary reset.
   OVERRIDES is an alist ((variable . value) ...): an override wins over the
   registered fresh value, and an override for a variable NOT in the registry
   is bound as well (used for state whose fresh value is computed per call,
   e.g. *notinline-functions*). Bindings have dynamic extent, exactly like
   the special let* bindings this replaces; progv only runs on the compile
   path, so the per-binding cost is irrelevant."
  (let ((vars '())
        (vals '()))
    (dolist (entry *closure-fresh-state*)
      (let ((ov (assoc (car entry) overrides)))
        (push (car entry) vars)
        (push (if ov (cdr ov) (cdr entry)) vals)))
    (dolist (ov overrides)
      (unless (assoc (car ov) *closure-fresh-state*)
        (push (car ov) vars)
        (push (cdr ov) vals)))
    (progv vars vals
      (funcall thunk))))

;; The *LOCALS* alist and the other scope tables live in the *CSTATE* pack;
;; see its section below (readers CSTATE-LOCALS / CSTATE-BLOCK-TAGS /
;; CSTATE-GO-TAGS / CSTATE-BOXED-VARS / CSTATE-LOCAL-FUNCTIONS).

;; Counters for gen-local / gen-label. Under concurrent eval the plain incf can
;; race (lost update), but this is benign: the generated names are only ever
;; method-scoped locals/labels, and each concurrent compile emits an independent
;; method, so a duplicated V_n / L_n in two different methods never collides.
;; intern is idempotent and the package table is concurrency-safe, so no shared
;; state is corrupted. Left as plain incf to avoid per-gensym atomic overhead on
;; the hot compile path.
(defvar *var-counter* 0)

(defvar *label-counter* 0)


(defvar *specials* '()
  "List of symbols known to be special (both global and locally declared).")

(defvar *global-specials* '()
  "List of symbols that are GLOBALLY special (via defvar/defparameter/proclaim).
   Used to determine binding classification: only global specials force nested let bindings
   to be dynamic. Locally-declared specials (declare (special x)) only affect references.")

(defvar *macros* (make-hash-table :test #'eq :synchronized t)
  "Global macro table: symbol -> macro-expander-function.
   :synchronized so concurrent eval (dotcl:set-parallel-eval) cannot corrupt the
   table during a rehash-while-write; overhead is compile-time only.
   Keyed by symbol identity (not name string).
   Not reset by compile-toplevel (defmacro has global effect).")

(defvar *function-return-types* (make-hash-table :test #'eq :synchronized t)
  "symbol -> return-type.
   Populated by (declaim (ftype (function (...) ret) name...)), and by the
   return type inferred from a DEFUN body (see *INFERRED-RETURN-TYPE-NAMES*).
   Read through FUNCTION-RETURN-TYPE, never directly, by fixnum-typed-p and
   fixnum-leaf-range (FIXNUM), double-float-typed-p (DOUBLE-FLOAT, LONG-FLOAT),
   single-float-typed-p (SINGLE-FLOAT) and single-value-form-p (atomic types),
   which then enable the native int64 / r8 / r4 paths and skip the MV unwrap.")

(defvar *inferred-return-type-names* (make-hash-table :test #'eq :synchronized t)
  "symbol -> the compilation unit (see %RETURN-TYPE-UNIT) in which its
   *FUNCTION-RETURN-TYPES* entry was inferred from a DEFUN body, or NIL when
   it was inferred outside any unit. Absent for a declaimed entry.

   A declaimed ftype is the user's promise about every definition of the name
   and is honored everywhere. An inferred type describes one definition, and
   CLHS 3.2.2.3 lets a call assume which definition it reaches only when the
   caller is that definition itself (a recursive call) or is in the same file
   as it. So FUNCTION-RETURN-TYPE answers an inferred entry only for those two
   callers. Anything else (the REPL, a source LOAD, another file) compiles the
   call without the type, and so stays correct when the function is later
   redefined to return something else.")

(defvar *return-type-self* nil
  "The name of the DEFUN whose body is being compiled, for the recursive-call
   case of FUNCTION-RETURN-TYPE.")

(defun %return-type-unit ()
  "The compilation unit an inferred return type is recorded against and
   checked against. :CORE while cross-compiling the core (one closed unit).
   Under COMPILE-FILE, the (truename . load-truename) pair: every COMPILE-FILE
   binds a fresh truename object, and a source LOAD nested inside one (an
   EVAL-WHEN at compile time) binds a fresh load truename, so the functions it
   defines are not taken to be in the file. NIL otherwise."
  (cond (*cross-compiling* :core)
        ((and *compile-file-mode* *compile-file-truename*)
         (cons *compile-file-truename* *load-truename*))
        (t nil)))

(defun %in-return-type-unit-p (unit)
  "Is UNIT, as recorded by %RETURN-TYPE-UNIT, the unit being compiled now?"
  (cond ((null unit) nil)
        ((eq unit :core) (and *cross-compiling* t))
        (t (and (not *cross-compiling*)
                *compile-file-mode*
                (eq (car unit) *compile-file-truename*)
                (eq (cdr unit) *load-truename*)))))

(defvar *symbol-macros* '()
  "Alist of (symbol . expansion) for symbol-macrolet. Dynamically scoped.")

;;; ------------------------------------------------------------
;;; xref (who-calls) collection
;;;
;;; While a named function's body compiles, every named call site and #'name
;;; reference is recorded as CALLER->CALLEE. compile-defun binds the two specials,
;;; collects the edges, and appends a load-time (dotcl:%xref-note ...) call to
;;; the compiled output, so the table rebuilds on fasl load: no fasl format
;;; change. Plain specials, NOT *CSTATE* slots: a nested lambda's compile resets
;;; *CSTATE* at the closure boundary, but its call sites must still attribute to
;;; the enclosing defun, which is exactly what dynamic binding gives for free.
;;; Cross-compile is excluded: stdlib edges would bloat cil-out.sil and startup.

(defvar *xref-caller* nil
  "Function name (symbol or (setf sym)) whose body is being compiled, for
   who-calls edge attribution. NIL = not inside a named function (no recording).")

(defvar *xref-edges* nil
  "Callee names recorded for the current *XREF-CALLER* (reverse order, deduped).")

(defun xref-record-call (name)
  "Record *XREF-CALLER* -> NAME if xref collection is active and NAME is a
   global function name (interned symbol or (setf sym)). Local flet/labels
   calls are skipped; they would alias a same-named global."
  (when (and *xref-caller* (not *cross-compiling*))
    (when (if (symbolp name)
              (and (symbol-package name)
                   (not (local-function-entry name)))
              (and (consp name) (eq (car name) 'setf) (symbolp (cadr name))
                   (not (local-function-entry name))))
      (pushnew name *xref-edges* :test #'equal))))

;;; xref-note-instrs (the load-time registration emitter) is defined after the
;;; *in-mv-context* defvar below: it rebinds that special, so it must compile
;;; after the defvar is seen or the host would bind it lexically.

(defvar *notinline-functions* '()
  "List of function-name symbols currently declared NOTINLINE in the lexical
   scope (via (declare (notinline f ...))). Per CLHS 3.2.2.1.1, a NOTINLINE
   declaration of a function name suppresses its compiler macro for calls in the
   declaration's scope. Bound (extended) by body-compiling forms that strip
   declares. Dynamically scoped so nested bodies see enclosing declarations.")

(defun extract-notinline (body)
  "Collect function names from (declare (notinline f ...)) forms at the head of
   BODY. Returns a list of symbols (possibly empty)."
  (let ((result '())
        (rest body))
    (loop while (and rest (consp (car rest)) (eq (caar rest) 'declare))
          do (dolist (decl (cdar rest))
               (when (and (consp decl) (eq (car decl) 'notinline))
                 (dolist (f (cdr decl))
                   (when (symbolp f) (push f result)))))
             (pop rest))
    result))

(defun extract-inline (body)
  "Collect function names from (declare (inline f ...)) forms at the head of
   BODY. Returns a list of symbols (possibly empty). The FLET/LABELS counterpart
   of EXTRACT-NOTINLINE: the declaration sits in the binding form's body, which
   is where a local function's INLINE request is written."
  (let ((result '())
        (rest body))
    (loop while (and rest (consp (car rest)) (eq (caar rest) 'declare))
          do (dolist (decl (cdar rest))
               (when (and (consp decl) (eq (car decl) 'inline))
                 (dolist (f (cdr decl))
                   (when (symbolp f) (push f result)))))
             (pop rest))
    result))

(defvar *inline-defs* (make-hash-table :test #'eq :synchronized t)
  "Function-name symbol -> (LAMBDA-LIST . BODY) for functions whose DEFUN was
   compiled while the name was proclaimed INLINE. Filled by COMPILE-DEFUN,
   consumed by MAYBE-EXPAND-INLINE at call sites.

   CLHS 3.2.2.1.3 puts the proclamation BEFORE the defun for exactly this
   reason: the compiler has to be told to keep the definition around before it
   sees it. A later (declaim (inline f)) does not reach back and make an
   already-compiled f inlinable, which matches every other implementation.")

(defvar *inlining-stack* '()
  "Function names whose bodies are currently being substituted, innermost first.
   Stops a recursive function from inlining into its own expansion forever, and
   stops a mutually recursive pair from doing the same. Dynamically scoped.")

(defvar *inline-body-size-limit* 80
  "Maximum cons count of a recorded body that will be substituted at a call
   site. INLINE is a request, not an order (CLHS 3.2.2.1.3), and expanding a
   large body at every call site trades a call for a large multiple of the code.
   Deliberately generous: the functions people declaim inline are small.")

(defvar *global-symbol-macros* (make-hash-table :test #'eq :synchronized t)
  "Hash table of global symbol macros defined by DEFINE-SYMBOL-MACRO.")

(defvar *symbol-macro-shadow-marker* (make-symbol "LEXICAL-VARIABLE")
  "In the symbol-macro table of an environment object (keyed by name), the entry
   (MARKER . VAR) says VAR is a lexical variable there that shadows a symbol
   macro of the same name, global or from an outer SYMBOL-MACROLET, so
   MACROEXPAND-1 of VAR in that environment does not expand it. The runtime's
   MACROEXPAND-1 / MACROEXPAND read this variable to recognise the entry.")

(defvar *ltv-counter* 0
  "Counter for load-time-value slot IDs. Incremented for each load-time-value form.")

(defvar *ltv-hoisted* nil
  "While COMPILE-TOPLEVEL compiles a form for a fasl: a cell whose car collects
   (%SET-LTV-SLOT-IN module id form) for the LOAD-TIME-VALUE forms in it, so
   their forms run when the fasl is loaded, before the top level form, instead
   of when the code first reaches them. NIL everywhere else.")

(defvar *current-module-id* nil
  "Set by compile-file to the current FASL module's unique name.
   Used to namespace LTV slot IDs per module, preventing cross-run collisions.")

(defvar *at-toplevel* nil
  "T when compiling a form at top level (per CLHS 3.2.3.1).
   Only progn, eval-when, locally, macrolet, and symbol-macrolet preserve
   top-level-ness for their body forms.  All other forms set this to NIL.")

(defvar *toplevel-segments-p* nil
  "T while COMPILE-TOPLEVEL-EVAL compiles a form for EVAL. A top-level progn too
   long for one method is then cut at (:TOPLEVEL-BOUNDARY) markers, which the
   loader runs as separate methods in sequence. Only EVAL's caller splits the
   instruction list there, so every other entry point binds this to NIL.")

(defvar *compile-was-toplevel* nil
  "Captures *at-toplevel* at the start of compile-form, before it is reset to NIL.
   Handlers in *compile-form-handlers* that need to propagate top-level-ness
   (progn, locally, macrolet, symbol-macrolet, eval-when per CLHS 3.2.3.1)
   bind *at-toplevel* to this value.")

(defvar *compile-form-handlers* (make-hash-table :test #'eq :size 400)
  "Hash table: operator symbol -> (lambda (expr) ...) handler.
   Populated at the bottom of cil-forms.lisp after all compile-* helpers are defined.
   Provides O(1) dispatch for ~250+ common operators in compile-form.")

(defvar *macroexpand-cache* nil
  "Hash table (form -> ((scope . expansion) ...)) for memoizing macro expansions
   within one top-level compile.  Keyed by eq (cons-cell identity) on the form so
   that the same source form object is never expanded twice even when it appears
   in both the analysis pass and the code-gen pass.  NIL outside a compile-toplevel
   call, which disables caching (eval-time macro calls are not cached).")

(defvar *macrolet-shadowed* '()
  "Operator names bound by an enclosing MACROLET at the current lexical position.

   COMPILE-FORM dispatches on *COMPILE-FORM-HANDLERS* before it macroexpands, so
   a name that is a CL macro but is lowered here as a handler (WHEN, UNLESS, AND,
   OR, ...) would otherwise ignore a MACROLET binding for it and compile the
   built-in; silently, and differently from the interpreter, which consults its
   lexical macros first (CLHS 3.1.2.1.2.2 requires the MACROLET to win).

   This list is what tells the dispatcher to step aside.  It is bound where
   COMPILE-MACROLET registers the expanders, so it cannot drift out of sync with
   them.  True special operators are never affected: MACROLET cannot shadow those,
   and SPECIAL-OPERATOR-NAME-P excludes them.")

(defvar *lexical-operators* '()
  "Alist of the lexical operator bindings that decide what a macro call means at
   the current position, innermost first: (NAME . EXPANDER) for a MACROLET
   binding, (NAME) for an FLET / LABELS binding that hides a macro of the same
   name. This is the CAR of the &ENVIRONMENT object (%LEXICAL-MACRO-ENVIRONMENT),
   so MACROEXPAND-1 with that environment sees a local function hide a global
   macro (CLHS 3.1.2.1.2.2) and a MACROLET binding win over a global macro.

   An FLET name is only entered when it hides a macro (%FLET-MACRO-SHADOWS): an
   entry for any other name would change no expansion, and keeping the list
   empty keeps the environment NIL in the common case.")

(defun %flet-macro-shadows (fn-defs)
  "The names among the FLET / LABELS definitions FN-DEFS that are macros or
   have a compiler macro at the current position, and so are hidden by the
   local function inside its scope. Usually NIL."
  (let ((out '()))
    (dolist (d fn-defs (nreverse out))
      (let ((name (and (consp d) (car d))))
        (when (and name (symbolp name)
                   (let ((e (assoc name *lexical-operators*)))
                     (if e
                         (cdr e)
                         (or (find-macro-expander name)
                             ;; A local function also hides a global compiler
                             ;; macro, which COMPILER-MACRO-FUNCTION with this
                             ;; environment has to see.
                             (compiler-macro-function name)))))
          (push name out))))))

(defun %lexical-function-p (name)
  "True when an FLET / LABELS binding in *LEXICAL-OPERATORS* hides the macro
   NAME at the current position."
  (let ((e (and *lexical-operators* (assoc name *lexical-operators*))))
    (and e (null (cdr e)))))

(defun %lexical-macro-p (name)
  "True when the innermost lexical operator binding of NAME is a MACROLET, so
   a local function of that name from an outer FLET / LABELS is hidden."
  (let ((e (and *lexical-operators* (assoc name *lexical-operators*))))
    (and e (cdr e) t)))

(defun %mv-call-lambda-bind (form)
  "When FORM is (MULTIPLE-VALUE-CALL #'(LAMBDA (&OPTIONAL v... [&REST r]) . body)
   value-form), the shape the standard MULTIPLE-VALUE-BIND expansion has, return
   the equivalent binding through the per-thread value snapshot:
     (LET* ((#:p (%MV-CAPTURE value-form)) (v (%MV-NTH 0)) ...) . body)
   Otherwise NIL.

   MACROEXPAND-1 hands code walkers MULTIPLE-VALUE-BIND in that portable form,
   and a walker that expands a body (iterate, cl-cont) gives it back to be
   compiled. Compiled as a real call it would build a closure and a list of the
   values; this keeps it as cheap as the MULTIPLE-VALUE-BIND written directly.
   The analysis walks take the same rewrite, so both passes see one form.

   Only plain variables: no defaults or supplied-p parameters. An &REST variable
   must be declared IGNORE (or IGNORABLE), since nothing binds it here."
  (let ((fn (cadr form))
        (args (cddr form)))
    (when (and (consp args) (null (cdr args)) (consp fn))
      (let ((lam (cond ((and (eq (car fn) 'function) (consp (cdr fn)) (null (cddr fn))
                             (consp (cadr fn)) (eq (car (cadr fn)) 'lambda))
                        (cadr fn))
                       ((eq (car fn) 'lambda) fn))))
        (when (and lam (consp (cdr lam)) (consp (cadr lam))
                   (eq (car (cadr lam)) '&optional))
          (let ((vars '()) (rest nil) (ll (cdr (cadr lam))))
            (flet ((plain-var (v)
                     (and v (symbolp v) (not (eq v t)) (not (keywordp v))
                          (not (member v lambda-list-keywords))
                          (not (constantp v)))))
              (loop
                (cond ((null ll) (return))
                      ((not (consp ll)) (return-from %mv-call-lambda-bind nil))
                      ((eq (car ll) '&rest)
                       (unless (and (consp (cdr ll)) (plain-var (cadr ll)) (null (cddr ll)))
                         (return-from %mv-call-lambda-bind nil))
                       (setq rest (cadr ll))
                       (return))
                      (t
                       (let ((p (car ll)))
                         ;; v, (v) or (v nil)
                         (cond ((plain-var p) (push p vars))
                               ((and (consp p) (plain-var (car p))
                                     (or (null (cdr p))
                                         (and (consp (cdr p)) (null (cadr p))
                                              (null (cddr p)))))
                                (push (car p) vars))
                               (t (return-from %mv-call-lambda-bind nil))))
                       (setq ll (cdr ll))))))
            (let ((body (cddr lam)))
              (when rest
                ;; Drop REST from the leading IGNORE / IGNORABLE declarations; it
                ;; is not bound any more. Without such a declaration the body may
                ;; read it, so keep the call.
                (let ((found nil) (out '()))
                  (loop while (and (consp body) (consp (car body))
                                   (eq (car (car body)) 'declare))
                        do (let ((specs '()))
                             (dolist (spec (cdr (car body)))
                               (if (and (consp spec)
                                        (member (car spec) '(ignore ignorable))
                                        (member rest (cdr spec)))
                                   (let ((others (remove rest (cdr spec))))
                                     (setq found t)
                                     (when others (push (cons (car spec) others) specs)))
                                   (push spec specs)))
                             (when specs (push (cons 'declare (nreverse specs)) out))
                             (setq body (cdr body))))
                  (unless found (return-from %mv-call-lambda-bind nil))
                  (setq body (append (nreverse out) body))))
              (let ((p (gensym "MVP")))
                `(let* ((,p (%mv-capture ,(car args)))
                        ,@(let ((i -1))
                            (mapcar (lambda (v) (list v (list '%mv-nth (incf i))))
                                    (nreverse vars))))
                   (declare (ignorable ,p))
                   ,@body)))))))))

(defun special-operator-name-p (name)
  "True for the 25 CL special operators, which no lexical binding may shadow.
   Shared by the FLET/LABELS override check and the MACROLET one so the two
   cannot disagree about what is shadowable."
  (and (symbolp name)
       (member name '(quote function if progn let let* setq block return-from
                      tagbody go unwind-protect catch throw flet labels macrolet
                      symbol-macrolet the locally load-time-value eval-when
                      multiple-value-call multiple-value-prog1 progv))
       t))

(defun macrolet-shadowed-p (name)
  "True when NAME is bound by an enclosing MACROLET and so must expand as that
   macro rather than dispatch to its built-in handler."
  (and (symbolp name)
       (not (special-operator-name-p name))
       (member name *macrolet-shadowed*)
       t))

(defvar *macroexpand-scope* '()
  "Stack of enclosing MACROLET scope markers (the source MACRO-DEFS cons of each
   active macrolet) for the current lexical position.  A single source form
   spliced, by a macro, into both a shadowing macrolet scope and an outer scope
   must expand differently in each, but a plain form-keyed cache would reuse the
   first expansion for both (with-cached-result shadowed in only
   one of two ,@body splices).  Keying the cache by (form, scope) keeps them
   apart.  Both the free-variable analysis walk and the code-gen walk push the
   SAME source cons when entering a given macrolet, so the scope is identical
   between the two passes; preserving their shared-expansion contract (one
   expansion object reused across passes, important for gensym identity).")

(defvar *expansion-uncacheable* nil
  "Set to T by LOOKUP-SYMBOL-MACRO while a macro expander runs, when the
   expansion used a global symbol macro in a context that cannot tell whether
   an enclosing lexical binding shadows it (the analysis walks: see
   *SM-WALKER-CONTEXT*). CACHED-MACROEXPAND then does not store the result, so
   the code-generation pass expands the form again with the real *LOCALS*.")

(defvar *sm-walker-context* nil
  "T while an analysis walk (free-variable / mutation) runs a macro expander.
   Those walks do not bind *LOCALS* for the bindings inside the form they
   walk, and walk nested lambdas once per form independent of their
   enclosing bindings, so a global symbol macro looked up there may in fact be
   shadowed by a LET in the walked code.")

(defvar *sm-walker-bound* :none
  "The free-variable walk's bound-name list (BND) at the form being expanded,
   or :NONE outside that walk. Names in it are lexical variables bound inside
   the walked form, which shadow a symbol macro of the same name.")

(defvar *sm-interp-env* :none
  "The tree-walk interpreter's lexical ENV alist while it macroexpands a form,
   or :NONE. LOOKUP-SYMBOL-MACRO resolves names against it there.")

;; Proclaimed special here for LOOKUP-SYMBOL-MACRO; the value is given where the
;; interpreter defines it (cil-forms.lisp).
(defvar *%mini-symbol-macro-marker*)

(defvar *sm-global-hits* nil
  "Global symbol macros LOOKUP-SYMBOL-MACRO resolved inside an analysis walk.
   The walk treats each as a possible reference to a variable of that name,
   since the expansion it walks may be the wrong one.")

(defun %macroexpand-scope= (a b)
  "Identity comparison of two macroexpand scopes (lists of eq markers)."
  (loop (cond ((eq a b) (return t))
              ((or (null a) (null b)) (return nil))
              ((eq (car a) (car b)) (setf a (cdr a) b (cdr b)))
              (t (return nil)))))

;; Phase profiler hook. Resolved at load time in the TARGET (the SBCL
;; cross-compile host has no DOTCL package), same shape as
;; *known-type-name-p-fn*. NIL =&gt; WITH-PHASE is a plain PROGN.
(defvar *phase-time-fn*
  (ignore-errors
   (let ((sym (find-symbol "%PHASE-TIME" "DOTCL")))
     (and sym (fboundp sym) (symbol-function sym)))))

(defmacro with-phase (name &body body)
  "Charge BODY's elapsed time to phase NAME when DOTCL_PHASE_PROF=1 is on."
  `(if *phase-time-fn*
       (funcall *phase-time-fn* ,name (lambda () ,@body))
       (progn ,@body)))

(defun cached-macroexpand (form expander)
  "Expand FORM using EXPANDER, memoizing the result per (FORM, *macroexpand-scope*)
   in *macroexpand-cache*.  If the cache is NIL (outside a compile-toplevel) falls
   through to a plain funcall so that eval / macrolet restore paths are unaffected."
  (if *macroexpand-cache*
      (let* ((by-scope (gethash form *macroexpand-cache* '()))
             (cell (assoc *macroexpand-scope* by-scope :test #'%macroexpand-scope=)))
        (if cell
            (cdr cell)
            (let ((result nil) (uncacheable nil))
              (let ((*expansion-uncacheable* nil))
                (setf result (with-phase "macroexpand" (funcall expander form))
                      uncacheable *expansion-uncacheable*))
              (if uncacheable
                  ;; An enclosing expansion that embeds this one is just as
                  ;; context-dependent, so it must not be cached either.
                  (setf *expansion-uncacheable* t)
                  (setf (gethash form *macroexpand-cache*)
                        (cons (cons *macroexpand-scope* result) by-scope)))
              result)))
      (funcall expander form)))

;;; A macro whose expander signals an ERROR while COMPILE-FILE compiles a form
;;; does not stop the compilation. As SBCL does, the error is reported -- as a
;;; WARNING, so COMPILE-FILE answers T for both WARNINGS-P and FAILURE-P -- and the
;;; form is compiled into one that signals an error saying so when it is
;;; evaluated; the rest of the file is still compiled. Only while COMPILE-FILE is
;;; compiling (*COMPILE-FILE-MODE*): code it evaluates at compile time (EVAL-WHEN
;;; :COMPILE-TOPLEVEL) and EVAL / COMPILE at the REPL see the expander's own
;;; error, as in SBCL.
(defvar *macroexpansion-failures* 0
  "How many macro expansions COMPILE-FILE has reported as failed. COMPILE-FILE
   compares it around the compilation of a form evaluated at compile time too, and
   then does not run the stand-in there: the error is reported once, as SBCL does,
   instead of again as an error out of COMPILE-FILE.")

(defvar *evaluating-at-compile-time* nil
  "True while COMPILE-FILE compiles a form only to evaluate it at compile time
   (EVAL-WHEN :COMPILE-TOPLEVEL without :LOAD-TOPLEVEL). There a failed expansion
   is the expander's own error, as when EVAL meets it.")

(defun %macroexpansion-failed (form condition)
  "Report CONDITION, signalled by the expander of FORM, and return the form that
   stands in for FORM in the compiled file."
  (let ((form-text (or (ignore-errors
                        (let ((*print-length* 10) (*print-level* 4))
                          (prin1-to-string form)))
                       "#<form>"))
        (message (or (ignore-errors (princ-to-string condition))
                     "#<error>")))
    ;; The wording is SBCL's, so a log reads the same under both.
    (incf *macroexpansion-failures*)
    (warn "caught ERROR:~%  (during macroexpansion of ~A)~%  ~A" form-text message)
    `(error "Execution of a form compiled with errors.~%Form:~%  ~A~%Compile-time error:~%  (during macroexpansion of ~A)~%~A"
            ,form-text ,form-text ,message)))

(defun %macroexpand-for-compile (form expander)
  "CACHED-MACROEXPAND, except that inside COMPILE-FILE an ERROR from the expander
   becomes a report and a stand-in form (see %MACROEXPANSION-FAILED)."
  (if (and *compile-file-mode* (not *evaluating-at-compile-time*))
      (handler-case (cached-macroexpand form expander)
        (error (c) (%macroexpansion-failed form c)))
      (cached-macroexpand form expander)))

;;; TCO (Tail Call Optimization) state. The TCO-scope values live in the
;;; *CSTATE* pack (readers below its section); the two flags that are rebound
;;; per expression stay dynamic specials here.
(define-compile-state *in-tail-position* nil
  :fresh-init t
  :doc "T when the currently-being-compiled expression is in tail position
   within the TCO scope. Reset to NIL at function entry; compile-progn
   and compile-if set it appropriately. Closure-boundary fresh value is T:
   a closure body's last form is in tail position (MV propagation).")
;; Multiple values without an MvReturn when the caller says what it will do
;; with them: the call passes a value mode (LispFunction.InvokeNM), and a body
;; that takes it on entry (MultipleValues.TakeMode) returns its tail VALUES in
;; the cheaper form the mode names.
(defvar *mv-mode-elision* t
  "When NIL, no call passes a value mode and no body takes one.")
(define-compile-state *mv-mode-local* nil
  :doc "The value mode for the code in a tail position: the Int32 local holding
   the mode the current body's caller passed, an integer mode where the values
   go straight to a consumer in this body (%COMPILE-FOR-CAPTURE), or NIL. Bound
   by the function body compilers; reset at every closure boundary and for
   inline LABELS bodies, so it is only ever seen by code whose value is this
   body's own.")
(define-compile-state *mv-expect-args* nil
  :doc "(ARGS . MODE): the call whose argument list is EQ to ARGS has its result
   consumed at once, as MODE (:primary or :capture) says.")

(define-compile-state *mv-exit-cleanup* nil
  :doc "Non-NIL inside the protected form of an UNWIND-PROTECT: a RETURN-FROM
   out of it runs cleanup forms, which can publish values of their own, so the
   values it returns must travel in an MvReturn.")

(defvar *mv-tail-functions* (make-hash-table :test #'eq :synchronized t)
  "Global function names (symbols) whose compiled body takes a value mode, i.e.
   can return multiple values from its tail. Filled when such a body is
   compiled. A global call whose primary value only is used passes the mode
   only to these: it is the commonest call there is, and for any other callee
   the mode would be one more argument for nothing. (A FUNCALL or a call of a
   local function has no name to look up and always passes it.)")

(defvar *key-shared-takes-mode* nil
  "True while compiling the typed entries of a &key / &optional function whose
   shared body takes a value mode on entry: a %CALL-KEY-SHARED in such an
   entry's tail hands the entry's own mode on (MultipleValues.PassMode). Only
   then: a mode passed to a body that does not take it would be left for
   whatever body runs next.")

(defun %mv-expect-for (outer inner)
  "*MV-EXPECT-ARGS* for compiling INNER as all there is to OUTER (OUTER's macro
   expansion, or the form a THE wraps): the expectation made for OUTER carries
   over to INNER."
  (let ((ex *mv-expect-args*))
    (if (and ex (consp outer) (eq (car ex) (cdr outer)))
        (and (consp inner) (cons (cdr inner) (cdr ex)))
        ex)))

(defun %mv-tail-function-p (name)
  (and (symbolp name) (gethash name *mv-tail-functions*)))

(defun %mv-mode-local ()
  "The value mode of the code being compiled in a tail position, or NIL: the
   Int32 local of a body that took its caller's mode, or a constant mode where
   the values go straight to a consumer in the same body (see
   %COMPILE-FOR-CAPTURE)."
  *mv-mode-local*)

(defun %mv-mode-push ()
  "The instructions that push %MV-MODE-LOCAL's mode."
  (let ((m *mv-mode-local*))
    (if (integerp m) `((:ldc-i4 ,m)) `((:ldloc ,m)))))

(define-compile-state *in-finally-block* nil
  :doc "T while compiling unwind-protect cleanup forms. return-from / go
   inside a finally region cannot use `leave` to a label outside it; they
   take the exception (throw) path instead. Reset at closure boundaries: a
   closure body is a separate CLR method, never inside the outer finally
   region, so its own block/tagbody exits may use the local leave path.")
(define-compile-state *in-try-block* nil
  :doc "Non-NIL when the currently-being-compiled expression is inside a try/
   finally region whose finally must run on exit (e.g. special-variable
   LET, UNWIND-PROTECT). TCO branches (`br` to the loop label) cannot
   legally cross such a region; IL requires `leave`, which this compiler
   does not yet emit for tail-recursion. Suppressing TCO via this flag
   avoids invalid IL while keeping MV propagation intact. Reset at closure
   boundaries: a closure body is a separate CLR method, never inside the
   outer try region, so its own TCO branches need no suppression.")

(defvar *fixnum-locals* '()
  "List of symbol-name strings for lexical locals declared (fixnum X) /
   (type fixnum X) / (type (integer LO HI) X) with bounded fixnum range.
   compile-as-long treats references to these as int64 unbox sites, and
   fixnum-typed-p reports them as fixnum. Values are boxed LispObject at
   the slot; unboxing happens inline in compile-as-long (castclass Fixnum
   + get_Value). Caller-side guarantee: the user's declaration contract.")

(defvar *fixnum-wrap-licensed* nil
  "When true, FIXNUM-TYPED-P also accepts +, -, *, 1+, 1- of fixnum-typed
   operands that no range proof covers. COMPILE-AS-LONG computes those with the
   raw, wrapping int64 op, so the value is right only modulo 2^64. That is
   enough exactly where the result goes into a place declared FIXNUM (a
   declared parameter of a native self-call, for instance): a result outside
   int64 violates the declaration, and a wrapped one is the writer's problem.
   Everywhere else the default NIL applies, and FIXNUM-TYPED-P means the int64
   COMPILE-AS-LONG leaves is the exact value. Range proofs (EXPR-INT-RANGE)
   and the operands of MOD, REM and a right shift, which look at the whole
   value rather than its low bits, always ask with it NIL.")

(defvar *character-locals* '()
  "List of symbol-name strings for lexical locals declared (character X) /
   (type character X). Only the CHAR= family reads it: comparing two
   characters is comparing their codes, so when both operands are a literal
   character or one of these, the comparison lowers to CHAR-CODE-OF-L plus an
   integer compare instead of building a boolean object and testing it. The
   slot still holds the character; nothing else about the variable changes.")

;;; --- *CSTATE*: the key-verified table pack (one special, seven tables) ----
;;; The per-compilation tables whose entries are pinned to slot keys handed
;;; out by *LOCALS* travel together as one simple-vector held by the special
;;; *CSTATE*. Updates are functional, CSTATE-WITH copies the vector, and
;;; are installed either by rebinding *CSTATE* in a LET/LET* (scoped
;;; extension, exactly like the per-table specials this replaces) or by SETF
;;; of *CSTATE* inside such a frame (the sequential-LET* idiom). The empty
;;; pack is shared and never mutated, so a reset is a binding to
;;; +CSTATE-EMPTY+, and the closure-boundary reset is ONE registry entry no
;;; matter how many tables the pack grows.

(defparameter +cs-long-locals+ 0)
(defparameter +cs-small-int-locals+ 1)
(defparameter +cs-numeric-array-locals+ 2)
(defparameter +cs-native-double-locals+ 3)
(defparameter +cs-native-single-locals+ 4)
(defparameter +cs-native-decimal-locals+ 5)
(defparameter +cs-dotnet-typed-locals+ 6)
(defparameter +cs-tco-self-name+ 7)
(defparameter +cs-tco-loop-label+ 8)
(defparameter +cs-tco-param-entries+ 9)
(defparameter +cs-self-fn-local+ 10)
(defparameter +cs-tco-self-symbol+ 11)
(defparameter +cs-tco-local-fn-key+ 12)
(defparameter +cs-tco-leave-instrs+ 13)
(defparameter +cs-tco-in-try-catch+ 14)
(defparameter +cs-labels-mutual-tco+ 15)
(defparameter +cs-labels-direct-speculation+ 16)
(defparameter +cs-native-self-name+ 17)
(defparameter +cs-locals+ 18)
(defparameter +cs-block-tags+ 19)
(defparameter +cs-go-tags+ 20)
(defparameter +cs-boxed-vars+ 21)
(defparameter +cs-local-functions+ 22)
(defparameter +cs-no-safepoint+ 23)
(defparameter +cs-array-backing-locals+ 24)
(defparameter +cs-native-char-locals+ 25)
(defparameter +cs-struct-backing-locals+ 26)
(defparameter +cs-tco-key-shared+ 27)
(defparameter +cstate-empty+
  (vector '() '() '() '() '() '() '() nil nil nil nil nil nil nil nil nil nil nil
          '() '() '() '() '() nil '() '() '() nil))

(defvar *cstate* +cstate-empty+
  "The key-verified table pack; see the section comment above. Participates
   in the closure-boundary reset as a single registry entry.")
(%register-closure-fresh '*cstate* +cstate-empty+)

(defmacro cstate-with (cstate &rest index-value-pairs)
  "A copy of CSTATE with each +CS-...+ INDEX set to the following VALUE.

   A macro rather than a function taking &REST: every compiled form that binds
   *CSTATE* goes through here, and the &REST list was a cons per pair on each of
   them (4% of everything a COMPILE allocates). The pairs are written out at all
   53 call sites, so the expansion is a straight-line sequence of SETFs.

   When every VALUE is EQ to what CSTATE already holds at its INDEX, CSTATE
   itself is returned and nothing is copied. Packs are never modified after they
   are made, so sharing one is safe. Most sites extend a table with what the
   form at hand declares -- (APPEND NEW (CSTATE-...)) -- and most forms declare
   nothing, which leaves the same list: a function body made four copies of the
   whole pack for a lambda with no declarations at all."
  (let ((v (gensym "CS"))
        (old (gensym "OLD"))
        (temps (loop repeat (floor (length index-value-pairs) 2)
                     collect (gensym "VAL"))))
    `(let* ((,old ,cstate)
            ,@(loop for (nil value) on index-value-pairs by #'cddr
                    for tmp in temps
                    collect `(,tmp ,value)))
       (if (and ,@(loop for (index) on index-value-pairs by #'cddr
                        for tmp in temps
                        collect `(eq ,tmp (svref ,old ,index))))
           ,old
           (let ((,v (copy-seq ,old)))
             ,@(loop for (index) on index-value-pairs by #'cddr
                     for tmp in temps
                     collect `(setf (svref ,v ,index) ,tmp))
             ,v)))))

(defun cstate-fresh-function-body ()
  "The pack a non-closure function body compiles under: everything fresh;
   the body is a separate CLR method, so it must not inherit the enclosing
   body's compile context; EXCEPT the tco-self-symbol / tco-local-fn-key
   slots, which are parameters the CALLER hands in through its *CSTATE*
   binding (compile-defun passes the defun symbol, the labels path passes
   the self-TCO key) and the self-call fast path in the body reads.
   The closure boundary differs on exactly this point: it resets to the
   plain +CSTATE-EMPTY+, clearing the handoff (a closure is never the
   self-call target of its enclosing function)."
  (cstate-with +cstate-empty+
               +cs-tco-self-symbol+ (cstate-tco-self-symbol)
               +cs-tco-local-fn-key+ (cstate-tco-local-fn-key)))

(defun cstate-locals ()
  "Alist of (symbol . local-key). local-key is a keyword like :v1."
  (svref *cstate* +cs-locals+))

(defun cstate-block-tags ()
  "Alist of (block-name . (tag-key . result-key)).
   tag-key is a keyword for the tag object, result-key for the result local."
  (svref *cstate* +cs-block-tags+))

(defun cstate-go-tags ()
  "Alist of (tag-symbol . (tagbody-id-key . integer-label))."
  (svref *cstate* +cs-go-tags+))

(defun cstate-boxed-vars ()
  "Set of variable names (symbols) that need boxing (mutated + captured)."
  (svref *cstate* +cs-boxed-vars+))

(defun cstate-local-functions ()
  "Alist of (name-string local-key boxed-p).
   For flet: local-key is a keyword for a LispObject local holding the function.
   For labels: boxed-p is T, local-key is a keyword for a LispObject[] box."
  (svref *cstate* +cs-local-functions+))

(defun cstate-tco-self-name ()
  "Mangled name of function currently being compiled for self-TCO. NIL = disabled."
  (svref *cstate* +cs-tco-self-name+))

(defun cstate-tco-loop-label ()
  "Label name to branch back to for TCO self-call."
  (svref *cstate* +cs-tco-loop-label+))

(defun cstate-tco-param-entries ()
  "List of (key . boxed-p) in param order, for rewriting TCO self-call args.
   key = gen-local string; boxed-p = T if the local is a LispObject[] box."
  (svref *cstate* +cs-tco-param-entries+))

(defun cstate-self-fn-local ()
  "When set, name of the local variable holding the current function's own
   LispFunction object. compile-named-call uses this instead of doing a full
   load-sym-pkg / GetFunctionBySymbol sequence for non-tail self-calls.
   Must be cleared at closure boundaries; it refers to a local declared in
   the OUTER method."
  (svref *cstate* +cs-self-fn-local+))

(defun cstate-tco-self-symbol ()
  "Original symbol of function currently being compiled for self-TCO.
   NIL = fall back to mangled-name string comparison against the tco-self-name
   slot. Used by compile-named-call for symbol-identity self-call matching
   (avoids cross-package false matches like uiop/os:getenv vs dotcl:getenv).
   Must be reset at closure boundaries and rebound per labels function: a stale
   outer defun's symbol makes a tail call to that OUTER defun inside an inner
   named function match as a SELF call and branch to the inner TCO loop."
  (svref *cstate* +cs-tco-self-symbol+))

(defun cstate-tco-local-fn-key ()
  "Box key (gen-local string) of the labels function currently being compiled
   for self-TCO, or NIL if compiling a defun. Allows compile-named-call to
   permit self-TCO despite the function name appearing in *local-functions*."
  (svref *cstate* +cs-tco-local-fn-key+))

(defun cstate-tco-leave-instrs ()
  "List of CIL instructions to emit BEFORE the TCO branch (br or leave).
   Used when the TCO site is inside a try block that requires explicit cleanup
   before branching back (e.g. handler-case must call HandlerClusterStack.PopCluster
   before leaving the catch-protected region). NIL for ordinary TCO."
  (svref *cstate* +cs-tco-leave-instrs+))

(defun cstate-no-safepoint ()
  "T while compiling a body declared (optimize (safety 0)): loop back-edge
   interrupt polls (ConditionSystem.PollInterrupt) are omitted, so tight
   call-free loops pay nothing per iteration; at the cost of that loop being
   unstoppable by Ctrl-C."
  (svref *cstate* +cs-no-safepoint+))

(defvar *optimize-debug* 1
  "The DEBUG quality in force for the code being compiled, from
   (declaim (optimize (debug N))). 0 means a compiled function records no
   debugger frame: the push is per call and cannot be made cheaper (measured;
   see the decision record), so the only way not to pay it is not to ask for it.
   Anything else keeps today's behaviour.")

(defun body-declared-debug (body)
  "The DEBUG quality BODY's leading declarations ask for, or NIL when they ask
   for nothing. Docstrings may precede or interleave with the declarations."
  (let ((result nil))
    (dolist (form body result)
      (unless (or (stringp form)
                  (and (consp form) (eq (car form) 'declare)))
        (return result))
      (when (consp form)
        (dolist (d (cdr form))
          (when (and (consp d) (eq (car d) 'optimize))
            (dolist (q (cdr d))
              (cond ((and (consp q) (eq (car q) 'debug) (integerp (cadr q)))
                     (setq result (cadr q)))
                    ((eq q 'debug) (setq result 3))))))))))

(defun debug-frames-off-p (body)
  "T when the function compiled from BODY should record no debugger frame.
   The body's own declaration decides it; with no declaration there, the value
   DECLAIM put in force decides (CLHS 3.3.4). Reading it the other way -- the
   global turning frames off and the body only able to agree -- left a single
   function unable to declare (debug 3) and get its frame back, which is the one
   thing that declaration is for."
  (let ((declared (body-declared-debug body)))
    (if declared
        (eql declared 0)
        (eql *optimize-debug* 0))))

(defvar *optimize-safety* 1
  "The SAFETY quality in force for the code being compiled, from
   (declaim (optimize (safety N))). 0 means the loops in the functions compiled
   after it carry no back-edge interrupt poll. A function body that declares
   SAFETY itself overrides this, in either direction.")

(defun body-declared-safety (body)
  "The SAFETY quality BODY's leading declarations ask for, or NIL when they ask
   for nothing. Docstrings may precede or interleave with the declarations."
  (let ((result nil))
    (dolist (form body result)
      (unless (or (stringp form)
                  (and (consp form) (eq (car form) 'declare)))
        (return result))
      (when (consp form)
        (dolist (d (cdr form))
          (when (and (consp d) (eq (car d) 'optimize))
            (dolist (q (cdr d))
              (cond ((and (consp q) (eq (car q) 'safety) (integerp (cadr q)))
                     (setq result (cadr q)))
                    ((eq q 'safety) (setq result 3))))))))))

(defun safepoints-off-p (body)
  "T when the function compiled from BODY should omit the loop back-edge
   interrupt polls (ConditionSystem.PollInterrupt), so a tight call-free loop
   pays nothing per iteration -- at the cost of that loop being unstoppable by
   Ctrl-C. The body's own declaration decides it; with no declaration there, the
   value DECLAIM put in force decides (CLHS 3.3.4)."
  (let ((declared (body-declared-safety body)))
    (if declared
        (eql declared 0)
        (eql *optimize-safety* 0))))

(defun compiling-at-safety-0-p ()
  "T when the function being compiled is under (optimize (safety 0)), counting
   both its own declaration and the DECLAIM in force. The CSTATE slot consulted
   here is filled by SAFEPOINTS-OFF-P, which asks exactly this question -- the
   loop back-edge poll was just its first customer. Outside a function body
   (a top-level form) there is no CSTATE, so the DECLAIM decides alone."
  (if (and (boundp '*cstate*) *cstate*)
      (and (cstate-no-safepoint) t)
      (eql *optimize-safety* 0)))

(defun cstate-tco-in-try-catch ()
  "T when the current TCO site is inside a handler-case try/catch body (not
   a try/finally). In this case, TCO uses `leave` to exit the protected region
   instead of `br`. Distinct from *in-try-block* which signals try/finally
   (special-variable LET / unwind-protect) and always suppresses TCO."
  (svref *cstate* +cs-tco-in-try-catch+))

(defun cstate-labels-mutual-tco ()
  "Dispatch table for labels mutual TCO. Each entry:
   (name-str fn-index which-fn-key tcoloop-label shared-param-keys).
   NIL outside a mutual-TCO labels group. Reset in every
   compile-function-body-*/compile-closure-body so closures compiled
   within the group never emit br-to-outer-TCOLOOP."
  (svref *cstate* +cs-labels-mutual-tco+))

(defun cstate-labels-direct-speculation ()
  "During speculative direct+TCO compilation of a single self-recursive
   labels function (compile-labels-boxed), holds (name-str . box-key).
   compile-lambda routes the fn, whose SOLE free var is its own labels box,
   through the direct :make-function-direct path (which carries self-TCO)
   instead of a closure; compile-function-body-direct injects the box as a
   local-function so any self-reference NOT lowered to a TCO branch emits a
   detectable (:ldloc box-key) load. The caller then scans the generated body
   and only accepts it when provably self-contained, otherwise recompiles via
   the closure path. NIL outside speculation. Reset at closure boundaries."
  (svref *cstate* +cs-labels-direct-speculation+))

(defun cstate-native-self-name ()
  "Mangled name of the current function if it is native-eligible (all fixnum params
   + fixnum return, no captures, no specials). Enables native self-call path
   in compile-as-long and native TCO arg evaluation."
  (svref *cstate* +cs-native-self-name+))

(defun cstate-small-int-locals ()
  "Alist (SLOT-KEY . (LO . HI)) for lexical locals whose value is statically
   known to lie in the inclusive int64 range [LO,HI]. Two sources: bounded
   integer type declarations ((signed-byte N) / (unsigned-byte N) / bit /
   (integer c c)) and let-binding init range inference (compile-let). Like
   *fixnum-locals* the slot holds a boxed LispObject (a Fixnum, since the range
   fits int64), so compile-as-long unboxes inline. Unlike *fixnum-locals* the
   tracked range is TIGHT, which is what lets expr-int-range prove a product
   stays in int64 and emit native arithmetic; an overflowing product instead
   falls back to the promoting path and yields a bignum (CL-compliant). Mutated
   locals are excluded; a setf could move the value out of [LO,HI]."
  (svref *cstate* +cs-small-int-locals+))

(defvar *double-float-locals* '()
  "Like *fixnum-locals* but for double-float declarations. Enables native
   r8 arithmetic on (declare (double-float x)) locals and references.")

(defvar *single-float-locals* '()
  "Like *double-float-locals* but for single-float declarations. Enables native
   r4 arithmetic on (declare (single-float x)) locals and references.")

(defvar *decimal-locals* '()
  "Like *double-float-locals* but for decimal declarations. Enables
   native System.Decimal arithmetic on (declare (type decimal x)) locals: in such a
   scope (+ x y) compiles to decimal.op_Addition and PRESERVES SCALE (1.50m+2.25m=
   3.75m), where the undeclared path degrades a decimal to its rational value.")

(defun native-slot-p (sym slot-keys)
  "The slot key SYM's binding occupies when that slot is one of SLOT-KEYS (a
   native-representation table: *LONG-LOCALS* / *NATIVE-DOUBLE-LOCALS* /
   *NATIVE-SINGLE-LOCALS*), or NIL.

   Resolving SYM first and then asking about the resulting SLOT is what makes
   shadowing a non-issue: an inner binding of the same name gets its own key and
   simply is not in the table, while an outer native slot stays valid for as long
   as SYM still resolves to it. Captured variables are excluded; env capture
   stores an object, so their slot holds the box, not a raw value."
  (and (symbolp sym)
       slot-keys
       (not (boxed-var-p sym))
       (let ((key (lookup-local sym)))
         (and key (member key slot-keys) key))))

(defun shadows-var-p (sym k)
  "True when a binding of SYM hides an existing *LOCALS* entry keyed by K.
   Same rule as LOCAL-ENTRY resolves references with, identity, or a
   package-compatible name, so a same-named symbol from ANOTHER package does
   not hide it (CL scoping is by symbol identity)."
  (and (symbolp k)
       (or (eq k sym)
           (and (same-var-package-p k (symbol-package sym))
                (string= (var-name k) (var-name sym))))))

(defun remove-locals-shadowed-by (syms locals)
  "LOCALS minus every entry that a binding of some symbol in SYMS hides."
  (remove-if (lambda (entry)
               (some (lambda (s) (shadows-var-p s (car entry))) syms))
             locals))

(defun drop-shadowed-type-locals (bound-names entries)
  "ENTRIES (a name list, or an alist keyed by name) minus every entry whose
   variable name is in BOUND-NAMES; the names a new binding form introduces.

   A type declaration covers the binding it was written for and nothing else. An
   inner binding of the same name is a DIFFERENT variable, so carrying the outer
   entry into that scope miscompiles: (let ((n 1)) (declare (fixnum n))
   (let ((n 2.5d0)) (* n 2))) would unbox the inner DoubleFloat slot as a Fixnum.
   Every form that binds variables must filter the inherited type-locals through
   this before adding its own declarations."
  (remove-if (lambda (e)
               (member (if (consp e) (car e) e) bound-names :test #'string=))
             entries))

(defun type-locals-after-binding (name declared entries)
  "ENTRIES updated for one LET* binding of NAME, where DECLARED is the list of
   names this LET*'s body declares of that type.

   Both halves are required by CLHS 3.3.4: the scope of a binding type
   declaration for a LET* variable starts at that variable's binding, so it
   covers every LATER init form in the same LET* -- and a variable this LET*
   binds WITHOUT such a declaration is a different variable from the outer one of
   the same name, so the outer entry must stop applying there. The LET body gets
   this rule applied once, in COMPILE-LET-BODY-INSTRS; a LET* has to apply it one
   binding at a time or its own later inits do not see it."
  (if (member name declared :test #'string=)
      (cons name entries)
      (drop-shadowed-type-locals (list name) entries)))

(defun cstate-long-locals ()
  "List of SLOT KEYS whose local slots hold Int64 directly (not boxed
   LispObject). Set in native function bodies where params are long-typed.
   compile-as-long skips :unbox-fixnum for these; fixnum-typed-p returns T.

   Keyed by slot, not by variable name: the fact recorded is about the SLOT, and
   a slot is never reused by another binding. A name-keyed list cannot express
   this; an inner binding of the same name (even from another package, which is
   a different variable entirely) had to be handled by dropping the outer entry,
   which silently un-declared a slot that was still live and still Int64, and the
   next read of it emitted a boxed load against an Int64 slot. With slot keys the
   question 'is THIS binding's slot native?' is answered by NATIVE-SLOT-P and
   shadowing needs no handling at all."
  (svref *cstate* +cs-long-locals+))

(defun cstate-native-double-locals ()
  "List of SLOT KEYS whose local slots hold a native r8 (double)
   directly, not a boxed DoubleFloat; the float analog of the long-locals
   table. Set by compile-let for double-float-declared, non-special,
   non-captured lexicals with a double-typed init. compile-as-double loads the
   slot raw (no unbox-double); compile-var-ref boxes on a generic read;
   compile-setq stores the raw double. This removes the per-setq DoubleFloat
   box in numeric loops (fft/mandelbrot's tr/ti/ur/ui accumulators). The
   variable is also in *double-float-locals*, so double-float-typed-p still
   reports it double."
  (svref *cstate* +cs-native-double-locals+))

(defun cstate-native-single-locals ()
  "Like the native-double table but for single-float locals (native r4 slot)."
  (svref *cstate* +cs-native-single-locals+))

(defun cstate-native-decimal-locals ()
  "Like the native-double table but for decimal locals: the slot holds a raw
   System.Decimal instead of a boxed LispDecimal. Set by compile-let for
   decimal-declared, non-special, non-captured lexicals with a decimal-typed
   init. compile-as-decimal loads the slot raw (no unbox-decimal), compile-setq
   stores it raw, and compile-var-ref boxes only when the value crosses back into
   generic code; which is what takes the get_Value/newobj pair out of every
   operation in a declared-decimal computation."
  (svref *cstate* +cs-native-decimal-locals+))

(defun cstate-native-char-locals ()
  "List of SLOT KEYS whose local slots hold a character's CODE as a raw Int32
   instead of a boxed LispChar. Set by compile-let for character-declared,
   non-special, non-captured lexicals whose init is a string element read.

   dotcl characters are UTF-16 code units and CHAR-CODE-LIMIT is 65536, so the
   code is exactly what a slot can hold; the character object adds nothing the
   comparisons use. CHAR-CODE-OPERAND reads the slot bare, COMPILE-VAR-REF
   rebuilds the character with LispChar.Make when the value crosses into generic
   code, and COMPILE-SETQ stores the raw code. What this removes is the
   CHAR-CODE-OF-L per comparison -- three per character in a scanner's
   whitespace test, which is what a tokenizer pays per input character."
  (svref *cstate* +cs-native-char-locals+))

(defun cstate-array-backing-locals ()
  "Alist (NAME-STRING KEY BACKING-SYMBOL . KIND) of locals declared
   (SIMPLE-ARRAY <integer type> (DIM)) whose element buffer was fetched once at
   binding time into BACKING-SYMBOL, a slot of the concrete array type. KIND is
   the runtime _numKind (1 = byte[], 2 = ushort[], 3 = int[], 4 = long[]).
   An AREF on such a local is a bare ldelem against that slot instead of a
   helper call that re-derives the buffer per element. KEY pins the binding the
   same way the numeric-array table does: a consumer only trusts the entry
   while (lookup-local NAME) still resolves to it."
  (svref *cstate* +cs-array-backing-locals+))

(defun cstate-struct-backing-locals ()
  "Alist (NAME-STRING KEY . BACKING-SYMBOL) of locals declared to hold a
   structure that has raw slot storage, whose raw int64 array was fetched once
   at binding time into BACKING-SYMBOL, an Int64[] slot.

   A raw slot read or write on such a local is a bare ldelem/stelem at a
   constant position against that slot, guarded by one test of the slot against
   null, instead of a helper call that re-derives the array, the layout entry
   and the position from the object on every access. BACKING-SYMBOL is null
   whenever the fetch could not be made -- the wrong layout version, no raw
   storage, not a structure -- and the null arm is the code that ran before, so
   nothing about the answers or the errors changes.

   KEY pins the binding exactly as the array table does: a consumer only trusts
   the entry while (lookup-local NAME) still resolves to it, which is what
   makes a shadowing binding or a closure re-key self-invalidating."
  (svref *cstate* +cs-struct-backing-locals+))

(defun cstate-tco-key-shared ()
  "In the shared body of a &key function: (PARAM-ENTRIES N-REQUIRED . KEYWORDS).
   The body takes every key as a positional argument, so a tail self-call has
   to be spread into that shape before it can loop (%KEY-SHARED-TCO-ARGS).
   PARAM-ENTRIES is the list the tco-param-entries slot held when this was set;
   the entry only applies while that slot still holds the same list, so a
   nested function that rebinds the TCO parameters is never matched against it."
  (svref *cstate* +cs-tco-key-shared+))

(defun cstate-numeric-array-locals ()
  "Alist (NAME-STRING KEY RANK LO . HI) of let locals proven to hold a
   numeric-backed array (make-array init with bounded-integer element type).
   aref on them reads/writes elements as raw int64 (Runtime.ArefNum*L) and
   contributes the storage range to expr-int-range. KEY pins the binding;
   consumers verify (lookup-local NAME) still resolves to it."
  (svref *cstate* +cs-numeric-array-locals+))

;; A single LABELS definition that never names itself is compiled as an FLET
;; (COMPILE-LABELS-1). That decision is taken on the SOURCE, and a macro can
;; still expand into a call of the name: the form is hidden from the walk
;; inside a literal it cannot see into (SBCL's backquote reads ,X as a
;; structure), or produced by the expander from data. Compiled as an FLET, such
;; a call would go to the global function of that name. Each entry here is
;; (NAME . TAG) for a demoted definition being compiled; a call of NAME that
;; resolves to no local binding throws to TAG, and the LABELS is compiled again
;; with its binding in scope.
(defvar *labels-demoted* nil)

(defvar *labels-once-inner* nil
  "The definitions of the LABELS form COMPILE-LABELS-BOXED is compiling as the
   run-once construction of a closed group, so the copy inside is compiled the
   ordinary way instead of being hoisted again.")

(defvar *compile-time-flet-defs* nil
  "List of flet function source definitions active during compilation.
   Each entry is (name lambda-list . body). Used by compile-defmacro
   to make flet-local functions available during compile-time eval.")

;;; ============================================================
;;; Utilities
;;; ============================================================

(defun gen-local (prefix)
  "Generate a unique local variable symbol in the compiler package.
   PREFIX is required (every call site passes one) so the lambda list stays
   required-only and the compiler routes calls through the direct-delegate
   fast path instead of the args-array entry."
  (intern (format nil "~a_~d" prefix (incf *var-counter*)) "DOTCL.CIL-COMPILER"))

(defvar *uninterned-var-names* (make-hash-table :test #'eq :synchronized t)
  "Uninterned variable symbol -> unique effective-name string.")
(defvar *uninterned-var-counter*
  ;; ATOMIC-LONG on dotcl so (set-parallel-eval t) workers can bump it lock-free:
  ;; a plain (incf) on a special var is a non-atomic read-modify-write and two
  ;; workers can lost-update to the same value, minting colliding names for
  ;; distinct gensyms. MAKE-ATOMIC-LONG is a runtime primitive absent on the SBCL
  ;; cross-compile host (which never runs VAR-NAME), so resolve it at load time by
  ;; name: DOTCL:: reader syntax would break reading on the host, which has no
  ;; DOTCL package. The host keeps a plain integer purely to stay loadable.
  (if (find-package "DOTCL")
      (funcall (find-symbol "MAKE-ATOMIC-LONG" "DOTCL") 0)
      0))

(defun uninterned-var-counter-next ()
  "Atomically bump the uninterned-var uniqueness counter and return the new
   value. Lock-free on dotcl (ATOMIC-LONG-INCF); a plain increment on the SBCL
   host, where VAR-NAME never runs concurrently."
  (if (integerp *uninterned-var-counter*)
      (incf *uninterned-var-counter*)
      (funcall (find-symbol "ATOMIC-LONG-INCF" "DOTCL") *uninterned-var-counter*)))

(defun name-key= (name key)
  "STRING= of NAME and KEY, where KEY is a name string or NIL for an entry that
   cannot match. Not STRING= itself: it takes the symbol NIL as the string
   \"NIL\", so a variable named NIL in some other package matched every such
   entry."
  (and key (string= name key)))

(defun var-name (sym)
  "Effective name string of a variable symbol for the string-keyed
   locals/free-var/capture machinery. Interned symbols use SYMBOL-NAME
   (cross-package name matching is intentional). Uninterned symbols get a
   unique stable name, because CL scoping is by symbol identity: two
   same-named gensyms; e.g. SBCL's (make-symbol \"CONSTRAINTS\") which
   deliberately omits a counter; are distinct variables, and one must not
   shadow the other (or an interned variable of the same name) in closure
   capture."
  (if (symbol-package sym)
      (symbol-name sym)
      (or (gethash sym *uninterned-var-names*)
          (setf (gethash sym *uninterned-var-names*)
                (format nil "~a#:~d" (symbol-name sym)
                        (uninterned-var-counter-next))))))

(defun local-function-entry (name)
  "The *LOCAL-FUNCTIONS* entry (NAME-STRING KEY BOXED-P CAPS TAG INLINE-PLAN NAME)
   NAME resolves to, or NIL; i.e. \"is this operator shadowed by an FLET/LABELS
   binding in scope?\", which guards every inlining, TCO and open-coding decision
   keyed on an operator name. The one place that walks *LOCAL-FUNCTIONS*, so that
   guard has one definition.

   NAME is a function name; a symbol, an (SETF f) list, or an ALREADY-MANGLED
   name string. Strings pass through untouched: entries are stored under
   MANGLE-NAME, which preserves symbol-name case, while MANGLE-NAME of a *string*
   upcases it; re-mangling would break a name that legitimately holds lowercase
   (a multiply-escaped symbol like |#{-reader|).

   The mangled string drops the package, so a symbol or list NAME must also be
   EQUAL to the function name the entry records: a LABELS binding of FOO::MULT
   does not shadow a call to BAR:MULT. A string NAME, or an entry that records
   no name, matches on the string alone."
  (let ((key (if (stringp name) name (mangle-name name))))
    (loop for entry in (cstate-local-functions)
          when (and (string= key (car entry))
                    (or (stringp name)
                        (null (nth 6 entry))
                        (equal name (nth 6 entry))))
            return entry)))

(defun %labels-demoted-check (name)
  "Throw back to the demoting LABELS when NAME, a function name about to be
   compiled as a global reference, is one of the demoted definitions."
  (let ((hit (and (symbolp name) (assoc name *labels-demoted* :test #'eq))))
    (when (and hit (not (local-function-entry name)))
      (throw (cdr hit) :labels-demote-aborted))))

(defun gen-label (prefix)
  "Generate a unique label symbol in the compiler package.
   PREFIX is required (every call site passes one) so the lambda list stays
   required-only and calls take the direct-delegate fast path (see GEN-LOCAL)."
  (intern (format nil "~a_~d" prefix (incf *label-counter*)) "DOTCL.CIL-COMPILER"))

;;; Precomputed "LispFunction.InvokeN" / "InvokeNativeN" method-name strings.
;;; Every compiled function call emits one of these; building it with (format nil
;;; "...~D" n) per call was a measurable per-form allocation. Index 0..8 covers
;;; the direct-call fast path; n>8 (array-arg path) falls back to format.
(defparameter +invoke-names+
  (coerce (loop for n from 0 to 8 collect (format nil "LispFunction.Invoke~D" n)) 'vector))
(defparameter +invoke-native-names+
  (coerce (loop for n from 0 to 8 collect (format nil "LispFunction.InvokeNative~D" n)) 'vector))

(defparameter +invoke-mode-names+
  (coerce (loop for n from 0 to 6 collect (format nil "LispFunction.Invoke~DM" n)) 'vector))

(defun invoke-mode-name (n)
  "LispFunction.InvokeNM: InvokeN plus the value mode (N <= 6)."
  (svref +invoke-mode-names+ n))

(defun invoke-name (n)
  "The shared LispFunction.InvokeN method-name string (no per-call format consing)."
  (if (<= 0 n 8) (svref +invoke-names+ n) (format nil "LispFunction.Invoke~D" n)))
(defun invoke-native-name (n)
  (if (<= 0 n 8) (svref +invoke-native-names+ n) (format nil "LispFunction.InvokeNative~D" n)))

(defun compile-sym-lookup (sym)
  "Generate CIL instructions to load a Symbol object for SYM.
Uses LOAD-SYM instructions to resolve symbols at assembly time
(constant pool), avoiding runtime dictionary lookups."
  (cond ((keywordp sym)
         `((:load-sym-keyword ,(symbol-name sym))))
        ;; Uninterned symbols (gensyms with nil package): use load-const to preserve identity
        ((null (symbol-package sym))
         `((:load-const ,sym)))
        ((and (not *cross-compiling*)
              (not (string= (package-name (symbol-package sym)) "COMMON-LISP")))
         `((:load-sym-pkg ,(symbol-name sym) ,(package-name (symbol-package sym)))))
        (t `((:load-sym ,(symbol-name sym))))))

(defun compile-fn-sym-lookup (sym)
  "Like compile-sym-lookup but for function-call sites: distinguishes
package-qualified from unqualified calls so GetFunctionBySymbol can be
authoritative (no cross-package bridge). Package-qualified (symbol-package
(symbol-package eq *package*) -> :load-sym-fn (Startup.SymFn bridges
at symbol-resolution to find the registered fbound symbol, checking the
symbol's home package first). Keywords and uninterned gensyms fall
through to compile-sym-lookup."
  (cond ((keywordp sym)
         `((:load-sym-keyword ,(symbol-name sym))))
        ((null (symbol-package sym))
         `((:load-const ,sym)))
        ((and (not *cross-compiling*)
              (not (string= (package-name (symbol-package sym)) "COMMON-LISP"))
              (eq (symbol-package sym) *package*))
         ;; Unqualified non-CL call: Startup.SymFn bridges to the
         ;; registered fbound symbol (e.g. class-precedence-list in CL-USER
         ;; -> dotcl-mop:class-precedence-list).
         `((:load-sym-fn ,(symbol-name sym) ,(package-name (symbol-package sym)))))
        (t (compile-sym-lookup sym))))

(defun %runtime-special-p (sym)
  "Check if the runtime marks SYM as special (via IsSpecial flag).
   Returns NIL during cross-compilation or if %SYMBOL-SPECIAL-P is unavailable."
  (when (not *cross-compiling*)
    (let ((fn (multiple-value-bind (s status)
                 (find-symbol "%SYMBOL-SPECIAL-P" "DOTCL-INTERNAL")
               (when (and s status (fboundp s))
                 (symbol-function s)))))
      (when fn (funcall fn sym)))))

(defun %standard-special-p (sym)
  "True when SYM is one of the special variables the standard defines in
   COMMON-LISP: every external CL symbol named *...* (the standard gives
   earmuffs to exactly those), plus the REPL variables + ++ +++ - / // ///
   (* ** *** are covered by the earmuff test, * by the list).
   While cross-compiling, %RUNTIME-SPECIAL-P cannot answer and nothing DEFVARs
   these, so without this a LET of *PACKAGE* or *PRINT-LENGTH* in the core
   would bind lexically. By name, so it holds on any host's CL symbols."
  (let ((pkg (symbol-package sym)))
    (and pkg
         (string= (package-name pkg) "COMMON-LISP")
         (let ((name (symbol-name sym)))
           (and (eq (nth-value 1 (find-symbol name pkg)) :external)
                (or (and (> (length name) 1)
                         (char= (char name 0) #\*)
                         (char= (char name (1- (length name))) #\*))
                    (member name '("*" "+" "++" "+++" "-" "/" "//" "///")
                            :test #'string=)))))))

(defun special-var-p (sym)
  "Check if a symbol is a special (dynamic) variable (includes locally declared specials).
   Matching is by symbol identity. Specialness belongs to the symbol, so a
   DEFVAR of FOO in one package must not make another package's FOO special."
  (unless (symbolp sym) (return-from special-var-p nil))
  (or (member sym *specials*)
      (and *cross-compiling* (%standard-special-p sym))
      (%runtime-special-p sym)))

(defun global-special-p (sym)
  "Check if a symbol is GLOBALLY special (via defvar/defparameter/proclaim).
   Used for binding classification: only globally special vars force nested let bindings dynamic.
   By symbol identity, as in SPECIAL-VAR-P."
  (unless (symbolp sym) (return-from global-special-p nil))
  (or (member sym *global-specials*)
      (and *cross-compiling* (%standard-special-p sym))
      (%runtime-special-p sym)))

(defun same-var-package-p (k sym-pkg)
  "Return (var-name k) if K's package is compatible with SYM-PKG for
  lookup-local matching: same package, DOTCL.CIL-COMPILER (closure
  env-locals), or uninterned (gensyms). nil otherwise.

  The DOTCL.CIL-COMPILER clause looks obsolete and is not. It was introduced to
  compensate for compile-closure-body registering env slots under a symbol
  re-interned from the free variable's name; that intern is long gone (the
  captured symbol itself is the slot key now), which makes the clause read like
  leftover scaffolding. Removing it has been tried: the SIL oracle corpus stays
  byte-identical and test-regression stays green, but the ANSI suite fails
  RESTART-CASE.21, which is

      (flet ((%f (s2) (format s2 \"A report\")))
        (restart-case ... (foo () :report %f)))
;
  a local function reached by name through a restart's report slot, which
  still routes through a compiler-package entry. Keep the clause until that
  path changes, and note that the corpus alone will not catch its removal."
  (let ((k-pkg (symbol-package k)))
    (if (or (null k-pkg)
            (null sym-pkg)
            (eq k-pkg sym-pkg)
            (string= (package-name k-pkg) "DOTCL.CIL-COMPILER"))
        (var-name k)
        nil)))

(defun bnd-member-p (sym bnd)
  "Check if SYM is bound by BND (the free-var bound-names list).
  BND entries are normally var-name strings, but the let/let* handler in
  find-free-vars-expr stores the binding SYMBOL instead, so EQ can
  distinguish same-name vars from different packages.
  String entries match by STRING=; symbol entries by EQ."
  (or (member sym bnd :test #'eq)
      (let ((name (var-name sym)))
        (member name bnd
                :test (lambda (n entry)
                        (and (stringp entry) (string= n entry)))))))

(defun local-entry (sym)
  "The *LOCALS* entry (BINDING-SYMBOL . SLOT-KEY) SYM resolves to, or NIL.
  Symbol identity first, then a name fallback. The var-name fallback is
  package-aware: it only matches entries from the same package, from
  DOTCL.CIL-COMPILER (closure env-locals), or uninterned (gensyms). This
  prevents cross-package collisions where two different packages have a symbol
  with the same printed name.

  This and LOCAL-ENTRY-BY-NAME are the only two places that walk *LOCALS*:
  callers that need the slot, the binding symbol, or just a yes/no go through
  them rather than re-inlining ASSOC + VAR-NAME, so the matching rule has one
  definition (the string-keyed design is the root of the shadow-bug class)."
  (or (assoc sym (cstate-locals) :test #'eq)
      (let ((name (var-name sym))
            (sym-pkg (symbol-package sym)))
        (assoc name (cstate-locals)
               :key (lambda (k)
                      (if (symbolp k)
                          (same-var-package-p k sym-pkg)
                          nil))
               ;; NIL from the key means "no match", and STRING= would take it
               ;; as the name "NIL": a variable named NIL in another package
               ;; matched the first local whose package was not its own.
               :test #'name-key=))))

(defun local-entry-by-name (name locals)
  "The entry in LOCALS (an alist shaped like *LOCALS*) whose variable's effective
  name is NAME, or NIL. The name-keyed counterpart of LOCAL-ENTRY, for callers
  that hold only a VAR-NAME string; free-var capture, closure env-locals, the
  typed-local check. LOCALS is required rather than defaulting to *LOCALS* so the
  lambda list stays required-only (direct-delegate calls; see GEN-LOCAL).

  No package filter is possible here: a name string has lost the symbol's
  package. Callers that still hold the symbol should use LOCAL-ENTRY."
  (assoc name locals
         :key (lambda (k) (if (symbolp k) (var-name k) nil))
         :test #'name-key=))

(defun lookup-local (sym)
  "The slot key SYM's binding lives in, or NIL. See LOCAL-ENTRY for the
  matching rule."
  (cdr (local-entry sym)))

;; When true, LOCAL-BOUND-P answers T for every symbol. Bound only during
;; free-var CANDIDATE collection (%compute-free-candidates, cil-analysis.lisp):
;; candidates are the structurally-free names of a lambda,
;; independent of *locals*, so they can be memoized per form. The real
;; *locals*-dependent filter is re-applied at each enclosing merge point.
(defvar *ffv-assume-bound* nil)

(defun local-bound-p (sym)
  "Check if symbol is bound in *locals* or has a boxed entry in *local-functions*.
  Package-aware: the var-name fallback only matches entries from the same
  package, DOTCL.CIL-COMPILER (closure env-locals), or uninterned (gensyms).
  This prevents a reference from matching a same-named binding in a
  different package, which would suppress free-var capture and cause
  the closure to read the wrong variable at runtime."
  (or *ffv-assume-bound*
      (local-entry sym)
      ;; Also check *local-functions* for boxed labels functions
      ;; (supports closure capture of labels functions whose name clashes with a variable)
      (local-function-entry sym)))

(defun local-bound-name-p (name)
  "Like LOCAL-BOUND-P but keyed by an effective-name STRING, so callers holding
   only a var-name (e.g. the free-var candidate merge) need not intern
   a symbol just to test binding. Equivalent to LOCAL-BOUND-P of any symbol whose
   VAR-NAME is NAME: the symbol-identity (EQ) branch of LOCAL-BOUND-P can only
   match an entry that this by-name branch also matches. Avoids the package
   mutation an INTERN would cause (harmful under concurrent compile)."
  (or (local-entry-by-name name (cstate-locals))
      (local-function-entry name)))


(defun boxed-var-p (sym)
  "Check if a variable needs boxing (by effective name, cross-package safe)."
  (member (var-name sym) (cstate-boxed-vars)
          :key (lambda (x) (if (symbolp x) (var-name x) x))
          :test #'string=))

(defun labels-cell-var-p (name-or-sym)
  "True if NAME-OR-SYM names a labels function cell; a boxed LispObject[1]
   holding a LispFunction, tracked in *locals* under a __LABELFN_ prefix. Such
   cells keep the array representation even under debug info emission (only
   genuine data variables use the debug-only LispBox cell), so their box
   reads/writes and env captures must stay ldelem / LispObject[]."
  (let ((name (if (stringp name-or-sym) name-or-sym (var-name name-or-sym))))
    (and (>= (length name) 10)
         (string= name "__LABELFN_" :end1 10))))

(defun mangle-name (symbol)
  "Convert a Lisp symbol/name to a display string (for defmethod names).
   Handles (setf foo), (cas foo), string names, and (\"c-name\" lisp-name) pairs.
   Preserves symbol-name case exactly so that multi-escaped symbols like |#{-reader|
   register under the same case-sensitive name that the reader interns them with."
  (cond
    ;; String: upcase (e.g. C name strings from define-alien-routine)
    ((stringp symbol) (string-upcase symbol))
    ((and (consp symbol) (eq (car symbol) 'setf))
     (format nil "(SETF ~A)" (symbol-name (cadr symbol))))
    ((and (consp symbol) (stringp (car symbol)))
     ;; ("c-name" lisp-name) pair from define-alien-routine -> use the Lisp name
     (if (symbolp (cadr symbol))
         (symbol-name (cadr symbol))
         (string-upcase (car symbol))))
    ((consp symbol)
     ;; Generic compound function name: (OP NAME) e.g. (cas car)
     (format nil "(~A~{ ~A~})"
             (if (symbolp (car symbol))
                 (symbol-name (car symbol))
                 (prin1-to-string (car symbol)))
             (mapcar (lambda (s) (if (symbolp s) (symbol-name s)
                                     (prin1-to-string s)))
                     (cdr symbol))))
    (t (symbol-name symbol))))

(defun constant-binding-name-p (name)
  "True when NAME may not be bound as a variable. CLHS 3.1.2.1.1.3: naming a
   constant variable in any binding form is a program error, and that includes
   NIL and T (which are constants). One predicate so every binding site --
   LET/LET*, lambda lists, DO, DOLIST, MULTIPLE-VALUE-BIND, handler-case
   clauses -- agrees on the answer; the previous state was that assignment was
   checked (SETQ/SET) and binding was not, so (let ((t 1)) t) quietly returned 1."
  ;; CONSTANTP, not SYMBOL-CONSTANT-P: on a symbol the two agree (dotcl's
   ;; CONSTANTP delegates to it, adding NIL/T/keywords), and CONSTANTP is the
   ;; one that also exists in the host SBCL this function runs in during
   ;; cross-compilation.
  (and (symbolp name) (constantp name)))

(defun check-binding-name (name context)
  "Signal PROGRAM-ERROR if NAME cannot be bound as a variable. CONTEXT names the
   binding form for the message."
  (when (constant-binding-name-p name)
    (error 'program-error
           :format-control "~a: ~s is a constant and cannot be bound as a variable"
           :format-arguments (list context name)))
  name)

(defun parse-lambda-list (params)
  "Parse lambda list -> (values required optional key rest-param aux allow-other-keys-p has-key-p).
   &body is treated as &rest. rest-param is NIL if no variadic.
   optional: ((name default-form) ...)
   key: ((keyword-name var-name default-form) ...)
   aux: ((name init-form) ...)
   allow-other-keys-p: T if &allow-other-keys was present
   has-key-p: T if &key was present (even if no key params listed)"
  (let ((required '()) (optional '()) (key '()) (rest-param nil) (aux '())
        (allow-other-keys-p nil) (has-key-p nil)
        (state :required))
    (dolist (p params)
      (cond
        ((member p '(&rest &body)) (setf state :rest))
        ((eq p '&optional) (setf state :optional))
        ((eq p '&key) (setf state :key) (setf has-key-p t))
        ((eq p '&allow-other-keys) (setf allow-other-keys-p t))
        ((eq p '&aux) (setf state :aux))
        ((eq state :aux)
         (if (consp p)
             (push (list (check-binding-name (car p) "lambda list") (cadr p)) aux)
             (push (list (check-binding-name p "lambda list") nil) aux)))
        ((eq state :required) (push (check-binding-name p "lambda list") required))
        ((eq state :optional)
         (if (consp p)
             (progn
               (when (cddr p) (check-binding-name (caddr p) "lambda list"))
               (push (list (check-binding-name (car p) "lambda list")
                           (cadr p) (caddr p))
                     optional))
             (push (list (check-binding-name p "lambda list") nil nil) optional)))
        ((eq state :key)
         (if (consp p)
             (let* ((spec (car p))
                    (explicit-p (consp spec))
                    (var-name (if explicit-p (cadr spec) spec))
                    (key-sym (if explicit-p (car spec) spec))
                    (key-name (symbol-name key-sym))
                    (key-pkg (when explicit-p
                               (let ((pkg (symbol-package key-sym)))
                                 (if pkg (package-name pkg) ""))))
                    (default (cadr p))
                    (supplied-p (caddr p)))
               (check-binding-name var-name "lambda list")
               (when (cddr p) (check-binding-name supplied-p "lambda list"))
               (push (list key-name var-name default supplied-p key-pkg) key))
             (push (list (symbol-name p) (check-binding-name p "lambda list") nil nil nil) key)))
        ((eq state :rest)
         (setf rest-param (check-binding-name p "lambda list"))
         (setf state :done))))
    (values (nreverse required) (nreverse optional) (nreverse key) rest-param (nreverse aux) allow-other-keys-p has-key-p)))

(defun %check-lambda-call-keywords (op args)
  "When OP is a (LAMBDA lambda-list ...) with &KEY and no &ALLOW-OTHER-KEYS, warn
   at compile time about literal unknown keyword arguments in a direct call
   ((lambda (&key a) a) :b 1) per CLHS 3.5.1.4 (static diagnosis).
   Only literal keywords are checked; non-literal keyword forms are left to the
   runtime keyword-validation. A literal :ALLOW-OTHER-KEYS with a non-NIL value in
   the call suppresses the warning, per CLHS."
  (when (and (consp op) (eq (car op) 'lambda) (consp (cdr op)) (listp (cadr op)))
    (multiple-value-bind (required optional key rest-param aux aok-p has-key-p)
        (parse-lambda-list (cadr op))
      (declare (ignore aux rest-param))
      (when (and has-key-p (not aok-p))
        (let* ((nfixed (+ (length required) (length optional)))
               (kw-args (when (> (length args) nfixed) (nthcdr nfixed args)))
               (accepted (mapcar #'car key))) ; symbol-name strings
          (flet ((aok-name-p (k)
                   (and (keywordp k) (string= (symbol-name k) "ALLOW-OTHER-KEYS"))))
            ;; A literal :allow-other-keys <non-nil> in the call permits other keys.
            (unless (loop for (k v) on kw-args by #'cddr
                          thereis (and (aok-name-p k) v (not (eq v 'nil))))
              (loop for k in kw-args by #'cddr
                    when (and (keywordp k) (not (aok-name-p k))
                              (not (member (symbol-name k) accepted :test #'string=)))
                      do (warn "unknown &KEY argument ~S in call to a ~
                                LAMBDA with keys ~S"
                               k (mapcar (lambda (n) (intern n :keyword)) accepted))))))))))

(defun lambda-list-keyword-p (sym)
  "Check if symbol is a lambda list keyword."
  (and (symbolp sym)
       (member sym '(&rest &body &optional &key &allow-other-keys &aux &whole &environment))))

(defun map-lambda-list-vars (params visit &optional (bound '()))
  "Walk PARAMS as a lambda list, left to right, maintaining the scope CLHS
   3.4.1.5 gives init forms: an init form sees the parameters to its left and
   no others. VISIT, when non-NIL, is called as (funcall VISIT init-form scope)
   for each &optional/&key/&aux parameter that has one, with SCOPE the names
   visible at that point. Returns the scope after the whole lambda list: the
   parameter names (as VAR-NAME strings) pushed onto BOUND, most recent first.

   This is the only lambda-list state machine in the compiler. It used to be
   copied per caller, and the copies had already drifted apart on what follows
   &rest."
  (let ((state :required)
        (scope bound))
    (dolist (p params)
      (cond
        ((lambda-list-keyword-p p)
         (case p
           ((&rest &body) (setf state :rest))
           (&optional (setf state :optional))
           (&key (setf state :key))
           (&aux (setf state :aux))
           (t nil)))
        ;; A non-keyword parameter after the &rest variable is not a valid
        ;; lambda list; ignore it rather than binding a second rest name.
        ((eq state :done) nil)
        ((eq state :required)
         (push (var-name p) scope))
        ((eq state :rest)
         (push (var-name p) scope)
         (setf state :done))
        (t                              ; :optional / :key / :aux
         (when (and visit (consp p) (cadr p))
           (funcall visit (cadr p) scope))
         ;; The parameter itself becomes visible only after its own init form.
         (let ((name (if (consp p) (car p) p)))
           ;; &key ((keyword var) init supplied-p): the variable is the cadr.
           (when (consp name) (setf name (cadr name)))
           (push (var-name name) scope))
         (when (and (consp p) (caddr p))
           (push (var-name (caddr p)) scope)))))
    scope))

(defun extract-param-names (params)
  "Extract variable names (as effective-name strings, see VAR-NAME) from a
   lambda list, handling &optional/&key specs."
  (nreverse (map-lambda-list-vars params nil)))

(defun scan-lambda-list-defaults (params bound free-ht)
  "Scan default value forms in &optional/&key parameters for free variable references.
   Per CLHS 3.4.1.5, each init-form may only refer to params to its left,
   so we progressively add param names to bound as we process each default."
  (map-lambda-list-vars params
                        (lambda (init scope) (find-free-vars-expr init scope free-ht))
                        bound)
  nil)

;;; ============================================================
;;; Instruction builders (thin wrappers for readability)
;;; ============================================================

(defun emit-fixnum (n)
  `((:ldc-i8 ,n) (:call "Fixnum.Make")))

(defun emit-nil ()
  '((:ldsfld "Nil.Instance")))

(defun emit-t ()
  '((:ldsfld "T.Instance")))

;;; ============================================================
;;; Quote / literal compilation
;;; ============================================================

(defun %externalizable-constant-p (obj)
  "True when OBJ written to the cross-compile output and read back means the same
thing, AND nothing in this compiler will try to modify it in place.

Strings, numbers and characters qualify, and lists of them. Symbols do not:
which package they land in is decided by the reader that reads the output, not
by the compiler that wrote it.

Keywords would survive the round trip, but they are excluded on purpose. An
instruction list is a list of keyword-headed forms, and the compiler builds its
output by NCONCing instruction lists together. A constant template that reaches
one of those NCONCs must not be shared, or the appended tail stays attached to
the literal and the next compilation inherits it."
  (cond ((null obj) t)
        ((eq obj t) t)
        ((consp obj) (and (%externalizable-constant-p (car obj))
                          (%externalizable-constant-p (cdr obj))))
        ((stringp obj) (= (array-rank obj) 1))
        ((numberp obj) t)
        ((characterp obj) t)
        ((keywordp obj) t)   ; DETECTOR
        (t nil)))

(defun compile-quoted-cached (obj)
  "COMPILE-QUOTED, but when the result is a recipe rather than a constant, wrap
   it so the recipe runs once and the value is kept.

   Cross-compilation cannot put a symbol inside :LOAD-CONST (the text would be
   read back into whatever package the reader is in), so a quoted list of
   symbols comes out as LOAD-SYM + MAKECONS -- a chain that rebuilds the list on
   every call. SPECIAL-OPERATOR-NAME-P, whose whole body is a MEMBER against a
   25 element quoted list, cost 800 bytes a call for exactly this reason, and it
   is one of the two hottest allocation sites in the compiler itself.

   :LOAD-CONST-CACHED keeps the recipe (so the symbols are still resolved at
   first use, and nothing has to exist earlier than it does now) and stores the
   result, which is what a literal is supposed to be anyway."
  (let ((instrs (compile-quoted obj)))
    (if (and *cross-compiling*
             (cdr instrs)                    ; more than one instruction = a recipe
             (consp obj))
        `((:load-const-cached ,instrs))
        instrs)))

(defun compile-quoted (obj)
  "Compile a quoted datum to instruction list."
  (cond
    ((null obj) (emit-nil))
    ((eq obj t) (emit-t))
    ((integerp obj)
     (if (typep obj '(integer #.(- (expt 2 63)) #.(1- (expt 2 63))))
         (emit-fixnum obj)
         `((:load-const ,obj))))
    ((and (stringp obj) (= (array-rank obj) 1))
     (if *cross-compiling*
         `((:ldstr ,obj) (:newobj "LispString"))
         `((:load-const ,obj))))
    ((characterp obj)
     `((:ldc-i4 ,(char-code obj)) (:call "LispChar.Make")))
    ((symbolp obj)
     (if *cross-compiling*
         (compile-sym-lookup obj)
         `((:load-const ,obj))))
    ((consp obj)
     ;; A quoted list is one object, not a recipe. LOAD-CONST puts it in the
     ;; constant pool, so every evaluation of the site yields the same list --
     ;; which is what the standard says about a literal, and what every other
     ;; implementation does.
     ;;
     ;; Cross-compilation cannot always take that road: the output is text, and
     ;; a symbol written there is read back into whatever package the reader is
     ;; in, not the one the compiler meant. So symbols still go through
     ;; COMPILE-SYM-LOOKUP and the list around them is rebuilt with MAKECONS.
     ;; Data that means the same on both sides of the file does not need that,
     ;; and the standard library's dispatch tables -- lists of strings handed to
     ;; MEMBER -- are exactly that shape. They were being consed again on every
     ;; single call.
     (if (and *cross-compiling* (not (%externalizable-constant-p obj)))
         `(,@(compile-quoted (car obj))
           ,@(compile-quoted (cdr obj))
           (:call "Runtime.MakeCons"))
         `((:load-const ,obj))))
    ((typep obj 'single-float)
     `((:load-const ,obj)))
    ((typep obj 'double-float)
     `((:load-const ,obj)))
    ((typep obj 'ratio)
     `((:load-const ,obj)))
    ((bit-vector-p obj)
     ;; Preserve bit-vector element type via load-const (newobj "LispVector" would lose BIT type)
     `((:load-const ,obj)))
    ((vectorp obj)
     (if *cross-compiling*
         (if (simple-vector-p obj)
             ;; 1D simple vector: reconstruct element-by-element during cross-compile
             `((:ldc-i4 ,(length obj))
               (:newarr "LispObject")
               ,@(loop for i from 0 below (length obj)
                       append `((:dup) (:ldc-i4 ,i) ,@(compile-quoted (aref obj i)) (:stelem-ref)))
               (:newobj "LispVector"))
             ;; Multi-dimensional or specialized vector: use load-const
             `((:load-const ,obj)))
         ;; At runtime, use load-const to preserve EQL identity
         `((:load-const ,obj))))
    ((pathnamep obj)
     (if *cross-compiling*
         ;; Emit explicit make-pathname to preserve all components (esp. version)
         ;; through text serialization in compile-file output
         (let ((parts (list (pathname-host obj) (pathname-device obj)
                            (pathname-directory obj) (pathname-name obj)
                            (pathname-type obj) (pathname-version obj))))
           `(,@(compile-args-array (mapcar (lambda (p) `(quote ,p)) parts))
             (:call "Runtime.MakePathnameFromParts")))
         ;; At runtime, use load-const to preserve EQL identity
         `((:load-const ,obj))))
    ((complexp obj)
     `((:load-const ,obj)))
    ;; General fallback for runtime objects (packages, hash-tables, functions, etc.)
    (t `((:load-const ,obj)))))

;;; ============================================================
;;; Variable reference
;;; ============================================================

(defun lookup-symbol-macro (sym)
  "Return (values expansion found-p). FOUND-P distinguishes a symbol-macro whose
   expansion happens to be NIL (e.g. (symbol-macrolet ((foo nil)) foo)) from an
   unregistered symbol; both have a NIL expansion, so callers must branch on
   FOUND-P, not on the expansion's truth value."
  (let ((cell (or (assoc sym *symbol-macros* :test #'eq)
                  (and (symbol-package sym)
                       (assoc (var-name sym) *symbol-macros*
                              :key (lambda (k) (if (and (symbolp k) (symbol-package k))
                                                   (var-name k) nil))
                              :test #'name-key=)))))
    (cond
      ;; Under the tree-walk interpreter the lexical environment is its ENV
      ;; alist, not *LOCALS* / *SYMBOL-MACROS*: a variable entry shadows, a
      ;; symbol-macrolet entry is (name marker expansion).
      ((listp *sm-interp-env*)
       (let ((b (assoc sym *sm-interp-env*)))
         (cond
           ((and b (consp (cdr b)) (eq (cadr b) *%mini-symbol-macro-marker*))
            (values (caddr b) t))
           (b (values nil nil))
           (cell (values (cdr cell) t))
           (t (multiple-value-bind (val found) (gethash sym *global-symbol-macros*)
                (if found (values val t) (values nil nil)))))))
      ;; Inside the free-variable walk, a name bound by a LET / lambda within
      ;; the walked form shadows any symbol macro visible here: the walk
      ;; drops a name from BND where a SYMBOL-MACROLET inside rebinds it, but
      ;; it does not remove shadowed entries from *SYMBOL-MACROS*.
      ((and (listp *sm-walker-bound*) *sm-walker-bound*
            (bnd-member-p sym *sm-walker-bound*))
       (values nil nil))
      (cell (values (cdr cell) t))
      ;; A lexical variable binding of the same name shadows a global symbol
      ;; macro (CLHS 3.1.1). Lexical symbol macros are removed from
      ;; *SYMBOL-MACROS* when LET binds the name, but the global table is not
      ;; scoped, so it has to be checked against *LOCALS* here. Without this,
      ;; SETQ / SETF / INCF of a LET-bound name used the global expansion.
      ((local-entry sym) (values nil nil))
      (t
       ;; Check global symbol macros from DEFINE-SYMBOL-MACRO
       (multiple-value-bind (val found) (gethash sym *global-symbol-macros*)
         (cond
           ((not found) (values nil nil))
           (t
            (when *sm-walker-context*
              (setf *expansion-uncacheable* t)
              (push sym *sm-global-hits*))
            (values val t))))))))

(defun compile-var-ref (sym)
  "Compile a variable reference."
  ;; Local variable bindings shadow symbol macros (CLHS 5.1.2.1).
  (let ((key (lookup-local sym)))
    (if key
        (if (boxed-var-p sym)
            ;; Boxed read: LispObject[1] cell (normal, and labels function cells
            ;; even under debug) or LispBox.Value (debug data variable).
            (if (and *emit-source-lines* (not (labels-cell-var-p sym)))
                `((:ldloc ,key) (:ldfld "LispBox.Value"))
                `((:ldloc ,key) (:ldc-i4 0) (:ldelem-ref)))
            (if (and (boundp '*cstate*)
                     (native-slot-p sym (cstate-long-locals)))
                `((:ldloc ,key) (:call "Fixnum.Make"))
                ;; Native float slot: the raw r8/r4 needs boxing for a generic
                ;; (LispObject) read; native float contexts use compile-as-*.
                (cond
                  ((eq (float-native-local-kind sym) :double)
                   `((:ldloc ,key) (:newobj "DoubleFloat")))
                  ((eq (float-native-local-kind sym) :single)
                   `((:ldloc ,key) (:newobj "SingleFloat")))
                  ;; Native decimal slot boxes the same way on a generic read.
                  ((decimal-native-local-p sym)
                   `((:ldloc ,key) (:newobj "LispDecimal")))
                  ;; Native character slot: the slot holds the code, so the
                  ;; character is rebuilt here. LispChar.Make takes a CHAR, and
                  ;; the code was kept at int64 width for the comparison path,
                  ;; so it narrows first. Make interns the ASCII range, so this
                  ;; is usually not an allocation.
                  ((char-native-local-p sym)
                   `((:ldloc ,key) (:conv-i4) (:call "LispChar.Make")))
                  (t `((:ldloc ,key))))))
        ;; No local binding: check symbol-macro (let/let* shadow these too)
        (multiple-value-bind (sm-exp found) (lookup-symbol-macro sym)
          (if found
              (compile-expr sm-exp)
              ;; No lexical binding: check special (includes locally declared specials)
              `(,@(compile-sym-lookup sym)
                (:castclass "Symbol") (:call "DynamicBindings.Get")))))))

;;; ============================================================
;;; Main expression compiler
;;; ============================================================

(defvar *in-mv-context* nil
  "T when compiling an expression whose multiple values should propagate
  (e.g. the form inside multiple-value-list). Default nil = unwrap MvReturn.")

(defun xref-note-instrs ()
  "CIL instructions calling dotcl:%xref-note with the edges collected for the
   current caller; run at load time, they register the edges in the runtime
   xref table. NIL when nothing was collected (or during cross-compile)."
  (when (and *xref-edges* (not *cross-compiling*))
    (let* ((pkg (find-package "DOTCL"))
           (note (and pkg (intern "%XREF-NOTE" pkg))))
      (when note
        ;; *xref-caller* nil: the %xref-note call itself must not be recorded
        ;; as an edge of the function being registered.
        (let ((caller *xref-caller*)
              (edges (reverse *xref-edges*))
              (*xref-caller* nil)
              (*in-tail-position* nil)
              (*in-mv-context* nil))
          `(,@(compile-expr `(,note (quote ,caller) (quote ,edges)))
            (:pop)))))))
(defun compile-expr-raw (expr)
  "Compile expression without MvReturn unwrapping."
  ;; SBCL cross-compile: expand SB-INT:QUASIQUOTE at compile time
  #+sbcl
  (when (and (consp expr)
             (symbolp (car expr))
             (string= (symbol-name (car expr)) "QUASIQUOTE")
             (let ((pkg (symbol-package (car expr))))
               (and pkg (member (package-name pkg) '("SB-INT" "SB-IMPL") :test #'string=))))
    (setf expr (macroexpand-1 expr)))
  (setf expr (fold-constant-symbol expr))
  (cond
    ((integerp expr)
     (if (typep expr '(integer #.(- (expt 2 63)) #.(1- (expt 2 63))))
         (emit-fixnum expr)
         ;; Bignum: use constant pool
         `((:load-const ,expr))))
    ((typep expr 'single-float) `((:load-const ,expr)))
    ((typep expr 'double-float) `((:load-const ,expr)))
    ((typep expr 'ratio) `((:load-const ,expr)))
    ((and (stringp expr) (= (array-rank expr) 1))
     (if *cross-compiling*
         `((:ldstr ,expr) (:newobj "LispString"))
         `((:load-const ,expr))))
    ((characterp expr) `((:ldc-i4 ,(char-code expr)) (:call "LispChar.Make")))
    ((null expr) (emit-nil))
    ((eq expr t) (emit-t))
    ((keywordp expr) `((:load-sym-keyword ,(symbol-name expr))))
    ((symbolp expr) (compile-var-ref expr))
    ((consp expr) (compile-form expr))
    ((vectorp expr)
     ;; Vector literals are self-evaluating in CL: quote each element
     (compile-quoted expr))
    ;; Other self-evaluating objects (pathnames, etc.) -> load as constant
    (t `((:load-const ,expr)))))


(defun compile-expr (expr)
  "Compile expression. Unwraps MvReturn unless in MV-propagating position.
   Tail positions (*in-tail-position* t) propagate MV to the caller.
   MV-context positions (*in-mv-context* t, e.g. inside multiple-value-list)
   also propagate. Single-value forms never produce MvReturn so no unwrap.
   A structure slot read is single-valued too, but it is not recognisable from
   the form alone, so it is settled from the code that was actually emitted
   (STRUCT-SLOT-READ-EMITTED-P)."
  (let ((code (let ((*mv-expect-args*
                      ;; A named call compiled here in single-value position is
                      ;; followed by nothing but the UnwrapMv below.
                      (if (and (consp expr) (not *in-mv-context*) (not *in-tail-position*))
                          (cons (cdr expr) :primary)
                          *mv-expect-args*)))
                (compile-expr-raw expr))))
    (let ((c2 (if (or *in-mv-context* *in-tail-position* (single-value-form-p expr)
                      (struct-slot-read-emitted-p expr code))
                  code
                  `(,@code (:call "Runtime.UnwrapMv")))))
      ;; Debug info: prepend a source-span marker for literally-written cons forms
      ;; (only when line emission is on). A no-op instruction for the assembler
      ;; unless it is collecting sequence points. SPAN is (start-line start-col
      ;; end-line end-col) from *emit-source-lines*, or NIL (macroexpansion output).
      (if (and *emit-source-lines* (consp expr))
          (let ((span (funcall *emit-source-lines* expr)))
            (if span (cons (cons :line span) c2) c2))
          c2))))

(defun compile-for-single-value (expr)
  "Compile expr and ensure result is a single value (unwrap MvReturn).
   Forces unwrap even in MV context. Used at positions that must always be single-valued."
  (if (single-value-form-p expr)
      (compile-expr-raw expr)
      (let ((code (let ((*mv-expect-args* (if (consp expr) (cons (cdr expr) :primary) nil)))
                    (compile-expr-raw expr))))
        (if (struct-slot-read-emitted-p expr code)
            code
            `(,@code (:call "Runtime.UnwrapMv"))))))

;;; ============================================================
;;; Form dispatch
;;; ============================================================

(defvar *compile-depth* 0)


(defun %symbol-macro-name-p (sym)
  "True when SYM names a symbol macro here: a global one, or a SYMBOL-MACROLET
   binding in *SYMBOL-MACROS*."
  (or (nth-value 1 (gethash sym *global-symbol-macros*))
      (assoc sym *symbol-macros* :test #'eq)))

(defun %symbol-macro-string-name-p (name)
  "%SYMBOL-MACRO-NAME-P for a name string (the free-variable walk records some
   bound names as strings)."
  (or (assoc name *symbol-macros*
             :key (lambda (k) (if (symbolp k) (symbol-name k) ""))
             :test #'string=)
      (block found
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (when (string= (symbol-name k) name) (return-from found t)))
                 *global-symbol-macros*)
        nil)))

(defun %symbol-macro-shadows ()
  "The lexical variables in scope here that shadow a symbol macro of the same
   name, innermost first: symbols, or name strings where the free-variable walk
   kept only the name.

   Two sources. *SM-WALKER-BOUND*, inside an analysis walk: the names bound
   within the walked form, which the walk drops where a SYMBOL-MACROLET inside
   rebinds them, so each one that names a symbol macro shadows it. Then
   *LOCALS*: the compiler takes a LET-bound name out of *SYMBOL-MACROS* and a
   SYMBOL-MACROLET-bound one out of *LOCALS*, so a name in both was bound by a
   SYMBOL-MACROLET the walk entered and is a symbol macro here; only a global
   symbol macro is shadowed from *LOCALS*.

   A walk that keeps no bound names (the mutation walk) cannot tell, so the
   expansion is not cached for the compiler, which expands it again with
   *LOCALS* in place."
  (let ((out '()))
    (when (or *symbol-macros* (plusp (hash-table-count *global-symbol-macros*)))
      (when (listp *sm-walker-bound*)
        (dolist (v *sm-walker-bound*)
          (cond ((stringp v)
                 (when (%symbol-macro-string-name-p v) (push v out)))
                ((and v (symbolp v) (symbol-package v) (%symbol-macro-name-p v))
                 (push v out)))))
      (when (and *sm-walker-context* (eq *sm-walker-bound* :none))
        (setf *expansion-uncacheable* t))
      (when (plusp (hash-table-count *global-symbol-macros*))
        (dolist (e (cstate-locals))
          (let ((v (car e)))
            (when (and v (symbolp v) (symbol-package v)
                       (nth-value 1 (gethash v *global-symbol-macros*))
                       (not (assoc v *symbol-macros* :test #'eq)))
              (push v out))))))
    (nreverse out)))

(defun %add-symbol-macro-shadows (ht vars)
  "Enter in HT (an environment's symbol-macro table) a shadow entry
   (MARKER . VAR) for each of VARS (symbols or name strings, innermost first),
   keyed by VAR itself. A key already in HT is left alone: the entry put there
   first is the inner binding. Returns HT, made on demand (NIL if VARS is empty
   and HT was NIL)."
  (dolist (v vars ht)
    (unless ht (setq ht (make-hash-table :test #'equal)))
    (unless (nth-value 1 (gethash v ht))
      (setf (gethash v ht) (cons *symbol-macro-shadow-marker* v)))))

(defun %symbol-macro-shadow-entry-p (entry)
  "True when ENTRY, a value from an environment's symbol-macro table, is a
   variable's shadow entry rather than an expansion."
  (and (consp entry) (eq (car entry) *symbol-macro-shadow-marker*)))

(defun %add-env-symbol-macro (ht name expansion)
  "Enter NAME's SYMBOL-MACROLET EXPANSION in HT, an environment's symbol-macro
   table, keyed by the symbol, unless an inner binding of NAME is already there:
   an entry under NAME, or a shadow under its name string (the analysis walk
   keeps some bound names only as strings)."
  (unless (or (nth-value 1 (gethash name ht))
              (nth-value 1 (gethash (symbol-name name) ht)))
    (setf (gethash name ht) expansion))
  ht)

(defun %reify-macro-environment ()
  "Reify the compiler's current lexical macro / symbol-macro scope into the
   (lexical-operators . symbol-macros-ht) environment object that macroexpand-1 and
   macroexpand read (see the MACROEXPAND-1 runtime). Macro expanders built by
   defmacro/macrolet call this to supply &environment, so a macro can
   macroexpand-1 a symbol-macro that is lexically in scope at the call site;
   e.g. serapeum with-boolean's %all-branches% channel. *macros* is
   already a hash table; the symbol-macro side is keyed by symbol.
   Returns NIL when no lexical symbol-macro or operator binding is in scope
   (top-level / the common case), so a macro's (if env ...) still reads a null
   environment there."
  (let ((shadows (%symbol-macro-shadows)))
    (when (or *symbol-macros* *lexical-operators* shadows)
      (%lexical-macro-environment shadows))))

(defun %lexical-macro-environment (&optional (shadows (%symbol-macro-shadows)))
  "The (lexical-operators . symbol-macros-ht) environment for the current
   position, even when both are empty. The CAR is *LEXICAL-OPERATORS*, which
   MACROEXPAND-1 searches before the global macro tables."
  (cons *lexical-operators*
        (when (or *symbol-macros* shadows)
          ;; Shadowing variables first: %SYMBOL-MACRO-SHADOWS only returns a
          ;; variable that is the innermost binding of its name.
          (let ((ht (or (%add-symbol-macro-shadows nil shadows)
                        (make-hash-table :test #'equal))))
            ;; *symbol-macros* is innermost-first (compile-symbol-macrolet prepends
            ;; new bindings), so on nested same-name symbol-macrolet the FIRST entry
            ;; seen is the innermost: keep it and skip later (outer) shadows, matching
            ;; the assoc-based lookup the compiler uses internally (nested case).
            (dolist (entry *symbol-macros* ht)
              (%add-env-symbol-macro ht (car entry) (cdr entry)))))))

(defvar *interp-expansion-macros* nil
  "While the tree-walk evaluator runs a macro expander, the MACROLET bindings in
   scope there, as its %MINI-MACROS alist ((NAME . EXPANDER) innermost first, (NAME)
   for a local function that hides a macro); otherwise NIL. The evaluator keeps
   them lexical, out of *MACROS*, so FIND-MACRO-EXPANDER consults them first: the
   SETF family expands a place that is a local macro call through it.")

(defvar *interp-expansion-env* nil
  "The &ENVIRONMENT that goes with *INTERP-EXPANSION-MACROS*.")

(defun find-macro-expander (sym)
  "Find macro expander for SYM by symbol identity, with a runtime name bridge.
   Checks *macros* first, then (at runtime only) the runtime macro table
   (MACRO-FUNCTION) for macros that reached the C# _macroFunctions table but not
   the Lisp *macros* table.
   Runtime-table entries take (form env); we wrap them to the 1-arg convention.
   Skipped during cross-compile to avoid picking up host (SBCL) macro definitions.

   The MACRO-FUNCTION fallback used to be limited to the DOTCL package (only
   C#-registered macros like DOTCL:WITHOUT-PACKAGE-LOCKS). That left a hole:
   a macro registered at cross-compile under one symbol object can be referenced
   at runtime through a same-named symbol in a different package; e.g. a form
   whose head is DOTCL-INTERNAL::DEFINE-COMPILE-STATE built programmatically and
   EVALd. The registration only lands in _macroFunctions (keyed by that symbol),
   so the eq-keyed *macros* misses and the head would wrongly compile as a
   function call (UNDEFINED-FUNCTION / silent misbehavior). MACRO-FUNCTION finds
   it by symbol identity, so consulting it closes the gap and matches the
   cross-package bridge the function-resolution path already has
   (FindFunctionAcrossPackages).

   COMMON-LISP is excluded from the fallback: standard CL operators (FORMATTER,
   DEFUN, LOOP, CASE, DESTRUCTURING-BIND, ...) carry a no-op MACRO-FUNCTION
   bridge that returns the form UNCHANGED when no compiler macro exists
   (Runtime.Misc.cs standardMacros; the bridge exists so runtime MACROEXPAND-1
   works for code walkers). The compiler lowers those via its own handlers /
   named-call, so consulting the bridge for a CL symbol would loop forever
   (form unchanged -> re-compile -> same head) or mis-expand; this is exactly the
   trap the DEFMACRO special-case above documents. Every genuine macro missing
   from *macros* lives in another package and has a real expander. Special
   operators (WHEN/AND/OR/COND/...) never reach here anyway; they are handled by
   *compile-form-handlers* before macro expansion."
  (let ((lexical (and *interp-expansion-macros*
                      (assoc sym *interp-expansion-macros*))))
    (when lexical
      (return-from find-macro-expander
        (let ((expander (cdr lexical)) (env *interp-expansion-env*))
          (and expander (lambda (form) (funcall expander form env)))))))
  (or (gethash sym *macros*)
      (and (not *cross-compiling*)
           (symbolp sym)
           (let ((pkg (symbol-package sym)))
             (and pkg (not (string= (package-name pkg) "COMMON-LISP"))))
           (let ((mf (macro-function sym)))
             (and mf (lambda (form) (funcall mf form nil)))))))

(defun %macrolet-expander-form (params mbody)
  "Build the (lambda (form) ...) expander body for a MACROLET-defined macro with
   lambda list PARAMS and macro body MBODY, handling &whole and &environment per
   CL macro lambda-list rules. Shared by every macrolet registration site;
   compile-macrolet, the %mini-eval MACROLET case, and the three cil-analysis
   walkers; so they never diverge. The analysis copies previously inlined
   (destructuring-bind PARAMS (cdr form) ...), which omitted &whole handling and
   bound a &whole var to (cdr form) instead of the whole form; because the
   analysis pass caches its expansion in *macroexpand-cache* for compile to reuse,
   that bug surfaced even though compile-macrolet itself was correct."
  (let* ((env-arg (gensym "MLENV"))
         (whole-var (when (and (consp params) (eq (car params) '&whole)) (cadr params)))
         (rest-params (if whole-var (cddr params) params))
         (env-var nil)
         (clean-params
          (let ((result '()) (p rest-params))
            (loop
              (when (null p) (return))
              (cond
                ((eq (car p) '&environment) (setq env-var (cadr p)) (setq p (cddr p)))
                (t (push (car p) result) (setq p (cdr p)))))
            (nreverse result))))
    ;; Built inside out: the lambda list proper, then &whole around it, then
    ;; &environment around that, so &environment is bound whether or not &whole
    ;; is present.
    (let ((body `(destructuring-bind ,clean-params (cdr form) ,@mbody)))
      (cond ((and whole-var (consp whole-var))
             ;; &whole (pattern): destructure the whole form against the pattern
             (setq body `(destructuring-bind ,whole-var form ,body)))
            (whole-var
             ;; &whole var: bind var to the entire form
             (setq body `(let ((,whole-var form)) ,body))))
      (if env-var
          ;; The caller may supply the environment. The tree-walk evaluator does:
          ;; its MACROLET bindings are lexical (an entry in ENV), so they are not
          ;; in *MACROS* and an environment built from the globals here would not
          ;; show them to (MACROEXPAND x env) inside the expander. The compiler
          ;; does not; COMPILE-MACROLET registers its expanders in *MACROS* for
          ;; the extent of the body, so the globals are the right source there.
          ;;
          ;; Shape either way: (lexical-operators . symbol-macro-table), which is
          ;; what MACROEXPAND-1 destructures.
          `(lambda (form &optional ,env-arg)
             (let ((,env-var (or ,env-arg (%lexical-macro-environment))))
               ,body))
          `(lambda (form &optional ,env-arg)
             (declare (ignore ,env-arg))
             ,body)))))

(defun %eval-macrolet-expander (params mbody)
  "The expander function for a MACROLET binding of lambda list PARAMS and body
   MBODY, as the analysis walkers register it. Made with the tree-walk evaluator
   (%MINI-EVAL), the same one COMPILE-MACROLET uses for code generation, not with
   EVAL. An expander runs a handful of times while one form is compiled, so
   compiling it cost far more than running it: EVAL built and JITted a method per
   MACROLET binding the walk met, thousands for a library whose macros expand into
   MACROLET. It is interpreted in the compiler's lexical environment, so a closure
   the expander returns still sees the enclosing MACROLET / SYMBOL-MACROLET
   bindings.

   While cross-compiling the host's EVAL is kept: the evaluator would expand the
   expander body with the host's macros (DESTRUCTURING-BIND into host internals
   it cannot call). That EVAL runs with *CROSS-COMPILING* off, because the
   expander is host code: its quoted constants must stay the very objects the
   reader produced. Under *CROSS-COMPILING* the dotcl compiler instead writes a
   quoted symbol out by name and interns it again on load, so an expansion that
   names an enclosing FLET function or lexical variable came back as a different
   symbol and missed the binding."
  (let ((form (%macrolet-expander-form params mbody)))
    (if *cross-compiling*
        (let ((*cross-compiling* nil))
          (eval form))
        (%expander-with-caller-env
         (%mini-eval form (%compile-scope-mini-env)) params))))

(defun %expander-with-caller-env (fn params)
  "FN, an expander %MINI-EVAL made from %MACROLET-EXPANDER-FORM, called with the
   &ENVIRONMENT of the place it expands when PARAMS has an &ENVIRONMENT parameter.
   The expander builds that environment itself from *SYMBOL-MACROS* when it is not
   given one, but an interpreted closure rebinds *SYMBOL-MACROS* to what it was
   where the closure was made (the MACROLET form), so the expander saw none of the
   SYMBOL-MACROLET bindings around the macro call: (MACROEXPAND X ENV) of a
   symbol macro X bound there gave X back. Building the environment here, outside
   the closure, takes it from the compiler's scope at the call."
  (if (loop for p on params thereis (and (consp p) (eq (car p) '&environment)))
      (lambda (form &optional env)
        (funcall fn form (or env (%lexical-macro-environment))))
      fn))

(defun cstate-dotnet-typed-locals ()
  "Alist (var-name-string type-name-string . local-key) of lexical locals whose
   value is statically known to be a .NET object of a specific type, inferred
   from a let/let* init form (DOTNET:NEW / DOTNET:BOX / (THE (DOTNET \"T\") ...)).
   Bound by COMPILE-LET around the body; only non-mutated, non-special bindings
   qualify. The DOTNET:INVOKE compiler macro reads the filtered view (see
   DOTNET-VALID-TYPED-LOCALS) to lower a call on such a variable to a typed direct
   callvirt. The recorded LOCAL-KEY is verified against the current *LOCALS* so an
   inner rebinding of the same name shadows it (no stale type -> no miscompile)."
  (svref *cstate* +cs-dotnet-typed-locals+))

(defun dotnet-valid-typed-locals ()
  "Return *DOTNET-TYPED-LOCALS* as a plain (name-string . type-string) alist,
   keeping only entries whose variable still resolves to the exact lexical local
   it was typed for. An intervening binding (inner let, lambda param, dolist var,
   ...) maps the name to a different key, so the entry is dropped and the call falls
   back to the dynamic path. Conservative: when in doubt, omit."
  (let ((result '()))
    (dolist (e (cstate-dotnet-typed-locals))
      ;; NAME is a VAR-NAME (INFER-DOTNET-TYPED-BINDINGS stores one), so resolve
      ;; it with the same rule: a gensym-bound typed local is a real binding,
      ;; and the (EQ CUR KEY) check below is what makes the entry safe to keep.
      (let* ((name (car e)) (type (cadr e)) (key (cddr e))
             (cur (cdr (local-entry-by-name name (cstate-locals)))))
        (when (eq cur key)
          (push (cons name type) result))))
    result))

;; Probed by name at load time, like *KNOWN-TYPE-NAME-P-FN*: the proclamation
;; lives on the runtime's symbol and the SBCL cross-compile host has neither the
;; DOTCL package nor the flag. Defined here rather than beside its sibling
;; probes because the cross-compile compiles the core in segments, and a use
;; that precedes its definition is reported as undefined at segment end.
(defvar *global-notinline-p-fn*
  (ignore-errors
   (let ((sym (find-symbol "GLOBAL-NOTINLINE-P" "DOTCL")))
     (and sym (fboundp sym) (symbol-function sym)))))

(defun %global-notinline-p (sym)
  "Was SYM proclaimed NOTINLINE globally? NIL on the cross-compile host, which
   carries no proclamations of its own."
  (and *global-notinline-p-fn* (funcall *global-notinline-p-fn* sym)))

(defun function-return-type (name)
  "The return type a call to the global function NAME may be compiled against,
   or NIL. A declaimed ftype always answers. An inferred one answers only for a
   recursive call from NAME's own body, or for a call in the same unit
   (COMPILE-FILE, or the core build) as the definition it was inferred from,
   and not when NAME is NOTINLINE (CLHS 3.2.2.3). The caller must still rule out
   a local function of the same name."
  (multiple-value-bind (ty found) (gethash name *function-return-types*)
    (when found
      (multiple-value-bind (unit inferred) (gethash name *inferred-return-type-names*)
        (cond ((not inferred) ty)
              ((or (member name *notinline-functions*)
                   (%global-notinline-p name))
               nil)
              ((eq name *return-type-self*) ty)
              ((%in-return-type-unit-p unit) ty)
              (t nil))))))

(defun record-inferred-return-type (name type)
  "Record TYPE (NIL: nothing could be inferred) as the inferred return type of
   the DEFUN of NAME being compiled, replacing an earlier inferred one: each
   definition answers for itself. A declaimed entry is left alone. When the
   same type is re-inferred outside any unit (COMPILE-FILE evaluating the same
   DEFUN at compile time), the unit it was first recorded in is kept."
  (multiple-value-bind (old found) (gethash name *function-return-types*)
    (multiple-value-bind (old-unit inferred) (gethash name *inferred-return-type-names*)
      (cond ((and found (not inferred)) nil)
            ((null type)
             (remhash name *function-return-types*)
             (remhash name *inferred-return-type-names*))
            (t
             (let ((unit (%return-type-unit)))
               (setf (gethash name *function-return-types*) type)
               (setf (gethash name *inferred-return-type-names*)
                     (if (and (null unit) found (equal old type)) old-unit unit))))))))

(defvar *global-inline-p-fn*
  (ignore-errors
   (let ((sym (find-symbol "GLOBAL-INLINE-P" "DOTCL")))
     (and sym (fboundp sym) (symbol-function sym)))))

(defun %global-inline-p (sym)
  "Was SYM proclaimed INLINE globally? NIL on the cross-compile host, so the
   core is never built with inline substitution; the host has no
   proclamations, and keeping cross-compile output identical to the runtime
   compiler's is worth more here than the speed."
  (and *global-inline-p-fn* (funcall *global-inline-p-fn* sym)))

(defun %form-size (form &optional (limit 1000000))
  "Cons count of FORM, stopping once LIMIT is exceeded (returns LIMIT then).
   Bounded so a circular constant inside a body cannot hang the size check."
  (let ((n 0))
    (labels ((walk (x)
               (when (and (consp x) (< n limit))
                 (incf n)
                 (walk (car x))
                 (walk (cdr x)))))
      (walk form))
    n))

(defun %inline-body-capturable-p (body params)
  "True when BODY, placed at the current lexical position, could have one of its
   free names bound by something the text of BODY does not show.

   Used at both ends of inline substitution. At a call site, the body was
   recorded where no lexical binding captured its free names, so they mean the
   GLOBAL ones; dropped verbatim inside a caller's FLET/LABELS, MACROLET or
   SYMBOL-MACROLET of the same name they would silently mean the caller's local
   binding instead, which is the classic inline-hygiene bug and a wrong ANSWER,
   not just wrong speed. At the DEFUN, a body that uses a name bound by an
   enclosing MACROLET (a top-level MACROLET around a group of DEFUNs is common)
   cannot be recorded at all: substituted anywhere outside that MACROLET, the
   local macro would compile as a call to an undefined function.

   Only the names that actually appear in BODY matter, so a MACROLET that binds
   unrelated names neither blocks the substitution nor the recording. An active
   macro scope whose names are not known here (none of the lists below carries
   any) refuses outright, as before.

   PARAMS are excluded because the expansion's own lambda binds them."
  (let ((locals (cstate-local-functions)))
    (cond ((and *macroexpand-scope*
                (null *macrolet-shadowed*)
                (null *symbol-macros*))
           t)
          ((not (or locals *macrolet-shadowed* *symbol-macros*))
           nil)
          (t
           (let ((hit nil))
             (labels ((walk (x)
                        (cond (hit nil)
                              ((symbolp x)
                               (when (and x (not (member x params))
                                          (or (member x *macrolet-shadowed*)
                                              (assoc x *symbol-macros*)
                                              (and locals (local-function-entry x))))
                                 (setf hit t)))
                              ((consp x)
                               (unless (eq (car x) 'quote)
                                 (walk (car x))
                                 (walk (cdr x)))))))
               (walk body))
             hit)))))

(defun maybe-expand-inline (op expr)
  "If OP names a function proclaimed INLINE whose definition was recorded, return
   the call EXPR rewritten as an immediately-applied lambda; else NIL.

   (let ((p1 arg1) (p2 arg2) ...) decls... (block op body...)) is the expansion.
   For an all-required lambda list this is exactly what applying the function
   means: the inits are evaluated left to right in the caller's scope, then the
   parameters are bound. Everything else; capture, RETURN-FROM, declarations
   (including a SPECIAL one, which makes the LET binding dynamic just as it
   would a parameter); is then the existing LET machinery's problem.

   NOT ((lambda (params) ...) args): dotcl does not beta-reduce an immediately
   applied lambda, it builds a function object and calls it, so that shape
   replaces the call with a call plus an allocation.

   The leading declarations have to stay OUTSIDE the block; they belong to the
   binding form's body, and (block name (declare ...)) is not a declaration
   position at all. Getting this wrong drops every parameter type declaration,
   which is exactly what the functions people declaim inline are carrying.

   Refused when:
   - a local flet/labels/macrolet binds OP: that is a different function
   - NOTINLINE is in scope for OP, lexically or globally (CLHS 3.2.2.1.1)
   - OP is already being inlined (recursion would not terminate)
   - the argument count does not match the required parameters, or the lambda
     list has any non-required parameter (an &optional/&key/&rest expansion has
     to replicate defaulting rules; left to the normal call for now)
   - the body is larger than *INLINE-BODY-SIZE-LIMIT*
   - a local function binding or an active MACROLET at the call site could
     capture one of the body's free names (see %INLINE-BODY-CAPTURABLE-P)

   Every refusal simply leaves a normal function call, so none of them can be
   wrong; only slower."
  (when (and (not *cross-compiling*)
             (symbolp op)
             (not (local-function-entry op))
             (not (member op *notinline-functions*))
             (not (member op *inlining-stack*))
             (not (%global-notinline-p op)))
    (let ((def (gethash op *inline-defs*)))
      (when def
        (let ((params (car def))
              (body (cdr def))
              (args (cdr expr)))
          (when (and (every #'symbolp params)
                     (notany (lambda (p) (member p lambda-list-keywords)) params)
                     (= (length args) (length params))
                     (< (%form-size body (1+ *inline-body-size-limit*))
                        (1+ *inline-body-size-limit*))
                     (not (%inline-body-capturable-p body params))
                     ;; The global proclamation is asked about last: it crosses
                     ;; into the runtime, and everything above is cheap.
                     (%global-inline-p op))
            (let ((decls '())
                  (rest body))
              (loop while (and (consp rest) (consp (car rest))
                               (eq (caar rest) 'declare))
                    do (push (pop rest) decls))
              `(let ,(mapcar #'list params args)
                 ,@(nreverse decls)
                 (block ,op ,@rest)))))))))

;;; --- Inlining a LOCAL function: (declare (inline f)) in an FLET/LABELS ---
;;;
;;; The declaration was read and thrown away. Honouring it buys more than the
;;; call it saves: a local function is compiled as its own .NET method, so a
;;; RETURN-FROM out of it -- to a block outside, the way a scanner gives up --
;;; leaves that method, and leaving a .NET frame is only possible by throwing.
;;; Unwinding costs about 288 bytes per frame crossed. Substituted into the
;;; caller, the identical RETURN-FROM is a LEAVE and costs nothing: writing the
;;; function as a MACROLET instead already measured 0 bytes where the local
;;; function measured 608.
;;;
;;; The substitution has the same shape as the global one (MAYBE-EXPAND-INLINE):
;;; (let ((p arg) ...) decls... (block f body...)). Hygiene is where the two
;;; differ. A body proclaimed inline globally was written at top level, so only
;;; a binding at the CALL site can capture it. A local function's body was
;;; written in the middle of a lexical environment and means the bindings that
;;; were in scope THERE; moved elsewhere in that same environment, every name it
;;; uses must still mean the same binding.
;;;
;;; So the plan records, for every symbol the body mentions, what that symbol
;;; resolved to where the function was written -- variable, local function,
;;; block, go tag, symbol macro -- and a call site substitutes only when all of
;;; them still resolve identically (EQ on the binding entry, not on the name).
;;; Anything that rebinds one of those names in between refuses, and a refusal
;;; is an ordinary call. The walk is deliberately blunt: every symbol, not the
;;; free ones, so a name the body binds itself can refuse too. Over-refusing
;;; costs speed; under-refusing would be a wrong answer.
;;;
;;; Two cases fall out of that rule rather than needing their own: a body that
;;; calls itself (the name is bound at the call site and was not where the body
;;; was written, so it never matches), and a call from inside a closure (the
;;; closure boundary resets the locals, so any free variable refuses).

(defvar *local-inline-body-size-limit* 500
  "Maximum cons count of a local function body that will be substituted at a
   call site. Much more generous than *INLINE-BODY-SIZE-LIMIT*: a global
   proclamation is a standing order about every call to a name, while
   (declare (inline f)) inside an FLET names one function in one scope, written
   by someone looking at it. cl-ppcre's ADVANCE-FN, the case this was built for,
   is a few hundred conses.")

(defvar *local-inline-call-limit* 4
  "How many call sites one local function binding may be substituted into. The
   body is copied at each one, so N sites cost N copies of it; the declaration
   asks for the substitution but says nothing about how much code it is worth.")

(defun %local-inline-bindings (sym)
  "Everything SYM could name in the current scope: variable, local function,
   block, go tag, symbol macro. The tuple a local inline plan compares, so the
   list of things a substitution has to keep meaning has one definition."
  (list (local-entry sym)
        (local-function-entry sym)
        (assoc sym (cstate-block-tags))
        (assoc sym (cstate-go-tags))
        (assoc sym *symbol-macros*)))

(defun %local-inline-snapshot (body params)
  "For every symbol BODY mentions other than PARAMS, what it resolves to here.
   PARAMS are excluded because the expansion's own LET binds them. QUOTE
   subforms are skipped: a quoted symbol names no binding."
  (let ((seen '())
        (result '()))
    (labels ((walk (x)
               (cond ((and x (symbolp x))
                      (unless (or (member x params :test #'eq)
                                  (member x seen :test #'eq))
                        (push x seen)
                        (push (cons x (%local-inline-bindings x)) result)))
                     ((consp x)
                      (unless (eq (car x) 'quote)
                        (walk (car x))
                        (walk (cdr x)))))))
      (walk body))
    result))

(defun %local-inline-env-matches-p (snapshot)
  "True when every symbol in SNAPSHOT still resolves to exactly the binding it
   resolved to where the function was written."
  (every (lambda (entry)
           (every #'eq (%local-inline-bindings (car entry)) (cdr entry)))
         snapshot))

(defvar *local-inline-report* nil
  "When true, every decision about substituting a local function is printed to
   *ERROR-OUTPUT*: the ones taken, and for the ones refused, which names no
   longer mean what they meant where the body was written.

   An INLINE declaration that quietly does nothing is otherwise invisible --
   the program is correct either way, and only the allocation says which
   happened -- so this is how to ask whether a declaration took.")

(defun %local-inline-note (op ok snapshot)
  "Report one substitution decision when *LOCAL-INLINE-REPORT* asks for it."
  (when *local-inline-report*
    (if ok
        (format *error-output* "~&; inlined local ~s~%" op)
        (format *error-output* "~&; local ~s not inlined here~@[ (rebound: ~s)~]~%"
                op
                (loop for entry in snapshot
                      unless (every #'eq (%local-inline-bindings (car entry))
                                    (cdr entry))
                        collect (car entry))))))

(defun maybe-expand-local-inline (op expr)
  "If OP names a local function whose FLET/LABELS declared it INLINE, and its
   body still means here what it meant where it was written, return the call
   EXPR rewritten as a LET over that body; else NIL.

   Refused when the argument count does not match, when the budget of call sites
   is spent, when a MACROLET scope is active that was not active at the
   definition, when OP is NOTINLINE here, when OP is already being substituted,
   and -- the one that does the work -- when any name the body mentions resolves
   to a different binding at this call site. Every refusal leaves an ordinary
   call, so none of them can be wrong, only slower."
  (let ((plan (nth 5 (local-function-entry op))))
    (when (and plan
               (not (member op *inlining-stack*))
               (not (member op *notinline-functions*)))
      (destructuring-bind (name params decls body blockp snapshot scope budget) plan
        (let ((args (cdr expr))
              (ok nil))
          (when (and (= (length args) (length params))
                     (plusp (car budget))
                     (eq scope *macroexpand-scope*)
                     (setq ok (%local-inline-env-matches-p snapshot)))
            (decf (car budget)))
          (%local-inline-note op ok snapshot)
          (when ok
            ;; NAME, not OP: entries are matched by mangled name, so the call
            ;; could spell the function with a symbol from another package,
            ;; while the body's RETURN-FROM names the defining symbol.
            `(let ,(mapcar #'list params args)
               ,@decls
               ,@(if blockp
                     `((block ,name ,@body))
                     body))))))))


(defun maybe-expand-compiler-macro (op expr)
  "If OP names a compiler macro and isn't shadowed by a local function, apply it to
   the call form EXPR (CLHS 3.2.2.1). Return the expansion, or NIL when there is no
   compiler macro, it declines (returns the &whole form unchanged), or OP is shadowed.
   Skipped during cross-compile so we never pick up the host (SBCL) compiler macros."
  (when (and (not *cross-compiling*)
             (symbolp op)
             (not (local-function-entry op))
             ;; CLHS 3.2.2.1.1: a NOTINLINE declaration of OP in scope disables
             ;; its compiler macro for calls in that scope.
             (not (member op *notinline-functions*)))
    (let ((expander (compiler-macro-function op)))
      ;; The global proclamation is asked about only once a compiler macro is
      ;; known to exist: the lexical list above is checked for every call form,
      ;; but this crosses into the runtime, and a call with no compiler macro
      ;; has nothing to suppress anyway.
      (when (and (functionp expander) (not (%global-notinline-p op)))
        ;; Expander takes (whole-form environment). dotcl passes the static .NET
        ;; type environment (an alist of locals with a known .NET type) as the
        ;; environment so DOTNET:INVOKE can lower a call on such a variable to a
        ;; typed direct callvirt. NIL when no such locals are in scope: the
        ;; common case, and what every user-defined compiler macro receives
        ;; (define-compiler-macro ignores its environment argument). Returning the
        ;; original form (eq) is the standard way to decline expansion.
        ;; A call whose arguments do not fit the compiler macro's lambda list
        ;; makes the expander signal a PROGRAM-ERROR. As in SBCL, that is
        ;; reported as a warning and the form is compiled as an ordinary call
        ;; (a compiler macro is only ever an optional rewrite, CLHS 3.2.2.1),
        ;; so the function itself gets to report the call, or accept it.
        (let ((expansion
                (handler-case (funcall expander expr
                                       (and (cstate-dotnet-typed-locals)
                                            (dotnet-valid-typed-locals)))
                  (program-error (e)
                    (warn "Error during compiler-macroexpansion of ~s: ~a" expr e)
                    expr))))
          (unless (eq expansion expr) expansion))))))

(defun compile-form (expr)
  "Compile a list form (op args...).
   Dispatch: quote fast-path -> flet-override check -> hash table (O(1), ~250 ops)
   -> cons-op cases -> string=-based ops -> macro expansion -> named-call."
  ;; Guard to catch runaway macro expansion loops. Raised to 500 from
  ;; the original 200 because legitimate large literal quasiquote forms
  ;; (e.g. cl-durian's 63-entry entity-map alist) can exceed 200 naturally,
  ;; while real infinite expansion loops still trip well before 500.
  ;; With the 256MB runtime stack (Program.Main) this is safe.
  (when (> *compile-depth* 500)
    (error (format nil "Compile depth limit exceeded at depth ~D, form head: ~S"
                   *compile-depth* (if (consp expr) (car expr) expr))))
  (let ((*compile-depth* (1+ *compile-depth*))
        (op (car expr))
        (*compile-was-toplevel* *at-toplevel*)
        (*at-toplevel* nil))
    (when *dotnet-type-references* (%note-dotnet-type-references expr))
    ;; in a project-core CONCATENATED build, a toplevel module/package
    ;; setup form must take effect at COMPILE time so later components in the
    ;; same concatenated unit see the macros / package use-list it establishes
    ;; (normal multi-file load-op gets this from compile+load interleaving).
    ;; Same SBCL-compat rationale as the defun ct-eval. try-eval compiles
    ;; to Runtime.TryEval, which rebinds *compile-file-mode* to NIL during the
    ;; eval: so the inner compile does not re-enter here (the gate needs both
    ;; *compile-file-mode* and *concatenate-build*). Gated to concat builds only:
    ;; ordinary user compile-file keeps standard CL semantics.
    ;; Compare by symbol-name (string=), not eq on a quoted literal: symbols
    ;; baked into this list at cross-compile time are not eq to the ones the
    ;; runtime reader interns for user code (Startup.Sym vs Reader identity).
    ;; Same idiom as compile-eval-when's defvar/defparameter list.
    (when (and *compile-was-toplevel* *compile-file-mode* *concatenate-build*
               (not *cross-compiling*)
               (symbolp op)
               (member (symbol-name op)
                       '("REQUIRE" "LOAD" "USE-PACKAGE" "PROVIDE"
                         "IMPORT" "SHADOW" "SHADOWING-IMPORT")
                       :test #'string=))
      (try-eval expr))
    ;; quote fast path
    (when *labels-demoted* (%labels-demoted-check op))
    (if (eq op 'quote)
        (compile-quoted-cached (cadr expr))
      ;; Local flet/labels function override (shadowing built-ins):
      ;; Must come before hash dispatch so flet can shadow built-in functions.
      ;; CL special operators must never be shadowed by flet: explicitly excluded.
      ;; Not when a MACROLET inside the FLET rebinds the name as a macro.
      (if (and (symbolp op)
               (not (special-operator-name-p op))
               (local-function-entry op)
               (not (%lexical-macro-p op)))
          ;; An INLINE declaration in the binding form substitutes the body
          ;; here instead of calling it, when the body still means the same
          ;; thing at this call site (MAYBE-EXPAND-LOCAL-INLINE).
          (let ((inl (maybe-expand-local-inline op expr)))
            (if inl
                (let ((*inlining-stack* (cons op *inlining-stack*)))
                  (compile-expr inl))
                (compile-named-call op (cdr expr))))
        ;; Hash table dispatch: O(1) for all registered ops (~250 cases).
        ;; *compile-was-toplevel* is already bound above; handlers use it directly.
        ;; A MACROLET binding for a handler name takes precedence: the handler
        ;; table lowers several CL macros (WHEN, AND, ...) as if they were special
        ;; forms, but they remain macros the standard lets MACROLET shadow. With
        ;; the handler skipped, the fallback below finds the local expander.
        (let ((handler (and (symbolp op)
                            (not (macrolet-shadowed-p op))
                            (gethash op *compile-form-handlers*))))
          (if handler
              (funcall handler expr)
            ;; Fallback: cons-op cases, string=-based ops, macro expansion, named-call
            (cond
              ;; Direct dispatch for a known-typed dotnet:invoke (typed direct callvirt).
              ;; (%dotnet-call-direct "Type.FullName" "Method" (param-type-strings...)
              ;;                      recv-expr arg-expr...)
              ;; emits unwrap + per-arg marshal + direct callvirt to the resolved
              ;; overload (no InvokeMember). Dispatched by NAME to avoid cross-package
              ;; symbol identity fragility for this internal lowering target.
              ((and (symbolp op) (string= (symbol-name op) "%DOTNET-CALL-DIRECT"))
               (let ((type (cadr expr)) (method (caddr expr))
                     (param-types (cadddr expr)) (recv (nth 4 expr)) (args (nthcdr 5 expr)))
                 (if (null args)
                     ;; zero-arg fast path: receiver only, no args array
                     `(,@(compile-for-single-value recv)
                       (:dotnet-call-direct ,type ,method))
                     ;; n-arg: evaluate the receiver and each argument into its own
                     ;; local (each with an empty stack: CIL try-block safety), then
                     ;; the typed direct call marshals straight from those locals. No
                     ;; LispObject[] is allocated (per-arg-local codegen).
                     (let ((recv-tmp (gen-local "DRCV"))
                           (arg-tmps (loop for a in args collect (gen-local "DARG"))))
                       `((:declare-local ,recv-tmp "LispObject")
                         ,@(compile-for-single-value recv)
                         (:stloc ,recv-tmp)
                         ,@(loop for arg in args
                                 for tmp in arg-tmps
                                 append `((:declare-local ,tmp "LispObject")
                                          ,@(let ((*in-tail-position* nil)
                                                  (*in-mv-context* nil))
                                              (compile-expr arg))
                                          (:stloc ,tmp)))
                         (:dotnet-call-direct-locals ,type ,method ,recv-tmp
                                                     ,arg-tmps ,param-types))))))
              ;; (setf name) function call: ((setf foo) args...) -> named call
              ((and (consp op) (symbolp (car op)) (string= (symbol-name (car op)) "SETF"))
               (compile-named-call op (cdr expr)))
              ;; ((lambda ...) args): immediate lambda application
              ;; Store the function to a local before evaluating args so the stack is
              ;; empty during arg evaluation. CIL requires empty stack at try-block entry;
              ;; loop/return in args would fail if the function is on the stack.
              ((and (consp op) (eq (car op) 'lambda))
               (%check-lambda-call-keywords op (cdr expr))
               (let ((fn-tmp (gen-local "FN")) (arr-tmp (gen-local "FNARR")))
                 `(,@(compile-expr op)
                   (:castclass "LispFunction")
                   (:declare-local ,fn-tmp "LispFunction") (:stloc ,fn-tmp)
                   ,@(compile-args-array (cdr expr))
                   (:declare-local ,arr-tmp "LispObject[]") (:stloc ,arr-tmp)
                   (:ldloc ,fn-tmp) (:ldloc ,arr-tmp)
                   (:callvirt "LispFunction.Invoke"))))
              ;; ((declare ...) body...): treat as (locally (declare ...) body...)
              ((and (consp op) (eq (car op) 'declare))
               (compile-expr `(locally ,op ,@(cdr expr))))
              ;; Any other Cons op: compile the op expression (expects it to
              ;; evaluate to a function designator) and funcall it with args.
              ;; CLHS is strict here but SBCL accepts forms like ((quote =) nil)
              ;; by evaluating `(quote =)` as a function designator. binfix /
              ;; series / similar generate such forms; deferring to runtime
              ;; (where the type error will surface if the designator is wrong)
              ;; matches SBCL's behavior better than rejecting at compile time.
              ((consp op)
               (let ((fn-tmp (gen-local "FN")) (arr-tmp (gen-local "FNARR")))
                 `(,@(compile-expr op)
                   (:call "Runtime.CoerceToFunction")
                   (:declare-local ,fn-tmp "LispFunction") (:stloc ,fn-tmp)
                   ,@(compile-args-array (cdr expr))
                   (:declare-local ,arr-tmp "LispObject[]") (:stloc ,arr-tmp)
                   (:ldloc ,fn-tmp) (:ldloc ,arr-tmp)
                   (:callvirt "LispFunction.Invoke"))))

              ;; Operator is neither a symbol nor a cons, e.g. (0 1 2). This shows
              ;; up when a macro definition was not loaded and one of its unquoted
              ;; literal arguments is read as a call form. Defer the diagnosis to
              ;; run time (like SBCL, so the rest of the file still compiles)
              ;; instead of letting the SYMBOL-NAME dispatch below signal an
              ;; opaque "0 is not of type SYMBOL" type error at compile time.
              ((not (symbolp op))
               (compile-static-program-error
                (let ((*print-length* 6) (*print-level* 3))
                  (format nil "Illegal function call: ~S" expr))))

              ;; String=-based dispatch for ops that may arrive from different packages.
              ;; These are rare (internal/cross-package ops): not in the eq hash table.
              ((string= (symbol-name op) "TRY-EVAL") (compile-unary-call (cdr expr) "Runtime.TryEval"))
              ;; Spill args to temps (compile-ternary-call): the value form may be
              ;; a try-based NLX (block/return, loop) and CIL forbids entering a
              ;; try region with a non-empty stack. Compiling the args inline left
              ;; the array+index on the stack while the value's try was entered,
              ;; producing unverifiable IL (e.g. (setf (svref s j) (block nil ...))).
              ((string= (symbol-name op) "%SET-CHAR")
               (compile-ternary-call (cdr expr) "Runtime.SetChar"))
              ((string= (symbol-name op) "%SET-ELT")
               (compile-ternary-call (cdr expr) "Runtime.SetElt"))
              ((string= (symbol-name op) "%SET-SUBSEQ")
               (compile-named-call '%set-subseq (cdr expr)))
              ((string= (symbol-name op) "%PUTF")
               (compile-ternary-call (cdr expr) "Runtime.Putf"))
              ((and (string= (symbol-name op) "CHAR=") (= (length (cdr expr)) 2))
               (compile-binary-call (cdr expr) "Runtime.CharEqual"))
              ((string= (symbol-name op) "%MAKE-PACKAGE") (compile-unary-call (cdr expr) "Runtime.MakePackage"))
              ((string= (symbol-name op) "%PACKAGE-USE") (compile-binary-call (cdr expr) "Runtime.PackageUse"))
              ((string= (symbol-name op) "%PACKAGE-EXPORT") (compile-binary-call (cdr expr) "Runtime.PackageExport"))
              ((string= (symbol-name op) "%PACKAGE-IMPORT") (compile-binary-call (cdr expr) "Runtime.PackageImport"))
              ((string= (symbol-name op) "%PACKAGE-SHADOW") (compile-binary-call (cdr expr) "Runtime.PackageShadow"))
              ((string= (symbol-name op) "%PACKAGE-NICKNAME") (compile-binary-call (cdr expr) "Runtime.PackageNickname"))
              ((string= (symbol-name op) "%UNEXPORT") (compile-binary-call (cdr expr) "Runtime.UnexportSymbol"))
              ((string= (symbol-name op) "%UNUSE-PACKAGE") (compile-binary-call (cdr expr) "Runtime.UnusePackage"))
              ((string= (symbol-name op) "%SHADOWING-IMPORT") (compile-binary-call (cdr expr) "Runtime.ShadowingImport"))
              ((string= (symbol-name op) "%PACKAGE-EXTERNAL-SYMBOLS")
               (compile-unary-call (cdr expr) "Runtime.PackageExternalSymbolsList"))
              ((string= (symbol-name op) "%PACKAGE-ALL-SYMBOLS")
               (compile-unary-call (cdr expr) "Runtime.PackageAllSymbolsList"))
              ((string= (symbol-name op) "INTERN")
               `(,@(compile-args-array (cdr expr))
                 (:call "Runtime.InternSymbolV")))
              ;; defmacro: string= match to catch cross-package variants (e.g. SB-XC:DEFMACRO),
              ;; but only when the variant has no macro definition of its own.
              ;; SBCL's host build defines SB-XC:DEFMACRO as a REAL host macro
              ;; (src/code/defmacro.lisp) whose expansion registers the macro in
              ;; the cross-compiler's own env; hijacking it here would instead
              ;; register e.g. the XC's DEFUN into dotcl's macro table under
              ;; CL:DEFUN, so host-compiling (defun ...) later in the same build
              ;; runs the XC's defun-expander (%compiler-defun reads the unbound
              ;; *IR1-NAMESPACE*). This DEFMACRO special-case remains only as the
              ;; fallback for variants with no registered expander (whose bridge
              ;; macro-function would loop forever returning the form unchanged).
              ((and (symbolp op) (string= (symbol-name op) "DEFMACRO")
                    (or (eq op 'defmacro)
                        (not (find-macro-expander op))))
               ;; CLHS 3.4.11: extract docstring (first form if string AND more forms follow).
               ;; Skip during cross-compile (bootstrap concern, see compile-defun handler).
               (let* ((name (cadr expr))
                      (lambda-list (caddr expr))
                      (body (cdddr expr))
                      (has-docstring (and (not *cross-compiling*)
                                          (consp body) (stringp (car body)) (cdr body)))
                      (docstring (when has-docstring (car body)))
                      (real-body (if has-docstring (cdr body) body))
                      (defm-instrs (compile-defmacro name lambda-list real-body)))
                 (if has-docstring
                     `(,@defm-instrs
                       (:pop)
                       ,@(compile-and-pop
                           `(funcall #'(setf documentation) ,docstring ',name 'function))
                       ,@(compile-sym-lookup name))
                     defm-instrs)))

              ;; Macro expansion (after string= checks, before named-call).
              ;; Preserves top-level-ness per CLHS 3.2.3.1.
              ((and (symbolp op)
                    (find-macro-expander op)
                    (or (not (local-function-entry op))
                        (%lexical-macro-p op)))
               (let* ((expander (find-macro-expander op))
                      (expanded (%macroexpand-for-compile expr expander))
                      (*at-toplevel* *compile-was-toplevel*)
                      (*mv-expect-args* (%mv-expect-for expr expanded)))
                 (compile-expr expanded)))

              ;; General function call (user-defined): apply a compiler macro first
              ;; (CLHS 3.2.2.1) when one is defined and the operator isn't shadowed by
              ;; a local function; otherwise compile the call directly.
              ((symbolp op)
               (let ((cm-expansion (maybe-expand-compiler-macro op expr)))
                 (if cm-expansion
                     (let ((*at-toplevel* *compile-was-toplevel*)
                           (*mv-expect-args* (%mv-expect-for expr cm-expansion)))
                       (compile-expr cm-expansion))
                     ;; No compiler macro: an INLINE proclamation may substitute
                     ;; the definition instead. A user compiler macro wins: it
                     ;; is the more specific instruction about this call.
                     (let ((inl (maybe-expand-inline op expr)))
                       (if inl
                           (let ((*inlining-stack* (cons op *inlining-stack*))
                                 (*at-toplevel* nil))
                             (compile-expr inl))
                           (compile-named-call op (cdr expr)))))))

              (t (error "Cannot compile form: ~s" expr)))))))))



;;; REMOVED: The ~250 (eq op 'foo) cond branches that previously followed here
;;; have been moved to *compile-form-handlers* hash table in cil-forms.lisp.
;;; See the handler registration block at the bottom of cil-forms.lisp.

;;; ============================================================
;;; Arithmetic
;;; ============================================================

;;; ============================================================
;;; Fixnum-typed expression detection & unboxed long arithmetic
;;;
;;; When the compiler can statically prove an expression produces a fixnum
;;; (via (the fixnum E) wrappers or recursive fixnum ops), it emits native
;;; int64 arithmetic instead of Runtime.Add / Runtime.Subtract method calls.
;;; Result is boxed via Fixnum.Make only at the outermost boundary, so
;;; intermediate values stay on the evaluation stack as raw longs.
;;;
;;; Trigger: fixnum-typed-p => all operands statically fixnum => long path.
;;; Overflow safety: the raw int64 path is only taken when a static value-range
;;; analysis (expr-int-range) proves every intermediate +/-/*/1+/1- result fits
;;; in int64. dotcl's fixnum is full int64 (most-positive-fixnum = 2^63-1) with no
;;; tag headroom, so bare (declare (fixnum ...)) operands have the full int64 range
;;; and nested arithmetic over them is NOT provably safe: it falls back to the
;;; boxed promoting path (Runtime.Add/Subtract/Multiply) which yields a bignum on
;;; overflow. Tightly-declared operands ((the (integer lo hi) ...)) keep the fast
;;; unboxed path when their composed range stays in int64.
;;; ============================================================

;;; int64 bounds, as exact host integers.
(defconstant +int64-min+ -9223372036854775808)
(defconstant +int64-max+ +9223372036854775807)

(defun range-fits-int64-p (r)
  "True if range R = (lo . hi) lies entirely within int64."
  (and r (<= +int64-min+ (car r)) (<= (cdr r) +int64-max+)))

(defun small-int-local-range (expr)
  "If EXPR is a reference to a small-int local (see CSTATE-SMALL-INT-LOCALS) that is a
   plain non-captured slot, return its proven (LO . HI) range; else NIL. These
   slots hold a boxed Fixnum, so compile-as-long can unbox them inline."
  (and (symbolp expr)
       (boundp '*cstate*)
       (not (boxed-var-p expr))
       (let ((key (lookup-local expr)))
         (and key
              ;; Slot-keyed, like the native tables: the proven range belongs to
              ;; the binding, so an inner binding of the same name resolves to a
              ;; different slot and never inherits it. (It used to be name-keyed,
              ;; and an inner (let ((f 3))) handed its integer range to an outer
              ;; F holding a native double: native multiply on an r8 slot.)
              (cdr (assoc key (cstate-small-int-locals) :test #'equal))))))

;;; ------------------------------------------------------------
;;; Numeric-backed array locals: let bindings whose init is a make-array
;;; with a statically-known rank and a bounded-integer :element-type that
;;; the runtime stores unboxed (byte[]/ushort[]/int[]/long[]). aref on such
;;; a local can read/write the element as a raw int64 (Runtime.ArefNum*L),
;;; and the element's storage range feeds expr-int-range so surrounding
;;; arithmetic can prove int64-safety and stay native.
;;; ------------------------------------------------------------

(defun %numeric-storage-range (spec)
  "Storage range (LO . HI) of the unboxed backing the runtime picks for
   element-type SPEC, or NIL when SPEC gets no numeric backing. Must mirror
   the runtime upgrade (ParseElementTypeName + NumKindForElementType):
   [0,1] upgrades to bit-packed BIT (not numeric), [0,255]->u8,
   [0,65535]->u16, int32->i32, int64->i64, wider (e.g. (unsigned-byte 64))
   stays boxed."
  (if (eq spec 'fixnum)
      (cons +int64-min+ +int64-max+)
      (let ((r (integer-type-range spec)))
        (and r
             (let ((lo (car r)) (hi (cdr r)))
               (cond
                 ((and (<= 0 lo) (<= hi 1)) nil)
                 ((and (<= 0 lo) (<= hi 255)) (cons 0 255))
                 ((and (<= 0 lo) (<= hi 65535)) (cons 0 65535))
                 ((and (<= -2147483648 lo) (<= hi 2147483647))
                  (cons -2147483648 2147483647))
                 ((range-fits-int64-p r) (cons +int64-min+ +int64-max+))
                 (t nil)))))))

(defun %float-storage-kind (spec)
  "Float backing kind keyword for element-type SPEC, mirroring the runtime
   NumKindForElementType (single/short-float->float[]=:single,
   double/long-float->double[]=:double), or NIL when SPEC is not a float type."
  (cond ((member spec '(single-float short-float)) :single)
        ((member spec '(double-float long-float)) :double)
        (t nil)))

(defun %simple-array-backing-kind (type)
  "Element-buffer kind for a (SIMPLE-ARRAY <integer type> (DIM)) specifier of
   rank 1 -- 1 = byte[], 2 = ushort[], 3 = int[], 4 = long[], mirroring the
   runtime's _numKind -- or NIL when TYPE is not one.

   Only SIMPLE-ARRAY qualifies, and that is the whole safety argument for
   hoisting the buffer out of a loop: CLHS 1.4.4 says a simple array is neither
   displaced nor adjustable nor fill-pointered, so its element storage cannot be
   replaced while the binding lives (ADJUST-ARRAY on a simple array returns a
   fresh array rather than rewriting this one). ARRAY and VECTOR specifiers
   promise none of that and keep the per-element helper."
  (and (consp type)
       (eq (car type) 'simple-array)
       (consp (cdr type))
       (consp (cddr type))
       (let ((dims (caddr type)))
         (and (consp dims) (null (cdr dims))
              (or (eq (car dims) '*) (integerp (car dims)))))
       (let ((r (%numeric-storage-range (cadr type))))
         (and r
              (cond ((and (eql (car r) 0) (eql (cdr r) 255)) 1)
                    ((and (eql (car r) 0) (eql (cdr r) 65535)) 2)
                    ((eql (car r) -2147483648) 3)
                    (t 4))))))

(defun %backing-slot-type (kind)
  "CIL local type for a KIND element buffer."
  (ecase kind (1 "Byte[]") (2 "UInt16[]") (3 "Int32[]") (4 "Int64[]")
         (:char "Char[]")))

(defun %backing-fetch-call (kind)
  "Runtime entry that checks the declaration once and returns the buffer.

   Above (safety 0) the *Checked entry: it signals a TYPE-ERROR where the plain
   one answers null for a non-simple array (adjustable, fill-pointered,
   displaced), which a SIMPLE-* declaration says the value is not. At (safety 0)
   the declaration is trusted, and the null sends each access to the
   per-element helper, which reads the right values. The branch is on the path
   the plain fetch takes when it declines, so a true declaration pays nothing."
  (if (compiling-at-safety-0-p)
      (ecase kind
        (1 "Runtime.BackingU8") (2 "Runtime.BackingU16")
        (3 "Runtime.BackingI32") (4 "Runtime.BackingI64")
        (:char "Runtime.BackingChars"))
      (ecase kind
        (1 "Runtime.BackingU8Checked") (2 "Runtime.BackingU16Checked")
        (3 "Runtime.BackingI32Checked") (4 "Runtime.BackingI64Checked")
        (:char "Runtime.BackingCharsChecked"))))

(defun %backing-ldelem (kind)
  "Element load leaving a raw int64."
  (ecase kind (1 :ldelem-u1) (2 :ldelem-u2) (3 :ldelem-i4) (4 :ldelem-i8)
         (:char :ldelem-u2)))

(defun %backing-stelem (kind)
  "Element store taking a raw int64 (the narrowing conv is in the opcode)."
  (ecase kind (1 :stelem-u1) (2 :stelem-u2) (3 :stelem-i4) (4 :stelem-i8)
         (:char :stelem-u2)))

(defun %backing-store-check (kind)
  "Runtime entry that signals the element-type violation an out-of-width store
   would otherwise commit silently, or NIL for the full-width kind. The boxed
   store path (LispVector.NumSet) raises the same error; narrowing in the
   opcode without this would wrap the value instead."
  (ecase kind
    (1 "Runtime.CheckStoreU8") (2 "Runtime.CheckStoreU16")
    (3 "Runtime.CheckStoreI32") (4 nil) (:char nil)))


(defun %simple-string-backing-kind (type)
  "The :CHAR backing kind for a SIMPLE-STRING type specifier of rank 1, else NIL.

   Accepts the names CL gives the same thing -- SIMPLE-STRING and
   SIMPLE-BASE-STRING -- and the spelled-out (SIMPLE-ARRAY CHARACTER (*)) with
   its BASE-CHAR and STANDARD-CHAR variants.

   Unlike %SIMPLE-ARRAY-BACKING-KIND this carries no safety argument of its own,
   because the declaration is not one here: a plain (MAKE-ARRAY n
   :element-type 'CHARACTER) is a simple string under CLHS while being a
   LispVector rather than a LispString, and programs pass adjustable and
   displaced character vectors under the declaration (SIMPLE-STRING-P used to
   accept them). The soundness lives entirely in Runtime.BackingChars, which
   takes the char[] of a LispString or of a simple rank-1 character LispVector
   and answers null for everything it cannot pin, and in the null arm every access
   carries. Above (safety 0) the fetch is BackingCharsChecked, which signals for
   the one case that is certainly a false declaration, a non-simple array, and
   still answers null for the rest."
  (labels ((char-elt-p (e)
             (and (symbolp e)
                  (member (symbol-name e) '("CHARACTER" "BASE-CHAR" "STANDARD-CHAR")
                          :test #'string=))))
    (and (or (and (symbolp type)
                  (member (symbol-name type) '("SIMPLE-STRING" "SIMPLE-BASE-STRING")
                          :test #'string=))
             (and (consp type)
                  (symbolp (car type))
                  (string= (symbol-name (car type)) "SIMPLE-ARRAY")
                  (consp (cdr type))
                  (char-elt-p (cadr type))
                  (let ((dims (and (cddr type) (caddr type))))
                    (and (consp dims) (null (cdr dims))
                         (or (eq (car dims) '*) (integerp (car dims)))))))
         :char)))

;;; Checking a SIMPLE-* array declaration where the variable is bound.
;;;
;;; Above (safety 0) a binding declared SIMPLE-ARRAY / SIMPLE-STRING /
;;; SIMPLE-BASE-STRING / SIMPLE-VECTOR / SIMPLE-BIT-VECTOR (the name or a
;;; compound spec headed by it) signals a TYPE-ERROR when the value is an array
;;; that is not simple: adjustable, fill-pointered or displaced. That is the one
;;; way such a declaration can be false that the declaration-driven fast paths
;;; would otherwise absorb silently, by falling back to the per-element helper.
;;;
;;; Where the element storage is hoisted, the prologue's fetch already tells a
;;; non-simple array apart (see %BACKING-FETCH-CALL), so the check costs
;;; nothing there and %CHECK-SIMPLE-DECL compiles to nothing. Everywhere else
;;; it is one inlined type test per binding.
;;;
;;; Only the non-simple case is checked. A value that is not an array at all is
;;; left to the operations that use it, as for every other declaration here.

(defparameter +simple-array-type-names+
  '("SIMPLE-ARRAY" "SIMPLE-STRING" "SIMPLE-BASE-STRING" "SIMPLE-VECTOR"
    "SIMPLE-BIT-VECTOR"))

(defun %simple-array-decl-spec-p (spec)
  "T when SPEC names a type every member of which is a simple array: one of
   +SIMPLE-ARRAY-TYPE-NAMES+ or a compound spec headed by one. Matched by
   SYMBOL-NAME, like the backing kinds, across the cross-compile package split."
  (let ((head (if (consp spec) (car spec) spec)))
    (and head
         (symbolp head)
         (member (symbol-name head) +simple-array-type-names+ :test #'string=)
         t)))

(defun simple-array-declared-vars (decl-forms names)
  "(VAR . SPEC) for each variable among NAMES (VAR-NAME strings) that the
   DECLARE forms at the head of DECL-FORMS declare with a SIMPLE-* array type,
   first declaration wins. A variable also declared IGNORE is left out, since
   checking it would read it."
  (let ((result '()) (ignored '()))
    (dolist (form decl-forms)
      (unless (and (consp form) (eq (car form) 'declare)) (return))
      (dolist (decl (cdr form))
        (when (consp decl)
          (cond
            ((and (symbolp (car decl))
                  (member (symbol-name (car decl)) '("IGNORE" "IGNORABLE")
                          :test #'string=))
             (dolist (v (cdr decl))
               (when (symbolp v) (push (var-name v) ignored))))
            (t
             (let* ((typed (and (symbolp (car decl))
                                (string= (symbol-name (car decl)) "TYPE")))
                    (spec (if typed (cadr decl) (car decl)))
                    (vars (if typed (cddr decl) (cdr decl))))
               (when (%simple-array-decl-spec-p spec)
                 (dolist (v vars)
                   (when (and (symbolp v) v
                              (member (var-name v) names :test #'string=)
                              (not (assoc (var-name v) result :test #'string=)))
                     (push (list (var-name v) v spec) result))))))))))
    (loop for (name v spec) in (nreverse result)
          unless (member name ignored :test #'string=)
            collect (cons v spec))))

(defun simple-decl-check-forms (decl-forms names)
  "The (%CHECK-SIMPLE-DECL VAR 'SPEC) statements for SIMPLE-ARRAY-DECLARED-VARS."
  (loop for (v . spec) in (simple-array-declared-vars decl-forms names)
        collect `(%check-simple-decl ,v (quote ,spec))))

(defun insert-simple-decl-checks (body names)
  "BODY with its SIMPLE-* declaration checks inserted right after its leading
   DECLARE forms, for the variables among NAMES (symbols or VAR-NAME strings)
   the form being compiled binds. BODY itself, EQ, when there is none."
  (let* ((names (mapcar (lambda (n) (if (stringp n) n (var-name n))) names))
         (checks (and names (simple-decl-check-forms body names))))
    (if (null checks)
        body
        (let ((decls '()) (rest body))
          (loop while (and rest (consp (car rest)) (eq (caar rest) 'declare))
                do (push (pop rest) decls))
          (append (nreverse decls) checks rest)))))

(defun %hoisted-backing-entry-p (var)
  "T when VAR's element storage is hoisted in the current scope (any kind), so
   the prologue's fetch has already checked its declaration."
  (and (symbolp var)
       var
       (boundp '*cstate*)
       (let ((e (assoc (var-name var) (cstate-array-backing-locals)
                       :test #'string=)))
         (and e
              (eq (lookup-local var) (second e))
              (not (boxed-var-p var))
              t))))

(defun string-backing-entry (var)
  "VAR's hoisted char[] entry, or NIL. Keyed and re-checked the way
   ARRAY-BACKING-ENTRY is, so a shadow or a closure re-key self-invalidates."
  (and (symbolp var)
       var
       (boundp '*cstate*)
       (let ((e (assoc (var-name var) (cstate-array-backing-locals)
                       :test #'string=)))
         (and e
              (eq (cdddr e) :char)
              (eq (lookup-local var) (second e))
              (not (boxed-var-p var))
              e))))

(defun array-backing-entry (expr)
  "For (AREF V IDX) on a rank-1 local with a hoisted element buffer, its
   CSTATE-ARRAY-BACKING-LOCALS entry; else NIL. Re-checks the binding key and
   the boxed flag exactly as NUMERIC-ARRAY-AREF-ENTRY does, so a shadow or a
   closure re-key self-invalidates."
  (and (consp expr)
       (eq (car expr) 'aref)
       (not (local-function-entry 'aref))
       (consp (cdr expr))
       (symbolp (cadr expr))
       (= (length (cddr expr)) 1)
       (boundp '*cstate*)
       (let ((e (assoc (var-name (cadr expr)) (cstate-array-backing-locals)
                       :test #'string=)))
         (and e
              (not (eq (cdddr e) :char))
              (eq (lookup-local (cadr expr)) (second e))
              (not (boxed-var-p (cadr expr)))
              e))))

(defun struct-backing-slot (entry)
  "The Int64[] slot holding ENTRY's hoisted raw slot array."
  (third entry))

(defun struct-backing-type (entry)
  "The structure name ENTRY's binding was declared to hold. The fetch compares
   it against the instance's own, because a position is read out of the
   DECLARED structure's layout and another structure maps the same slot index
   elsewhere."
  (fourth entry))

(defun struct-backing-version (entry)
  "The structure layout version ENTRY's binding was compiled against. The fetch
   compares it against the instance's, so a caller holding positions from an
   older definition gets null and falls back rather than reading the wrong
   slot -- the same thing the packed per-access constant does, asked once."
  (fifth entry))

(defun struct-backing-entry (obj packed)
  "For a raw slot read or write at the constant PACKED index on the local
   variable OBJ, (BACKING-ENTRY RAW-POSITION . KIND) when OBJ's raw slot array
   was hoisted and the slot PACKED names is a raw one; else NIL.

   Re-checks the binding key and the boxed flag exactly as ARRAY-BACKING-ENTRY
   does, so a shadowing binding or a closure re-key self-invalidates.

   PACKED carries the layout version the ACCESS was compiled against, and it
   must be the version the BINDING was compiled against, or the position taken
   from the binding's layout would describe a different definition. In one
   image they always agree; the test is what keeps that from being an
   assumption. When they disagree, or the slot is boxed, the access compiles
   the way it does today, which is also what reports a stale layout."
  (and (symbolp obj)
       obj
       (integerp packed)
       (boundp '*cstate*)
       (let ((e (assoc (var-name obj) (cstate-struct-backing-locals)
                       :test #'string=)))
         (and e
              (eq (lookup-local obj) (second e))
              (not (boxed-var-p obj))
              (= (ash packed (- +slot-version-shift+)) (struct-backing-version e))
              (let ((cell (nth (logand packed (1- (ash 1 +slot-version-shift+)))
                               (cdr (cddddr e)))))
                (and cell (>= (car cell) 0) (cons e cell)))))))

(defun %array-type-numeric-info (type)
  "For an array type specifier (simple-array ELT DIMS) / (array ELT DIMS) /
   (vector ELT ...) whose ELT gets an unboxed backing and whose rank is
   statically known to be 1-3, return the *numeric-array-locals* info tail:
   (RANK . :single/:double) for float backing, (RANK LO . HI) for integer
   backing; else NIL. DIMS must be a list of integers/* (its length is the
   rank); an unspecified rank (* or missing) is rejected.

   This is what lets an aref on an array-typed PARAMETER ride the unboxed path
   without a make-array in sight: (simple-array single-float (1025)) reads raw
   r8, (simple-array fixnum (*)) reads raw int64 (Runtime.ArefNum*L). The
   runtime fast paths re-check _numKind and fall back to the boxed path when
   the array is not actually backed that way, so a wrong declaration loses the
   speed rather than the data."
  (and (consp type)
       (let* ((head (car type))
              (elt (cadr type))
              (info (or (%float-storage-kind elt) (%numeric-storage-range elt))))
         (and info
              (cond
                ((member head '(simple-array array))
                 (let ((dims (and (cddr type) (caddr type))))
                   (and (consp dims)
                        (every (lambda (d) (or (eq d '*) (integerp d))) dims)
                        (<= 1 (length dims) 3)
                        (cons (length dims) info))))
                ((eq head 'vector) (cons 1 info))
                (t nil))))))

(defun %make-array-static-rank (dims)
  "Statically-known rank of a make-array DIMS argument form, or NIL.
   A bare fixnum-typed variable is rank 1 (an integer dimension), a (list ...)
   or quoted literal list gives its length; anything else is unknown (the
   variable could hold a dimension LIST at runtime)."
  (cond ((integerp dims) 1)
        ((and (symbolp dims) (fixnum-typed-p dims)) 1)
        ((and (consp dims) (eq (car dims) 'list)) (length (cdr dims)))
        ((and (consp dims) (eq (car dims) 'quote))
         (let ((d (cadr dims)))
           (cond ((integerp d) 1)
                 ((and (consp d) (every #'integerp d)) (length d))
                 (t nil))))
        (t nil)))

(defun %make-array-numeric-info (init)
  "If INIT is a (make-array DIMS ...) call with static rank 1-3, a constant
   quoted :element-type that upgrades to a numeric backing, and no
   :displaced-to, return (RANK LO . HI); else NIL."
  (and (consp init)
       (eq (car init) 'make-array)
       (not (local-function-entry 'make-array))
       (consp (cdr init))
       (let ((rank (%make-array-static-rank (cadr init)))
             (spec nil)
             (ok t))
         (let ((tail (cddr init)))
           (loop while (and ok tail)
                 do (let ((k (car tail)))
                      (cond
                        ((not (and (symbolp k) (consp (cdr tail))))
                         (setf ok nil))
                        ((string= (symbol-name k) "DISPLACED-TO")
                         (setf ok nil))
                        ((string= (symbol-name k) "ELEMENT-TYPE")
                         (let ((f (cadr tail)))
                           (if (and (consp f) (eq (car f) 'quote))
                               (setf spec (cadr f))
                               (setf ok nil)))))
                      (setf tail (cddr tail)))))
         (and ok rank (<= 1 rank 3) spec
              (let ((fk (%float-storage-kind spec)))
                (if fk
                    ;; Float backing: info tail is (RANK . :single/:double): an
                    ;; atom cdr distinguishes it from the integer (RANK LO . HI).
                    (cons rank fk)
                    (let ((r (%numeric-storage-range spec)))
                      (and r (cons rank r)))))))))

(defun infer-numeric-array-bindings (binding-info mutated outer)
  "Numeric-array environment for a let/let* body: start from OUTER, drop
   every name this let binds (shadowing), then add (NAME KEY RANK LO . HI)
   (or (NAME KEY RANK . :single/:double) for float backing) for each plain
   lexical, non-mutated binding whose init is a recognizable numeric make-array,
   OR whose init is a bare reference to an array local already proven numeric
   (copy propagation, e.g. the Gabriel fft's (prog ((ar areal)) ...) that
   aliases a (simple-array single-float (1025)) parameter). KEY pins the
   binding: a consumer only trusts the entry while (lookup-local NAME) still
   resolves to it (a closure body re-keys its captured vars, so stale entries
   self-invalidate). Mutated bindings are excluded; a setq could install an
   array with different backing; a mutated SOURCE alias is likewise not
   propagated."
  (let* ((bound-names (mapcar (lambda (b) (var-name (first b))) binding-info))
         (result (remove-if (lambda (e) (member (car e) bound-names :test #'string=))
                            outer)))
    (dolist (b binding-info)
      (let ((var (first b)) (init (second b)) (is-special (third b)) (key (fourth b)))
        (when (and (not is-special)
                   init
                   (not (member (var-name var) mutated :test #'string=)))
          (let ((info (or (%make-array-numeric-info init)
                          ;; copy propagation: init is a plain var already known
                          ;; numeric-backed and itself not mutated -> same backing.
                          (and (symbolp init)
                               (not (member (var-name init) mutated :test #'string=))
                               (let ((src (assoc (var-name init) result :test #'string=)))
                                 (and src (cddr src)))))))
            (when info
              (push (list* (var-name var) key info) result))))))
    result))

(defun struct-accessor-array-info (expr)
  "For (ACCESSOR OBJ) where ACCESSOR reads a structure slot declared to hold an
   array whose element type gets an unboxed backing, the *numeric-array-locals*
   info tail of that declaration -- (RANK LO . HI) or (RANK . :single/:double);
   else NIL.

   A slot read has no binding to hang a declaration on, so the slot's own :TYPE
   is the only evidence there is. It is the same evidence FIXNUM-TYPED-P already
   acts on for a slot declared to hold an integer, and it buys the same thing
   here: without it, an array reached through its accessor rather than through a
   declared local went down the generic per-element path even though the
   declaration said exactly which backing it has.

   Only the element read is decided here. The buffer is NOT hoisted (that is
   ARRAY-BACKING-ENTRY, which requires a plain local), so a slot whose contents
   contradict the declaration still falls back to the boxed path in the runtime
   rather than being rejected -- the declaration stays a hint, as it is for a
   parameter."
  (and (consp expr)
       (= (length expr) 2)
       (symbolp (car expr))
       (not (local-function-entry (car expr)))
       (boundp '*struct-accessor-types*)
       (let ((ty (gethash (car expr) *struct-accessor-types*)))
         (and ty (%array-type-numeric-info ty)))))

(defun numeric-array-aref-entry (expr)
  "If EXPR is (aref V IDX...) on a proven numeric-backed local with matching
   rank, return the info tail: (RANK LO . HI) for integer backing, or
   (RANK . :single/:double) for float backing; else NIL.
   numeric-array-aref-info / -float-kind split this by backing.

   V may also be a structure accessor call whose slot is declared to hold such
   an array (STRUCT-ACCESSOR-ARRAY-INFO), which is what puts
   (AREF (ACC X) I) on the same element path as (AREF A I) on a declared local.

   The subscripts are not required to be statically fixnum-typed: what this
   answers is how the ELEMENT is stored, which does not depend on them
   (COMPILE-INDEX-TO-LONG lowers a subscript of any shape). Requiring it cost
   the whole unboxed path whenever a loop counter carried no declaration;
   fft's inner loops are (DO ((I J (+ I LE))) ...), and every element they
   touched was boxed.

   (ROW-MAJOR-AREF V I) is answered too, but only when V is rank 1: there the
   row-major index IS the subscript (CLHS ROW-MAJOR-AREF), so the form reads
   exactly the element (AREF V I) does and every consumer of this answer, which
   takes the array and subscripts from (CADR EXPR) / (CDDR EXPR), compiles it
   unchanged. It is admitted with exactly one subscript, so the rank check
   below accepts only a rank-1 array. A higher rank would need a flat-index
   entry the runtime helpers do not have, so it stays on the generic call.

   A declaration the contents contradict fails as it does for AREF: the
   helper's fallback subscripts the actual array with one index, which signals
   for an array of another rank rather than reading a wrong element."
  (and (consp expr)
       (or (and (eq (car expr) 'aref)
                (not (local-function-entry 'aref)))
           (and (eq (car expr) 'row-major-aref)
                (not (local-function-entry 'row-major-aref))
                (consp (cdr expr))
                (consp (cddr expr))
                (null (cdddr expr))))
       (consp (cdr expr))
       (boundp '*cstate*)
       (let ((v (cadr expr))
             (idxs (cddr expr)))
         (if (symbolp v)
             (let ((entry (assoc (var-name v) (cstate-numeric-array-locals)
                                 :test #'string=)))
               (and entry
                    (eq (lookup-local v) (second entry))
                    (not (boxed-var-p v))
                    (= (length idxs) (third entry))
                    (cddr entry)))
             (let ((info (struct-accessor-array-info v)))
               (and info
                    (= (length idxs) (car info))
                    info))))))

(defun numeric-array-aref-info (expr)
  "Integer-backed aref only: (RANK LO . HI), else NIL. The (cdr info) is a
   (LO . HI) cons for integer backing but a keyword for float backing, so
   gating on a cons cdr excludes float arrays (which must not read raw longs)."
  (let ((info (numeric-array-aref-entry expr)))
    (and info (consp (cdr info)) info)))

(defun numeric-array-aref-float-kind (expr)
  "Float-backed aref only: :single / :double storage kind, else NIL."
  (let ((info (numeric-array-aref-entry expr)))
    (and info (keywordp (cdr info)) (cdr info))))

;; DOTCL:TYPEXPAND, looked up by name at load time: the SBCL cross-compile host
;; has no DOTCL package (same shape as *KNOWN-TYPE-NAME-P-FN*).
(defvar *typexpand-fn*
  (ignore-errors
   (let ((sym (find-symbol "TYPEXPAND" "DOTCL")))
     (and sym (fboundp sym) (symbol-function sym)))))

(defun %typexpand-for-range (type)
  "TYPE with user DEFTYPEs expanded, or TYPE itself when nothing can be
   expanded (or on the cross-compile host). Any error in an expander is
   treated as 'not expandable': this only feeds an optimization."
  (if (and *typexpand-fn* (or (symbolp type) (and (consp type) (symbolp (car type)))))
      (let ((e (ignore-errors (funcall *typexpand-fn* type))))
        (if e e type))
      type))

(defun %integer-type-bounds (type depth)
  "Bounds of the integers TYPE admits, as (LO . HI) where NIL means unbounded
   on that side; :NONE when TYPE is not known to be a subtype of INTEGER.
   AND intersects the bounds of the components known to be integer types and
   ignores the others: an AND is a subtype of each of its components, so any
   one integer component bounds it. FIXNUM is this runtime's fixnum, int64."
  (cond
    ((> depth 8) :none)
    ((eq type 'bit) (cons 0 1))
    ((eq type 'fixnum) (cons +int64-min+ +int64-max+))
    ((eq type 'unsigned-byte) (cons 0 nil))
    ((member type '(integer signed-byte)) (cons nil nil))
    ((and (consp type) (eq (car type) 'signed-byte) (integerp (cadr type)) (> (cadr type) 0))
     (let ((n (cadr type)))
       (cons (- (ash 1 (1- n))) (1- (ash 1 (1- n))))))
    ((and (consp type) (eq (car type) 'unsigned-byte) (integerp (cadr type)) (> (cadr type) 0))
     (cons 0 (1- (ash 1 (cadr type)))))
    ((and (consp type) (eq (car type) 'mod) (integerp (cadr type)) (> (cadr type) 0))
     (cons 0 (1- (cadr type))))
    ((and (consp type) (eq (car type) 'integer))
     (flet ((bound (b inc)
              (cond ((integerp b) b)
                    ((and (consp b) (integerp (car b)) (null (cdr b))) (+ (car b) inc))
                    (t nil))))
       (cons (bound (cadr type) 1)
             (and (cddr type) (bound (caddr type) -1)))))
    ((and (consp type) (eq (car type) 'and))
     (let ((lo nil) (hi nil) (known nil))
       (dolist (sub (cdr type))
         (let ((b (%integer-type-bounds sub (1+ depth))))
           (unless (eq b :none)
             (setf known t)
             (when (and (car b) (or (null lo) (> (car b) lo))) (setf lo (car b)))
             (when (and (cdr b) (or (null hi) (< (cdr b) hi))) (setf hi (cdr b))))))
       (if known (cons lo hi) :none)))
    ((or (symbolp type) (and (consp type) (symbolp (car type))
                             (not (member (car type) '(or not satisfies member eql)))))
     (let ((e (%typexpand-for-range type)))
       (if (equal e type) :none (%integer-type-bounds e (1+ depth)))))
    (t :none)))

(defun integer-type-range (type)
  "Inclusive (LO . HI) for a bounded integer TYPE specifier, or NIL if TYPE is
   not a bounded integer type. Recognizes bit, (signed-byte N), (unsigned-byte N),
   and (integer LO HI) with constant bounds. The caller gates on range-fits-int64-p,
   so e.g. (unsigned-byte 64) (whose HI exceeds int64) is rejected downstream.
   Storage decisions (raw structure slots, numeric array backing) use this one:
   they must agree with the runtime, which does not expand DEFTYPEs."
  (cond
    ((eq type 'bit) (cons 0 1))
    ((and (consp type) (eq (car type) 'signed-byte) (integerp (cadr type)))
     (let ((n (cadr type)))
       (cons (- (ash 1 (1- n))) (1- (ash 1 (1- n))))))
    ((and (consp type) (eq (car type) 'unsigned-byte) (integerp (cadr type)))
     (cons 0 (1- (ash 1 (cadr type)))))
    ((and (consp type) (eq (car type) 'integer)
          (integerp (cadr type)) (integerp (caddr type)))
     (cons (cadr type) (caddr type)))
    (t nil)))

(defun declared-integer-range (type)
  "Inclusive (LO . HI) for a TYPE declared on a variable or asserted by THE, or
   NIL. On top of INTEGER-TYPE-RANGE it understands (mod N), exclusive and
   half-open (integer ...) bounds, AND of an integer type with other types (e.g.
   (AND FIXNUM UNSIGNED-BYTE)), and DEFTYPEs expanding to these. A bare FIXNUM
   is left to the fixnum-locals path and answers NIL here. Only for values, never
   for storage: see INTEGER-TYPE-RANGE."
  (if (eq type 'fixnum)
      nil
      (let ((b (%integer-type-bounds type 0)))
        (and (consp b) (car b) (cdr b) (<= (car b) (cdr b)) b))))

(defun range-arith (op ra rb)
  "Interval arithmetic for OP over ranges RA=(lo . hi) and RB. Host bignums keep
   the bounds exact; the caller checks whether the result still fits int64."
  (let ((alo (car ra)) (ahi (cdr ra)) (blo (car rb)) (bhi (cdr rb)))
    (ecase op
      (+ (cons (+ alo blo) (+ ahi bhi)))
      (- (cons (- alo bhi) (- ahi blo)))
      (* (let ((c1 (* alo blo)) (c2 (* alo bhi)) (c3 (* ahi blo)) (c4 (* ahi bhi)))
           (cons (min c1 c2 c3 c4) (max c1 c2 c3 c4)))))))

(defun fixnum-leaf-range (expr)
  "Range (lo . hi) for a non-arithmetic fixnum-typed leaf, or NIL if EXPR is not
   such a leaf. Locals / calls / bitwise results take the full fixnum (int64) range;
   (the (integer lo hi) ...) contributes its declared bounds."
  (cond
    ;; Local with a proven bounded int64 range -> its TIGHT range (checked before
    ;; the full-range fixnum/long clauses so a tighter bound wins). This is what
    ;; lets e.g. (* rmdr 2) on a (signed-byte 56) prove int64-safety.
    ((small-int-local-range expr))
    ;; aref on a numeric-backed array local -> the element STORAGE range. Tight
    ;; enough to prove e.g. (+ (aref a i j) (aref b i j)) of two u16 arrays
    ;; stays in int64, keeping the whole store expression native.
    ((let ((info (numeric-array-aref-info expr)))
       (and info (cdr info))))
    ;; A structure slot declared to hold a bounded integer -> the DECLARED
    ;; range. FIXNUM-TYPED-P has taken this leaf since the slot storage became
    ;; raw, and COMPILE-AS-LONG has the matching STRUCT-SLOT-REF clause, but
    ;; the third of the three places that decide raw int64 did not, so
    ;; EXPR-INT-RANGE answered NIL and four downstream proofs failed together:
    ;; 1+ / 1- stayed on Runtime.Increment, FIXNUM-BINOP-NATIVE-P refused the
    ;; whole arithmetic expression, a subscript did not reach the raw element
    ;; path, and a LET bound from a slot got no Int64 slot.
    ;;
    ;; The declared type, not the full fixnum range: a slot declared
    ;; (SIGNED-BYTE 32) proves things a FIXNUM one cannot, and this is the
    ;; only place that knows which it was.
    ((and (consp expr) (= (length expr) 2) (symbolp (car expr))
          (not (local-function-entry (car expr)))
          (boundp '*struct-accessor-types*)
          (let ((ty (gethash (car expr) *struct-accessor-types*)))
            (and ty
                 (let ((r (if (eq ty 'fixnum)
                              (cons +int64-min+ +int64-max+)
                              (integer-type-range ty))))
                   (and r (range-fits-int64-p r) r))))))
    ;; (char-code (schar s i)) -> [0, 65535]. FIXNUM-TYPED-P has taken this
    ;; composition since COMPILE-AS-LONG grew the Runtime.CharCodeAtL lowering
    ;; for it, but FIXNUM-LEAF-RANGE did not, so the range was NIL and the
    ;; arithmetic around a string scan stayed generic even though the character
    ;; code itself was already arriving as a raw int64.
    ;;
    ;; The bound is entirely an argument from the type, with no step that holds
    ;; only on this machine. Every return of Runtime.CharCodeAtL is a C# char
    ;; widened to long: the two string backings, the char[] of a LispVector,
    ;; and Runtime.CharCode of a LispChar, whose Value is declared char. A
    ;; System.Char is 16 bits unsigned, so [0, 65535] holds for the same reason
    ;; the runtime type-checks, and CHAR-CODE-LIMIT is 65536 to match.
    ((string-char-code-ref expr)
     (cons 0 65535))
    ;; (mod X N) / (rem X N) with a constant integer divisor -> the range the
    ;; divisor forces. Unlike the three leaves above, the lowering here was
    ;; ALREADY reachable: Runtime.ModFixnumL is emitted today for a mod whose
    ;; operands are fixnum-typed. What was not reachable is the arithmetic
    ;; AROUND it -- (* 3 (mod i 10)) lowered the mod natively and then called
    ;; the generic multiply, because EXPR-INT-RANGE had nothing to say about
    ;; the mod's result and so could not prove the product fits.
    ;;
    ;; Which is why this one has to be the TIGHT range and a conservative full
    ;; int64 range would be worth nothing: [int64min, int64max] times [3,3]
    ;; does not fit, so the enclosing proof fails exactly as it did before.
    ;;
    ;; The bounds are CLHS 12.1.3.1's sign rules and not a guess about
    ;; magnitude. MOD takes the sign of the DIVISOR, so a positive N gives
    ;; [0, N-1] and a negative one [N+1, 0]. REM takes the sign of the NUMBER,
    ;; so either way it is [-(|N|-1), |N|-1]. A zero divisor is excluded
    ;; because it has no result to bound, and the dividend must be
    ;; fixnum-typed: (mod 5.5 2) is 1.5, and a float result has no integer
    ;; range at all.
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(mod rem))
          (not (local-function-entry (car expr)))
          (integerp (caddr expr))
          (not (zerop (caddr expr)))
          (fixnum-typed-p (cadr expr))
          (let* ((d (caddr expr))
                 (m (1- (abs d)))
                 (r (if (eq (car expr) 'mod)
                        (if (plusp d) (cons 0 m) (cons (- m) 0))
                        (cons (- m) m))))
            (and (range-fits-int64-p r) r))))
    ;; (length x) -> [0, 2^31-1]. The other half of the same omission the slot
    ;; clause above fixes: FIXNUM-TYPED-P has long taken LENGTH as an integer
    ;; leaf, but FIXNUM-LEAF-RANGE never did, so EXPR-INT-RANGE answered NIL
    ;; for it. COMPILE-AS-LONG has no LENGTH clause at all -- the generic
    ;; call is correct and stays -- so everything this buys is downstream of
    ;; the range: 1+ / 1- go native, FIXNUM-BINOP-NATIVE-P admits the whole
    ;; expression, and a LET bound from a length earns an Int64 slot.
    ;;
    ;; Two arguments hold the bound up and they are not the same kind.
    ;;
    ;; From the type: all four returns of Runtime.Length are Fixnum.Make of a
    ;; C# int -- 0 for NIL, the counter for a list, s.Length, v.Length -- and
    ;; every one is non-negative. That is what gives [0, int32max], and it
    ;; holds for the same reason the code type-checks.
    ;;
    ;; From the machine: the list arm counts into an int, so a list of more
    ;; than 2^31 conses would overflow it. No such list exists -- 2^31 conses
    ;; is 32 GB and the counting loop would not finish -- but that is an
    ;; argument about this machine, not about the type. Anyone widening this
    ;; bound, or reusing it where the count does not come from memory the
    ;; process holds, is leaning on the second argument and should know it.
    ((and (consp expr) (= (length expr) 2)
          (eq (car expr) 'length)
          (not (local-function-entry 'length)))
     (cons 0 (1- (ash 1 31))))
    ;; Raw int64 local (native body) or declared-fixnum local -> full int64 range.
    ((and (symbolp expr)
          (boundp '*cstate*)
          (native-slot-p expr (cstate-long-locals)))
     (cons +int64-min+ +int64-max+))
    ((and (symbolp expr)
          (boundp '*fixnum-locals*)
          (member (var-name expr) *fixnum-locals* :test #'string=)
          (not (boxed-var-p expr))
          (lookup-local expr))
     (cons +int64-min+ +int64-max+))
    ;; (the (integer lo hi) E) / (the (signed-byte N) E) etc. with constant
    ;; bounds -> declared bounds (only when they fit int64); otherwise
    ;; (the fixnum E) / (the (integer lo *) E) -> full fixnum range.
    ((and (consp expr) (eq (car expr) 'the))
     (let ((ty (cadr expr)))
       (cond
         ((let ((r (declared-integer-range ty))) (and r (range-fits-int64-p r) r)))
         ((or (eq ty 'fixnum) (and (consp ty) (eq (car ty) 'integer)))
          (cons +int64-min+ +int64-max+))
         (t nil))))
    ;; %dotimes-1+ asserts (from loop structure: counter < limit <= int64-max
    ;; at the increment site) that the incremented result fits int64, so it is
    ;; a leaf with the full int64 range: no +1 widening that would defeat the
    ;; range proof for a plain (1+ counter) on a full-range fixnum counter.
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '%dotimes-1+)
          (fixnum-typed-p (cadr expr)))
     (cons +int64-min+ +int64-max+))
    ;; (logand X M) with M non-negative: [0, max M], and exact even when X is
    ;; only right modulo 2^64 (see FIXNUM-TYPED-P).
    ((and (consp expr) (= (length expr) 3) (eq (car expr) 'logand)
          (let ((m (%logand-nonneg-operand (cadr expr) (caddr expr))))
            (and m
                 (fixnum-wrap-typed-p (if (eq m (cadr expr)) (caddr expr) (cadr expr)))
                 (cons 0 (cdr (expr-int-range m)))))))
    ;; Bitwise results stay within int64 (conservatively the full range).
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(logand logior logxor))
          (fixnum-typed-p (cadr expr)) (fixnum-typed-p (caddr expr)))
     (cons +int64-min+ +int64-max+))
    ((and (consp expr) (= (length expr) 2) (eq (car expr) 'lognot)
          (fixnum-typed-p (cadr expr)))
     (cons +int64-min+ +int64-max+))
    ;; ash with a constant shift is admitted by fixnum-typed-p only when it stays
    ;; in int64; mirror that here as the full range.
    ((and (consp expr) (= (length expr) 3) (eq (car expr) 'ash)
          (fixnum-typed-p expr))
     (cons +int64-min+ +int64-max+))
    ;; Declared-fixnum function return.
    ((and (consp expr) (symbolp (car expr))
          (eq (function-return-type (car expr)) 'fixnum)
          (not (local-function-entry (car expr))))
     (cons +int64-min+ +int64-max+))
    (t nil)))

(defun fold-nary-arith (expr)
  "(+ a b c) as (+ (+ a b) c), and the same for -, * and the associative LOGAND,
   LOGIOR and LOGXOR; anything else unchanged.
   The three places that decide whether an integer expression can be computed in
   raw int64, FIXNUM-TYPED-P, EXPR-INT-RANGE and COMPILE-AS-LONG, each match a
   two-argument call, so a three-argument one fell off the native path entirely.
   The cost was not the arithmetic but what it disqualified: an index like
   (- n i 1) made the whole AREF take the generic path, so reading from a
   specialized array boxed its element (24 B a read, e.g. every element copy in
   the fft benchmark). Left-associating is what the n-ary call means (CLHS 12.2),
   and evaluation stays left to right."
  (if (and (consp expr) (member (car expr) '(+ - * logand logior logxor)) (> (length expr) 3))
      (let ((op (car expr)))
        (reduce (lambda (a b) (list op a b)) (cdr expr)))
      (fold-typed-operand expr)))

(defun foldable-constant-symbol-p (expr)
  "True when EXPR is a symbol naming a constant variable whose value is a number
   or a character, so a reference to it may be compiled as that literal.
   CLHS 3.2.2.3 lets a reference to a constant variable be replaced by its
   value, and DEFCONSTANT requires the value to be available at compile time
   and to stay the same (a non-EQL redefinition has undefined consequences).
   Only numbers and characters are folded: their identity is EQL, so a fresh
   literal cannot be told apart from the value the symbol holds. A list or
   string constant keeps its single object and stays a variable read.
   Nothing is folded while cross-compiling: there the values come from the
   host Lisp, whose constants are not the target's (MOST-POSITIVE-FIXNUM,
   CHAR-CODE-LIMIT and friends differ between SBCL and dotcl)."
  (and (symbolp expr)
       expr
       (not (eq expr t))
       (not (keywordp expr))
       (not *cross-compiling*)
       (constantp expr)
       (boundp expr)
       (not (lookup-local expr))
       (not (nth-value 1 (lookup-symbol-macro expr)))
       (typep (symbol-value expr) '(or number character))))

(defun fold-constant-symbol (expr)
  "EXPR with a reference to a foldable constant (FOLDABLE-CONSTANT-SYMBOL-P)
   replaced by its value; anything else unchanged. Applied where the typed
   paths look at a form before compiling it, so a constant is seen as the
   literal it stands for: (setf (aref a i) +k+) then takes the same raw store
   as (setf (aref a i) 1) instead of reading the symbol through the dynamic
   binding lookup and falling to the generic store."
  (if (foldable-constant-symbol-p expr)
      (symbol-value expr)
      expr))

(defun fold-typed-operand (expr)
  "FOLD-CONSTANT-SYMBOL, and (LDB (BYTE SIZE POS) X) with literal SIZE and POS
   rewritten to the LOGAND / ASH form the LDB compiler entry open-codes. For the
   typed-path predicates and COMPILE-AS-LONG only, which look at a form before
   it is compiled."
  (if (%literal-ldb-form-p expr)
      (%literal-ldb-as-logand expr)
      (fold-constant-symbol expr)))

(defun %literal-ldb-form-p (expr)
  "True when EXPR is (LDB (BYTE SIZE POS) X) with literal SIZE and POS, the
   shape the LDB compiler entry open-codes as a shift and a mask."
  (and (consp expr) (eq (car expr) 'ldb)
       (consp (cdr expr)) (consp (cddr expr)) (null (cdddr expr))
       (let ((spec (cadr expr)))
         (and (consp spec) (eq (car spec) 'byte)
              (consp (cdr spec)) (consp (cddr spec)) (null (cdddr spec))
              (typep (cadr spec) '(integer 0 1024))
              (typep (caddr spec) '(integer 0 1024))))
       (not (local-function-entry 'ldb))
       (not (local-function-entry 'byte))
       (not (macrolet-shadowed-p 'ldb))
       (not (macrolet-shadowed-p 'byte))))

(defun %literal-ldb-as-logand (expr)
  "(LDB (BYTE SIZE POS) X) as the LOGAND / ASH form it means. The typed paths
   look at a form through FOLD-TYPED-OPERAND, so they see the mask: without
   this, (logior (ldb (byte 32 0) (ash x 7)) ...) was not known to be an
   integer at all and every operator around it took the boxed generic path."
  (let* ((spec (cadr expr))
         (size (cadr spec)) (pos (caddr spec))
         (mask (1- (ash 1 size)))
         (x (caddr expr)))
    (if (zerop pos)
        `(logand ,x ,mask)
        `(logand (ash ,x ,(- pos)) ,mask))))

(defvar *int-type-memo* nil
  "EQUAL table of answers of FIXNUM-TYPED-P and EXPR-INT-RANGE, bound for the
   duration of one outermost query. The two recurse into each other on the same
   operands (a LOGAND asks both whether its left operand is a fixnum and what
   its range is), so without the table a left-associated chain of N operands
   costs 2^N: a 30-operand LOGAND took minutes to compile. Within one query the
   compilation state they read does not change, so an answer can be reused;
   the key carries *FIXNUM-WRAP-LICENSED*, the one variable they rebind.")

(defun %int-type-memoized (tag expr fn)
  (if (atom expr)
      (funcall fn expr)
      (flet ((lookup ()
               (let ((key (list* tag *fixnum-wrap-licensed* expr)))
                 (multiple-value-bind (v found) (gethash key *int-type-memo*)
                   (if found
                       v
                       (setf (gethash key *int-type-memo*) (funcall fn expr)))))))
        (if *int-type-memo*
            (lookup)
            (let ((*int-type-memo* (make-hash-table :test 'equal)))
              (lookup))))))

(defun expr-int-range (expr)
  "Provable inclusive integer range (lo . hi) for EXPR computed entirely within
   int64, or NIL if unknown or any intermediate +/-/*/1+/1- result could exceed
   int64. Used to gate the raw unboxed arithmetic path."
  (%int-type-memoized :range expr #'%expr-int-range))

(defun %expr-int-range (expr)
  "Provable inclusive integer range (lo . hi) for EXPR computed entirely within
   int64, or NIL if unknown or any intermediate +/-/*/1+/1- result could exceed
   int64. Used to gate the raw unboxed arithmetic path."
  (let ((expr (fold-nary-arith expr))
        (*fixnum-wrap-licensed* nil))
  (cond
    ((integerp expr) (cons expr expr))
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(+ - *)))
     (let ((ra (expr-int-range (cadr expr)))
           (rb (expr-int-range (caddr expr))))
       (and ra rb
            (let ((r (range-arith (car expr) ra rb)))
              (and (range-fits-int64-p r) r)))))
    ((and (consp expr) (= (length expr) 2) (member (car expr) '(1+ 1-)))
     (let ((ra (expr-int-range (cadr expr))))
       (and ra
            (let ((r (range-arith (if (eq (car expr) '1+) '+ '-) ra '(1 . 1))))
              (and (range-fits-int64-p r) r)))))
    ;; (if c a b): the value is one arm or the other, so its range is the
    ;; union of theirs. Both arms are needed: a two-armed IF answers NIL when
    ;; the test fails. FIXNUM-TYPED-P and COMPILE-AS-LONG already take this
    ;; shape (raw int64 on each path); without the union here every consumer
    ;; that asks for a proof -- 1+ / 1-, the binop fast path, an Int64 slot's
    ;; init -- saw no range and compiled the IF boxed, arms included.
    ((and (consp expr) (eq (car expr) 'if) (= (length expr) 4))
     (let ((ra (expr-int-range (caddr expr))))
       (and ra
            (let ((rb (expr-int-range (cadddr expr))))
              (and rb
                   (cons (min (car ra) (car rb)) (max (cdr ra) (cdr rb))))))))
    ;; (ash X n), constant n: ASH is monotonic in X, so the range is the shifted
    ;; bounds. A left shift qualifies only when those fit int64, which is what
    ;; makes FIXNUM-TYPED-P accept it as exact.
    ((and (consp expr) (= (length expr) 3) (eq (car expr) 'ash)
          (integerp (caddr expr)) (<= -63 (caddr expr) 63)
          (not (local-function-entry 'ash)))
     (let ((ra (expr-int-range (cadr expr)))
           (n (caddr expr)))
       (and ra (fixnum-exact-p (cadr expr))
            (let ((r (cons (ash (car ra) n) (ash (cdr ra) n))))
              (and (range-fits-int64-p r) r)))))
    (t (fixnum-leaf-range expr)))))

(defun fixnum-binop-native-p (op args)
  "True when (OP A B) may be computed by COMPILE-FIXNUM-BINOP -- both operands
   pushed as raw int64 and the operation done by Runtime.{Add,Subtract,Multiply}Fixnum,
   which promotes to a bignum on int64 overflow and boxes the result.

   Two ways to qualify. The first is the value-range proof, which covers nested
   arithmetic: every intermediate is shown to fit int64, so COMPILE-AS-LONG may
   compute the operands raw.

   The second covers the shape the proof can never accept: operands that are
   already raw int64. A FIXNUM-declared parameter of a natively-entered function
   lives in an Int64 slot, so its range is the whole of int64 and (- N 1) is not
   provably in range -- yet N needs no computing at all, and the subtraction
   itself is what the promoting helper handles. Without this clause the operand
   was boxed back into a Fixnum only for the generic path to take it apart again,
   which is why declaring an FTYPE made a function slower than not declaring it.
   Only leaves qualify (a literal, an Int64 slot, a slot with a proven range):
   an arithmetic operand would reintroduce exactly the intermediate the range
   proof exists to check."
  (and (fixnum-typed-p (first args))
       (fixnum-typed-p (second args))
       (or (fixnum-arith-unboxed-safe-p (cons op args))
           (every (lambda (a)
                    (or (and (integerp a) (<= +int64-min+ a +int64-max+))
                        (and (symbolp a)
                             (boundp '*cstate*)
                             (or (native-slot-p a (cstate-long-locals))
                                 (small-int-local-range a)))))
                  args))))

(defun fixnum-arith-unboxed-safe-p (expr)
  "True when the BOXED fixnum fast path may compute EXPR with raw int64 ops and box
   the result. Safe only when a static value-range proof shows every intermediate
   +/-/*/1+/1- result fits int64; otherwise the caller must use the generic
   promoting path (Runtime.Add/Subtract/Multiply) so overflow yields a bignum.
   (The unsafe raw long path inside native bodies is gated separately by
   compile-as-long / fixnum-typed-p and is NOT routed through here.)"
  (and (expr-int-range expr) t))

(defun string-char-code-ref (expr)
  "(STRING . INDEX) when EXPR is (char-code (schar S I)) or (char-code (char S I)),
   NIL otherwise. That composition is how a string scan is written, and it has a
   typed entry (Runtime.CharCodeAtL) that produces the code directly -- reading
   the character first means a boxed index going in and a character object coming
   out, per iteration. A local flet/labels shadowing either name means the form
   is not this composition at all."
  (and (consp expr) (eq (car expr) 'char-code) (= (length expr) 2)
       (not (local-function-entry 'char-code))
       (let ((inner (cadr expr)))
         (and (consp inner) (member (car inner) '(schar char))
              (= (length inner) 3)
              (not (local-function-entry (car inner)))
              (cons (cadr inner) (caddr inner))))))

(defun fixnum-exact-p (expr)
  "True when COMPILE-AS-LONG of EXPR yields EXPR's exact value, not just its low
   64 bits: FIXNUM-TYPED-P with *FIXNUM-WRAP-LICENSED* NIL. Callers that may
   themselves run under the license (the fixnum-typed-p clauses for MOD, REM,
   ASH) use this for the operands whose high bits they read."
  (let ((*fixnum-wrap-licensed* nil))
    (fixnum-typed-p expr)))

(defun fixnum-wrap-typed-p (expr)
  "FIXNUM-TYPED-P under *FIXNUM-WRAP-LICENSED*: COMPILE-AS-LONG of EXPR is
   EXPR's value modulo 2^64. Only for a result that lands in a place declared
   FIXNUM; see *FIXNUM-WRAP-LICENSED*."
  (let ((*fixnum-wrap-licensed* t))
    (fixnum-typed-p expr)))

(defun %logand-nonneg-operand (a b)
  "For (LOGAND A B), the operand whose exact value is known non-negative, or
   NIL. LOGAND with a non-negative int64 keeps only bits below 63, and those
   are the same in the wrapped and the true value of the other operand, so the
   other one needs only FIXNUM-TYPED-P under the license."
  (flet ((nonneg (x) (let ((r (expr-int-range x)))
                       (and r (>= (car r) 0) (range-fits-int64-p r)
                            (fixnum-exact-p x)))))
    (cond ((nonneg b) b)
          ((nonneg a) a)
          (t nil))))

(defun %ash-count-range-within (count lo hi)
  "True when COUNT, an ASH shift count, is an exact integer expression whose
   proven range lies inside [LO, HI]."
  (let ((r (expr-int-range count)))
    (and r (<= lo (car r)) (<= (cdr r) hi)
         (fixnum-exact-p count))))

(defun fixnum-typed-p (expr)
  "Return T if EXPR is statically known to produce a Fixnum value; see
   %FIXNUM-TYPED-P."
  (%int-type-memoized :fixnum expr #'%fixnum-typed-p))

(defun %fixnum-typed-p (expr)
  "Return T if EXPR is statically known to produce a Fixnum value.
   Recognizes: literal integers in fixnum range, (the fixnum E),
   references to lexical locals declared fixnum (via *fixnum-locals*),
   calls to functions declared (declaim (ftype (function (...) fixnum) NAME)),
   and fixnum-typed arithmetic (+, -, *, 1+, 1-) whose operands are
   themselves fixnum-typed and whose result a range proof keeps in int64.
   The int64 COMPILE-AS-LONG leaves for such an EXPR is its exact value; under
   *FIXNUM-WRAP-LICENSED* only its value modulo 2^64."
  (setf expr (fold-typed-operand expr))
  (cond
    ((integerp expr) (<= +int64-min+ expr +int64-max+))
    ;; Direct Int64 local in native function body: already long, no unbox needed
    ((and (symbolp expr)
          (boundp '*cstate*)
          (native-slot-p expr (cstate-long-locals)))
     t)
    ;; Local var declared fixnum: must be a non-captured simple local
    ((and (symbolp expr)
          (boundp '*fixnum-locals*)
          (member (var-name expr) *fixnum-locals* :test #'string=)
          ;; Boxed (captured) vars need indirection: stick with generic path
          (not (boxed-var-p expr))
          (lookup-local expr))
     t)
    ;; Local with a proven bounded int64 range (signed-byte/unsigned-byte/bit
    ;; declaration or let-init inference). Slot holds a boxed Fixnum.
    ((small-int-local-range expr) t)
    ;; aref on a proven numeric-backed array local: elements are integers
    ;; within the storage range (stores are range-checked by the runtime).
    ((numeric-array-aref-info expr) t)
    ;; (char-code (schar s i)): a character code is a non-negative integer below
    ;; CHAR-CODE-LIMIT, so it is a fixnum whatever the string holds.
    ((string-char-code-ref expr) t)
    ;; (if c a b) with both arms fixnum-typed. Needs the else arm: a two-armed
    ;; IF yields NIL when the test fails, which is not a fixnum. Without this
    ;; the whole branch falls to the boxed path, and since a recursive function
    ;; is usually shaped (if base recur), that is where the boxing survives.
    ((and (consp expr) (eq (car expr) 'if) (= (length expr) 4)
          (fixnum-typed-p (caddr expr))
          (fixnum-typed-p (cadddr expr)))
     t)
    ((and (consp expr) (eq (car expr) 'the)
          (let ((ty (cadr expr)))
            (or (eq ty 'fixnum)
                (and (consp ty) (eq (car ty) 'integer))
                ;; (the (signed-byte N) E) etc.: only int64-fitting widths.
                ;; A wide type like (signed-byte 100) can hold a bignum at
                ;; runtime; bitwise/compare ops skip the overflow gate, so a
                ;; bare unbox-fixnum there would corrupt the value.
                (let ((r (declared-integer-range ty)))
                  (and r (range-fits-int64-p r))))))
     t)
    ;; +, -, *: fixnum-typed operands are not enough. COMPILE-AS-LONG computes
    ;; them with the raw int64 op, which wraps, so the result is exact only
    ;; where a range proof says it fits -- or where the license says the low
    ;; 64 bits are all the destination keeps.
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(+ - *))
          (fixnum-typed-p (cadr expr))
          (fixnum-typed-p (caddr expr))
          (or *fixnum-wrap-licensed* (expr-int-range expr)))
     t)
    ;; Three or more arguments: the same question about the left-associated form
    ;; the call stands for. Without this an index like (- n i 1) is not fixnum-
    ;; typed, which disqualifies the whole AREF from the native path.
    ((and (consp expr) (member (car expr) '(+ - * logand logior logxor)) (> (length expr) 3))
     (fixnum-typed-p (fold-nary-arith expr)))
    ((and (consp expr) (= (length expr) 2)
          (member (car expr) '(1+ 1-))
          (fixnum-typed-p (cadr expr))
          (or *fixnum-wrap-licensed* (expr-int-range expr)))
     t)
    ;; %dotimes-1+: dotimes-emitted increment whose result is asserted to fit
    ;; int64 (counter < limit at the increment site), fixnum-typed like 1+.
    ((and (consp expr) (= (length expr) 2)
          (eq (car expr) '%dotimes-1+)
          (fixnum-typed-p (cadr expr)))
     t)
    ;; logand/logior/logxor with fixnum operands -> fixnum result. Each bit of
    ;; the result depends on the same bit of the operands only, so exact
    ;; operands give an exact result and wrapped ones a wrapped one.
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(logand logior logxor))
          (fixnum-typed-p (cadr expr))
          (fixnum-typed-p (caddr expr)))
     t)
    ;; (logand X M) with M exact and non-negative is exact even when X is
    ;; wrapped: only bits below 63 survive, and wrapping keeps those.
    ;; (logand (+ a b) #xFFFFFFFF) is how 32-bit modular arithmetic is written.
    ((and (consp expr) (= (length expr) 3)
          (eq (car expr) 'logand)
          (not *fixnum-wrap-licensed*)
          (let ((m (%logand-nonneg-operand (cadr expr) (caddr expr))))
            (and m
                 (fixnum-wrap-typed-p (if (eq m (cadr expr)) (caddr expr) (cadr expr))))))
     t)
    ;; mod/rem of fixnums -> fixnum result (|r| < |divisor|, so no promotion).
    ;; The operands must be exact: the remainder of a wrapped value is not the
    ;; remainder of the true one.
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(mod rem))
          (fixnum-exact-p (cadr expr))
          (fixnum-exact-p (caddr expr)))
     t)
    ;; lognot with fixnum operand -> fixnum result
    ((and (consp expr) (= (length expr) 2)
          (eq (car expr) 'lognot)
          (fixnum-typed-p (cadr expr)))
     t)
    ;; ash with fixnum value and constant shift -> fixnum result.
    ;; A non-negative shift can overflow int64 (and a raw CIL shl masks the count
    ;; mod 64), so it is only safe to treat as a raw long when both operands are
    ;; constants and the folded result fits in int64. Negative shifts (right
    ;; shift) always shrink and stay in range.
    ;; The shifted value must be EXACT (FIXNUM-EXACT-P), not merely
    ;; fixnum-typed: a right shift exposes the high bits, which a wrapped
    ;; product or sum has lost.
    ((and (consp expr) (= (length expr) 3)
          (eq (car expr) 'ash)
          (fixnum-exact-p (cadr expr))
          (integerp (caddr expr))
          (let ((n (caddr expr)))
            (or (< n 0)
                (and (integerp (cadr expr))
                     (typep (ash (cadr expr) n) '(signed-byte 64))))))
     t)
    ;; A left shift the range proof keeps in int64 is exact: SHL of a value
    ;; below 2^(63-n) in magnitude loses no bit. (ash sum 7) of a 32-bit sum.
    ((and (consp expr) (= (length expr) 3)
          (eq (car expr) 'ash)
          (integerp (caddr expr))
          (<= 0 (caddr expr) 63)
          (expr-int-range expr))
     t)
    ;; Under the license only the low 64 bits are wanted, and SHL by a count
    ;; below 64 computes exactly those from the low 64 bits of the operand:
    ;; (logand (ash x 7) #xFFFFFFFF) is how a 32-bit rotate is written. The count
    ;; may be a variable here, as long as its range is inside [0, 63] (CIL SHL
    ;; takes the count mod 64).
    ((and (consp expr) (= (length expr) 3)
          (eq (car expr) 'ash)
          *fixnum-wrap-licensed*
          (fixnum-typed-p (cadr expr))
          (%ash-count-range-within (caddr expr) 0 63))
     t)
    ;; A right shift by a variable count whose range is inside [-63, 0]: SHR by
    ;; the negated count, exact for an exact operand (see the constant case).
    ((and (consp expr) (= (length expr) 3)
          (eq (car expr) 'ash)
          (not (integerp (caddr expr)))
          (fixnum-exact-p (cadr expr))
          (%ash-count-range-within (caddr expr) -63 0))
     t)
    ;; A structure slot declared to hold an integer that fits an int64. The
    ;; declaration is the only evidence -- a slot has no value to inspect at
    ;; compile time -- and it is now recorded and checked on every store, so it
    ;; is evidence worth acting on.
    ;;
    ;; What this decides is the READ. The slot's storage is raw, so a read that
    ;; is not known to be a fixnum goes through the generic path and boxes the
    ;; value on the way out -- on EVERY read, where the old boxed storage boxed
    ;; once and shared it. Raw storage without this clause is a pessimization,
    ;; not an optimization.
    ((and (consp expr) (= (length expr) 2) (symbolp (car expr))
          (not (local-function-entry (car expr)))
          (boundp '*struct-accessor-types*)
          (let ((ty (gethash (car expr) *struct-accessor-types*)))
            (and ty
                 (let ((r (if (eq ty 'fixnum)
                              (cons +int64-min+ +int64-max+)
                              (integer-type-range ty))))
                   (and r (range-fits-int64-p r))))))
     t)
    ;; (length x): a sequence length is a count of elements held in memory, so
    ;; it is a fixnum for any argument LENGTH accepts -- CLHS bounds a vector's
    ;; by ARRAY-TOTAL-SIZE-LIMIT, and a list's by what will fit. Here the bound
    ;; is tighter and exact: Runtime.Length counts into a C# int and every
    ;; return is Fixnum.Make of one, so the value cannot leave int32 range.
    ;;
    ;; What this buys is the loop bound. (let ((n (length arr))) (declare
    ;; (fixnum n)) ...) only earns an Int64 slot when its init is fixnum-typed,
    ;; so without this N sat in a boxed slot and every trip round the loop
    ;; unboxed it again to compare against the counter.
    ((and (consp expr) (= (length expr) 2)
          (eq (car expr) 'length)
          (not (local-function-entry 'length)))
     t)
    ;; Declared-fixnum function return: (name ...) where name has an
    ;; ftype declaration promising a fixnum result.
    ((and (consp expr) (symbolp (car expr))
          (eq (function-return-type (car expr)) 'fixnum)
          ;; Must not be shadowed by a local flet/labels function.
          (not (local-function-entry (car expr))))
     t)
    (t nil)))

(defun %long-operand-simple-p (expr)
  "True when EXPR compiles to a straight-line push with no branches, labels or
   exception regions; the only shape that is safe to leave half-evaluated on the
   CIL stack while the other operand is compiled."
  (or (integerp expr)
      (symbolp expr)
      (and (consp expr)
           (member (car expr) '(the + - * 1+ 1- logand logior logxor lognot ash
                                %dotimes-1+))
           ;; A bitwise op with an operand not known to be a fixnum branches
           ;; (COMPILE-GUARDED-LONG-BITOP).
           (or (not (member (car expr) '(logand logior logxor)))
               (/= (length expr) 3)
               (every #'fixnum-wrap-typed-p (cdr expr)))
           (every #'%long-operand-simple-p (cdr expr)))))

(defun compile-long-binop-spilled (a b)
  "Push both operands as int64, evaluating a non-straight-line operand into its
   own Int64 temp first. Same reason as COMPILE-LONG-BINOP: an operand whose code
   contains branches or a join label cannot run with the other operand pending on
   the CIL stack."
  (if (and (%long-operand-simple-p a) (%long-operand-simple-p b))
      `(,@(compile-as-long a)
        ,@(compile-as-long b))
      (let ((ta (gen-local "LSA")) (tb (gen-local "LSB")))
        `((:declare-local ,ta "Int64")
          (:declare-local ,tb "Int64")
          ,@(compile-as-long a)
          (:stloc ,ta)
          ,@(compile-as-long b)
          (:stloc ,tb)
          (:ldloc ,ta)
          (:ldloc ,tb)))))

;; Operands of a bitwise op one of which is not known to be a fixnum, in a
;; place that wants an int64 result (the result is asserted, not the operands).
;; The unknown operands are computed boxed; when they are all fixnums the op runs
;; on raw int64 values as for typed operands, otherwise the generic boxed op runs
;; and its result is unboxed (a declaration that lied still fails there).
;; An operand known to be a fixnum stays an int64 on both paths; one only known
;; modulo 2^64 is computed boxed, since the slow path needs its exact value.
;;; ------------------------------------------------------------
;;; 64-bit modular lane: integer expressions whose value is known to lie in
;;; [0, 2^64) and is made of operations that are exact modulo 2^64.
;;;
;;; An (unsigned-byte 64) at or above 2^63 is a bignum, so code written for
;;; 64-bit words -- (ldb (byte 64 0) (+ a b)), (logand x #xFFFFFFFFFFFFFFFF),
;;; a 64-bit rotate -- went through the generic bignum operations and
;;; allocated at every step. Here such an expression is computed in an int64
;;; holding the value's bits and boxed once at the end (Runtime.BoxU64).
;;;
;;; +, -, *, LOGAND, LOGIOR, LOGXOR, LOGNOT and a left shift are exact modulo
;;; 2^64 on any integers, so an operand under them only needs its low 64 bits
;;; (Runtime.Low64), whatever its declared type: a declaration that does not
;;; hold changes nothing. A right shift reads the high bits, so its operand has
;;; to be known to be in [0, 2^64): a mask, a proven range, or a value checked
;;; at run time (Runtime.IsU64), with the expression computed on the generic
;;; path from the same operand values when the check fails.
;;; ------------------------------------------------------------

(defparameter +u64-mask+ (1- (expt 2 64)))

(defvar *u64-leaves* nil
  "Operands of the expression being planned, in evaluation order: entries
   (LOCAL KIND EXPR GUARD), KIND :long (computed as an exact int64) or :obj
   (computed boxed). GUARD is true for an :obj operand that has to be checked
   to lie in [0, 2^64).")

(defun %u64-op-p (expr op)
  (and (consp expr) (eq (car expr) op)
       (not (local-function-entry op))
       (not (macrolet-shadowed-p op))))

(defun %u64-leaf (expr kind guard)
  (let ((l (gen-local (if (eq kind :long) "U64L" "U64O"))))
    (push (list l kind expr guard) *u64-leaves*)
    (list :leaf l kind)))

(defun u64-plan (expr need)
  "A plan for computing EXPR in the 64-bit lane, or NIL. NEED is :exact (the
   value itself must lie in [0, 2^64)) or :mod (its low 64 bits will do).
   Records the operands in *U64-LEAVES*."
  (let ((expr (fold-nary-arith expr)))
    (cond
      ((integerp expr)
       (and (or (eq need :mod) (<= 0 expr +u64-mask+))
            (list :const expr)))
      ((and (consp expr) (fixnum-exact-p expr))
       (let ((r (expr-int-range expr)))
         (and (or (eq need :mod) (and r (>= (car r) 0)))
              (%u64-leaf expr :long nil))))
      ((and (symbolp expr) expr (not (keywordp expr)) (fixnum-exact-p expr))
       (let ((r (expr-int-range expr)))
         (and (or (eq need :mod) (and r (>= (car r) 0)))
              (%u64-leaf expr :long nil))))
      ((or (%u64-op-p expr 'logand) (%u64-op-p expr 'logior) (%u64-op-p expr 'logxor))
       (and (= (length expr) 3)
            (let* ((op (car expr)) (a (cadr expr)) (b (caddr expr))
                   (mask-b (and (integerp b) (<= 0 b +u64-mask+))))
              (u64-plan-binary op a b
                               (cond ((eq need :mod) '(:mod :mod))
                                     ((not (eq op 'logand)) '(:exact :exact))
                                     (mask-b '(:mod :exact))
                                     ;; LOGAND of anything with a value in
                                     ;; [0, 2^64) stays in it.
                                     (t '(:mod :exact :or :exact :mod)))))))
      ((and (eq need :mod)
            (or (%u64-op-p expr '+) (%u64-op-p expr '-) (%u64-op-p expr '*))
            (= (length expr) 3))
       (u64-plan-binary (car expr) (cadr expr) (caddr expr) '(:mod :mod)))
      ((and (eq need :mod) (or (%u64-op-p expr '1+) (%u64-op-p expr '1-))
            (= (length expr) 2))
       (let ((a (u64-plan (cadr expr) :mod)))
         (and a (list (if (eq (car expr) '1+) :add :sub) a (list :const 1)))))
      ((and (eq need :mod) (%u64-op-p expr 'lognot) (= (length expr) 2))
       (let ((a (u64-plan (cadr expr) :mod)))
         (and a (list :not a))))
      ((and (%u64-op-p expr 'ash) (= (length expr) 3))
       (let ((x (cadr expr)) (k (caddr expr)))
         (cond
           ((and (integerp k) (<= 0 k 63))
            (and (eq need :mod)
                 (let ((a (u64-plan x :mod))) (and a (list :shl a k)))))
           ((and (integerp k) (< k 0))
            (let ((a (u64-plan x :exact))) (and a (list :shr a (min 64 (- k))))))
           ((integerp k) nil)
           ((%ash-count-range-within k 0 64)
            (and (eq need :mod)
                 (let ((a (u64-plan x :mod)))
                   (and a (list :shlv a (%u64-leaf k :long nil))))))
           ((%ash-count-range-within k -64 0)
            (let ((a (u64-plan x :exact)))
              (and a (list :shrv a (%u64-leaf k :long nil)))))
           (t nil))))
      ;; Any other operand is computed as it is and taken apart here.
      ((%literal-ldb-form-p expr) nil)
      ((and (symbolp expr) (or (null expr) (eq expr t) (keywordp expr))) nil)
      (t (%u64-leaf expr :obj (eq need :exact))))))

(defun u64-plan-binary (op a b needs)
  "Plan (OP A B) with A and B asked for (FIRST NEEDS) and (SECOND NEEDS); when
   NEEDS has a second pair after :OR, that is tried if the first fails."
  (let ((saved *u64-leaves*))
    (or (let ((pa (u64-plan a (first needs))))
          (and pa
               (let ((pb (u64-plan b (second needs))))
                 (and pb (list (ecase op
                                 (logand :and) (logior :or) (logxor :xor)
                                 (+ :add) (- :sub) (* :mul))
                               pa pb)))))
        (progn (setf *u64-leaves* saved)
               (and (eq (third needs) :or)
                    (u64-plan-binary op a b (cdddr needs)))))))

(defun %u64-plan-worth-p (plan)
  "True when PLAN has a boxed operand and something the generic path would do
   in bignums: arithmetic, a shift, or a 64-bit wide constant. LOGAND / LOGIOR /
   LOGXOR of boxed fixnums with narrow constants are cheaper on the generic
   entries (inlined fixnum cases) than unpacking and checking each operand."
  (labels ((wide-p (node)
             (case (car node)
               (:const (> (abs (second node)) most-positive-fixnum))
               (:leaf nil)
               ((:add :sub :mul :shl :shlv :shr :shrv) t)
               (t (some #'wide-p (cdr node))))))
    (and (consp plan) (not (eq (car plan) :leaf)) (not (eq (car plan) :const))
         (some (lambda (l) (eq (second l) :obj)) *u64-leaves*)
         (wide-p plan))))

(defun %u64-fast (node)
  (ecase (car node)
    (:const (let ((v (ldb (byte 64 0) (second node))))
              `((:ldc-i8 ,(if (logbitp 63 v) (- v (expt 2 64)) v)))))
    (:leaf (if (eq (third node) :long)
               `((:ldloc ,(second node)))
               `((:ldloc ,(second node)) (:call "Runtime.Low64"))))
    ((:and :or :xor :add :sub :mul)
     `(,@(%u64-fast (second node)) ,@(%u64-fast (third node)) (,(car node))))
    (:not `(,@(%u64-fast (second node)) (:not)))
    (:shl `(,@(%u64-fast (second node)) (:ldc-i4 ,(third node)) (:shl)))
    (:shlv `(,@(%u64-fast (second node)) (:ldloc ,(second (third node))) (:call "Runtime.ShlU64")))
    (:shr `(,@(%u64-fast (second node)) (:ldc-i8 ,(third node)) (:call "Runtime.ShrU64")))
    (:shrv `(,@(%u64-fast (second node)) (:ldloc ,(second (third node))) (:neg)
             (:call "Runtime.ShrU64")))))

(defun %u64-generic (node)
  "NODE computed on the generic path from the same operand values."
  (ecase (car node)
    (:const (let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (second node))))
    (:leaf (if (eq (third node) :long)
               `((:ldloc ,(second node)) (:call "Fixnum.Make"))
               `((:ldloc ,(second node)))))
    ((:and :or :xor :add :sub :mul)
     `(,@(%u64-generic (second node)) ,@(%u64-generic (third node))
       (:call ,(ecase (car node)
                 (:and "Runtime.Logand2") (:or "Runtime.Logior2") (:xor "Runtime.Logxor2")
                 (:add "Runtime.Add") (:sub "Runtime.Subtract") (:mul "Runtime.Multiply")))))
    (:not `(,@(%u64-generic (second node)) (:call "Runtime.Lognot")))
    (:shl `(,@(%u64-generic (second node)) (:ldc-i8 ,(third node)) (:call "Fixnum.Make")
            (:call "Runtime.Ash")))
    (:shlv `(,@(%u64-generic (second node)) (:ldloc ,(second (third node))) (:call "Fixnum.Make")
             (:call "Runtime.Ash")))
    (:shr `(,@(%u64-generic (second node)) (:ldc-i8 ,(- (third node))) (:call "Fixnum.Make")
            (:call "Runtime.Ash")))
    (:shrv `(,@(%u64-generic (second node)) (:ldloc ,(second (third node))) (:call "Fixnum.Make")
             (:call "Runtime.Ash")))))

(defun try-compile-u64 (expr)
  "EXPR compiled in the 64-bit lane, leaving the boxed value, or NIL."
  (let* ((*u64-leaves* nil)
         (plan (u64-plan expr :exact)))
    (when (and plan (%u64-plan-worth-p plan))
      (let* ((leaves (reverse *u64-leaves*))
             (slow (gen-label "U64SLOW"))
             (done (gen-label "U64DONE"))
             (guards (remove-if-not #'fourth leaves)))
        `(,@(loop for (l kind x) in leaves
                  append (if (eq kind :long)
                             `((:declare-local ,l "Int64")
                               ,@(compile-as-long x)
                               (:stloc ,l))
                             `((:declare-local ,l "LispObject")
                               ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                                   (compile-expr x))
                               (:stloc ,l))))
          ,@(loop for (l) in guards
                  append `((:ldloc ,l) (:call "Runtime.IsU64") (:brfalse ,slow)))
          ,@(%u64-fast plan)
          (:call "Runtime.BoxU64")
          ,@(when guards
              `((:br ,done)
                (:label ,slow)
                ,@(%u64-generic plan)
                (:label ,done))))))))

(defun compile-guarded-long-bitop (fn a b)
  (let ((op (ecase fn (logand :and) (logior :or) (logxor :xor)))
        (method (ecase fn (logand "Runtime.Logand2") (logior "Runtime.Logior2")
                  (logxor "Runtime.Logxor2")))
        (slow (gen-label "BITSLOW"))
        (done (gen-label "BITDONE"))
        (parts nil))
    ;; PARTS: (LONG-P . LOCAL) per operand, in evaluation order.
    (let ((code
            (loop for x in (list a b)
                  append (if (fixnum-exact-p x)
                             (let ((l (gen-local "BITL")))
                               (push (cons t l) parts)
                               `((:declare-local ,l "Int64")
                                 ,@(compile-as-long x)
                                 (:stloc ,l)))
                             (let ((o (gen-local "BITO")))
                               (push (cons nil o) parts)
                               `((:declare-local ,o "LispObject")
                                 ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                                     (compile-expr x))
                                 (:stloc ,o)))))))
      (setf parts (nreverse parts))
      `(,@code
        ,@(loop for (long-p . l) in parts
                unless long-p
                  append `((:ldloc ,l) (:call "Runtime.IsFixnumObject") (:brfalse ,slow)))
        ,@(loop for (long-p . l) in parts
                append (if long-p `((:ldloc ,l)) `((:ldloc ,l) (:unbox-fixnum))))
        (,op)
        (:br ,done)
        (:label ,slow)
        ,@(loop for (long-p . l) in parts
                append (if long-p `((:ldloc ,l) (:call "Fixnum.Make")) `((:ldloc ,l))))
        (:call ,method)
        (:unbox-fixnum)
        (:label ,done)))))

(defun compile-long-binop (op a b)
  "Emit a native int64 binop. Each operand that is not straight-line code is
   evaluated into its own Int64 temp FIRST, so nothing of this expression is
   pending on the CIL stack while that operand's code runs.

   Leaving operand 1 on the stack across operand 2 is only valid if operand 2 is
   branch-free. It is not, for instance, when operand 2 is a call to a function
   proclaimed INLINE whose body is a COND: MAYBE-EXPAND-INLINE rewrites the call
   to (LET (...) (BLOCK f body)), the block's arms jump to a join label, and the
   pending operand makes the stack depth at that label disagree between paths.
   The result was IL the JIT rejects outright; real libraries hit it (fset's
   CHAMP nodes, where every accessor is an inlined COND used inside fixnum
   arithmetic), and the failure surfaces only when the method is first CALLED,
   because JIT is lazy."
  (if (and (%long-operand-simple-p a) (%long-operand-simple-p b))
      `(,@(compile-as-long a)
        ,@(compile-as-long b)
        (,op))
      (let ((ta (gen-local "LBA")) (tb (gen-local "LBB")))
        `((:declare-local ,ta "Int64")
          (:declare-local ,tb "Int64")
          ,@(compile-as-long a)
          (:stloc ,ta)
          ,@(compile-as-long b)
          (:stloc ,tb)
          (:ldloc ,ta)
          (:ldloc ,tb)
          (,op)))))

(defun compile-as-long (expr)
  "Compile EXPR leaving an int64 on the stack. Caller must have verified
   fixnum-typed-p; this routine assumes the invariant."
  (setf expr (fold-typed-operand expr))
  (cond
    ((integerp expr)
     `((:ldc-i8 ,expr)))
    ;; Direct Int64 local in native body: already long, no unbox
    ((and (symbolp expr)
          (boundp '*cstate*)
          (native-slot-p expr (cstate-long-locals)))
     `((:ldloc ,(lookup-local expr))))
    ;; Native self-call: long args avoid boxing; InvokeNativeN returns LispObject,
    ;; so unbox-fixnum extracts the long back for the caller.
    ((and (consp expr) (symbolp (car expr))
          (boundp '*cstate*) (cstate-native-self-name)
          (cstate-self-fn-local)
          (string= (mangle-name (car expr)) (cstate-native-self-name))
          (not (local-function-entry (car expr)))
          (let ((n (length (cdr expr)))) (and (>= n 1) (<= n 4)))
          ;; The parameters of a native self are all declared FIXNUM.
          (every #'fixnum-wrap-typed-p (cdr expr)))
     (let ((n-args (length (cdr expr))))
       `(,(if (eq (cstate-self-fn-local) :arg0) '(:ldarg 0)
              `(:ldloc ,(cstate-self-fn-local)))
         ;; APPEND, not MAPCAN. A backquoted instruction list whose tail is all
         ;; constant folds to one literal for that tail, so COMPILE-AS-LONG can
         ;; hand back a list whose last cons is a literal shared by every call
         ;; that took the same branch. MAPCAN would NCONC the next argument's
         ;; instructions onto it, and they would still be there the next time
         ;; that branch is taken -- carrying a local from a function compiled
         ;; earlier into a body that never declared it.
         ,@(loop for arg in (cdr expr)
                 append (let ((*in-tail-position* nil) (*in-mv-context* nil))
                          (compile-as-long arg)))
         (:callvirt ,(invoke-native-name n-args))
         (:unbox-fixnum))))
    ;; Declared-fixnum local: load slot (LispObject) then unbox.
    ;; A captured+mutated (boxed) local's slot holds the LispObject[1] cell,
    ;; not the Fixnum: unboxing it directly would castclass the cell array.
    ;; Boxed vars fall through to the fallback, whose compile-expr dereferences
    ;; the cell before the unbox.
    ((and (symbolp expr)
          (boundp '*fixnum-locals*)
          (member (var-name expr) *fixnum-locals* :test #'string=)
          (not (boxed-var-p expr))
          (lookup-local expr))
     `((:ldloc ,(lookup-local expr))
       (:unbox-fixnum)))
    ;; Small-int local (bounded range): slot holds a boxed Fixnum: load + unbox.
    ((small-int-local-range expr)
     `((:ldloc ,(lookup-local expr))
       (:unbox-fixnum)))
    ;; aref on a numeric-backed array local: raw long element read, no box.
    ((numeric-array-aref-info expr)
     (compile-numeric-aref-as-long
      (cadr expr) (cddr expr) (car (numeric-array-aref-info expr))))
    ;; (char-code (schar s i)): one typed call, no boxed index, no character.
    ((string-char-code-ref expr)
     (let ((ref (string-char-code-ref expr)))
       (compile-string-char-code-as-long (car ref) (cdr ref))))
    ;; A structure slot read in a fixnum context: raw long out of the slot,
    ;; instead of the slot's LispObject through UnwrapMv and an unbox.
    ((struct-slot-ref expr)
     (let ((ref (struct-slot-ref expr)))
       (compile-struct-slot-as-long (car ref) (cdr ref))))
    ;; (if c a b), both arms fixnum-typed: branch with a raw int64 on each path
    ;; instead of boxing at the merge. The condition goes through the same
    ;; compile-boolean-branch the ordinary IF uses, so fused comparisons and
    ;; and/or/not chains behave identically here.
    ((and (consp expr) (eq (car expr) 'if) (= (length expr) 4)
          (fixnum-typed-p (caddr expr))
          (fixnum-typed-p (cadddr expr)))
     (let ((else-label (gen-label "LONGELSE"))
           (end-label (gen-label "LONGEND")))
       `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
             (compile-boolean-branch (cadr expr) else-label nil))
         ,@(compile-as-long (caddr expr))
         (:br ,end-label)
         (:label ,else-label)
         ,@(compile-as-long (cadddr expr))
         (:label ,end-label))))
    ((and (consp expr) (eq (car expr) 'the))
     ;; (the fixnum E): the declaration asserts E is a fixnum, so lower E
     ;; natively via compile-as-long: it handles +/-/*/1+/1-/locals as raw
     ;; int64 and falls back to compile-expr+unbox for forms it doesn't know.
     ;; Previously this always took compile-expr+unbox, forcing e.g.
     ;; (the fixnum (1- x)) through a box->Runtime.Decrement->unbox round-trip
     ;; (a method call) in hot fixnum loops like tak/stak.
     (compile-as-long (caddr expr)))
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(+ - *)))
     (let ((op (ecase (car expr) (+ :add) (- :sub) (* :mul))))
       (compile-long-binop op (cadr expr) (caddr expr))))
    ;; Three or more arguments: emit the left-associated form the call stands for
    ;; (FIXNUM-TYPED-P accepts it on the same reading).
    ((and (consp expr) (member (car expr) '(+ - * logand logior logxor)) (> (length expr) 3))
     (compile-as-long (fold-nary-arith expr)))
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '1+))
     `(,@(compile-as-long (cadr expr))
       (:ldc-i8 1)
       (:add)))
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '1-))
     `(,@(compile-as-long (cadr expr))
       (:ldc-i8 1)
       (:sub)))
    ;; %dotimes-1+: raw add: the emitting macro asserts the result fits int64
    ;; (the increment site is only reached while counter < limit <= int64-max).
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '%dotimes-1+))
     `(,@(compile-as-long (cadr expr))
       (:ldc-i8 1)
       (:add)))
    ;; mod/rem: leaves int64 on stack. The helpers carry CL's sign rules
    ;; (MOD follows the divisor, REM truncates); a raw CIL :rem would give REM's
    ;; answer for both.
    ;; The operands are re-checked for exactness because this can be reached
    ;; under THE or under the license, where nobody asked about them; a wrapped
    ;; operand takes the generic path through the fallback below.
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(mod rem))
          (fixnum-exact-p (cadr expr)) (fixnum-exact-p (caddr expr)))
     `(,@(compile-long-binop-spilled (cadr expr) (caddr expr))
       (:call ,(if (eq (car expr) 'mod) "Runtime.ModFixnumL" "Runtime.RemFixnumL"))))
    ;; Bitwise ops: leaves int64 on stack (callers box if needed).
    ;; Each operand has to be one COMPILE-AS-LONG can produce (its value, or
    ;; that modulo 2^64). Under (THE FIXNUM (LOGAND X #x3F)) only the result is
    ;; asserted: X may well be a bignum, and unboxing it unconditionally failed
    ;; with a .NET cast error. Such a form takes COMPILE-GUARDED-LONG-BITOP.
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(logand logior logxor))
          (fixnum-wrap-typed-p (cadr expr)) (fixnum-wrap-typed-p (caddr expr)))
     (let ((op (ecase (car expr) (logand :and) (logior :or) (logxor :xor))))
       (compile-long-binop op (cadr expr) (caddr expr))))
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(logand logior logxor)))
     (compile-guarded-long-bitop (car expr) (cadr expr) (caddr expr)))
    ((and (consp expr) (= (length expr) 2) (eq (car expr) 'lognot))
     `(,@(compile-as-long (cadr expr))
       (:not)))
    ;; ash with a variable count, admitted by FIXNUM-TYPED-P only with the
    ;; count's range inside [0, 63] (left) or [-63, 0] (right).
    ((and (consp expr) (= (length expr) 3) (eq (car expr) 'ash)
          (not (integerp (caddr expr)))
          (%ash-count-range-within (caddr expr) -63 0))
     `(,@(compile-long-binop-spilled (cadr expr) (caddr expr))
       (:neg)
       (:conv-i4)
       (:shr)))
    ((and (consp expr) (= (length expr) 3) (eq (car expr) 'ash)
          (not (integerp (caddr expr)))
          (%ash-count-range-within (caddr expr) 0 63))
     `(,@(compile-long-binop-spilled (cadr expr) (caddr expr))
       (:conv-i4)
       (:shl)))
    ;; ash with constant shift. fixnum-typed-p guarantees we only get here for a
    ;; negative shift (right shift, always safe) or a constant base whose folded
    ;; result fits int64: fold that to a literal so no overflowing SHL is emitted.
    ((and (consp expr) (= (length expr) 3) (eq (car expr) 'ash) (integerp (caddr expr))
          (or (>= (caddr expr) 0) (fixnum-exact-p (cadr expr))))
     (let ((n (caddr expr)) (x (cadr expr)))
       (cond
         ((< n 0)
          ;; Count capped at 63: CIL SHR takes it mod 64 (see COMPILE-ASH-FAST).
          `(,@(compile-as-long x)
            (:ldc-i4 ,(min 63 (- n)))
            (:shr)))
         ((integerp x)
          ;; Folded modulo 2^64: under the wrap license a constant may be
          ;; shifted past int64, and only the low 64 bits are asked for.
          (let ((v (ldb (byte 64 0) (ash x n))))
            `((:ldc-i8 ,(if (logbitp 63 v) (- v (ash 1 64)) v)))))
         (t
          ;; A left shift FIXNUM-TYPED-P admitted: exact by the range proof, or
          ;; wanted modulo 2^64 under the license. The count is below 64.
          `(,@(compile-as-long x)
            (:ldc-i4 ,n)
            (:shl))))))
    (t
     ;; Fallback: compile as LispObject and unbox. Shouldn't hit this
     ;; if fixnum-typed-p was checked first.
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr expr))
       (:unbox-fixnum)))))

(defun checked-long-arith-p (expr)
  "True for +/-/* (two operands after folding) or 1+/1- whose operands can all be
   produced as raw int64. Such an expression can be computed natively with an
   overflow CHECK even when no range proof exists; the destination is an Int64
   slot, so a result that overflows has nowhere to go and must signal either way."
  (let ((expr (fold-nary-arith expr)))
    (and (consp expr)
         (or (and (member (car expr) '(+ - *)) (= (length expr) 3)
                  (fixnum-typed-p (cadr expr)) (fixnum-typed-p (caddr expr)))
             (and (member (car expr) '(floor truncate)) (= (length expr) 3)
                  (not (local-function-entry (car expr)))
                  (fixnum-exact-p (cadr expr)) (fixnum-exact-p (caddr expr)))
             (and (member (car expr) '(1+ 1-)) (= (length expr) 2)
                  (fixnum-typed-p (cadr expr)))))))

(defun fixnum-long-lowerable-p (expr)
  "True when COMPILE-EXPR-TO-LONG lowers EXPR natively and exactly: either
   FIXNUM-TYPED-P (exact), or top-level +, -, *, 1+, 1- of such operands, which
   it computes with an overflow check. For destinations that need an int64 and
   have no use for a bignum anyway -- an array subscript above all -- where an
   overflow signalling is as good as a generic call signalling later."
  (or (fixnum-typed-p expr) (checked-long-arith-p expr)))

(defun compile-checked-long-arith (expr)
  "EXPR as a raw int64, checking for overflow rather than proving it away."
  (let* ((expr (fold-nary-arith expr))
         (op (car expr))
         (helper (ecase op
                   ((+ 1+) "Runtime.AddFixnumChecked")
                   ((- 1-) "Runtime.SubtractFixnumChecked")
                   (* "Runtime.MultiplyFixnumChecked")
                   (floor "Runtime.FloorFixnumL")
                   (truncate "Runtime.TruncateFixnumL"))))
    (if (member op '(1+ 1-))
        `(,@(compile-as-long (cadr expr)) (:ldc-i8 1) (:call ,helper))
        `(,@(compile-long-binop-spilled (cadr expr) (caddr expr)) (:call ,helper)))))

(defun compile-expr-to-long (expr)
  "Compile EXPR leaving a raw int64 on the stack, without risking silent wrap:
   the raw long path (compile-as-long) is taken only when a value-range proof
   shows every intermediate fits int64. Failing that, arithmetic whose operands
   are themselves raw goes native with an overflow check; the range proof can
   never succeed for FIXNUM + FIXNUM (that spans one bit more than int64), so
   without this every (SETQ ACC (+ ACC X)) in a declared-fixnum loop boxed both
   operands, called the generic promoting ADD, and unboxed the result. Anything
   else is evaluated boxed on the generic promoting path and unboxed; a
   declaration-violating bignum then signals (InvalidCast) instead of wrapping.
   Used to initialize and assign Int64-slot locals."
  (cond
    ((fixnum-arith-unboxed-safe-p expr) (compile-as-long expr))
    ((checked-long-arith-p expr) (compile-checked-long-arith expr))
    (t `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
             (compile-expr expr))
         (:unbox-fixnum)))))

(defun compile-index-to-long (expr)
  "Compile an array subscript leaving a raw int64 on the stack. A fixnum-typed
   subscript lowers natively (COMPILE-EXPR-TO-LONG); anything else is evaluated
   boxed and converted by Runtime.IndexL, which reports a non-integer subscript
   the way AREF does; where a bare unbox would surface it as a .NET cast.
   The unboxed element paths used to demand a statically fixnum-typed subscript,
   but an element's storage does not depend on the subscript. Requiring it meant
   an undeclared loop variable, the usual shape, (DO ((I 0 (1+ I))) ...), boxed
   every element the loop read."
  (if (fixnum-long-lowerable-p expr)
      (compile-expr-to-long expr)
      `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
            (compile-expr expr))
        (:call "Runtime.IndexL"))))

(defun compile-fixnum-binop (args op)
  "Emit native int64 binop, boxing result back to LispObject. All of +/-/*
   promote to Bignum on int64 overflow via Runtime.{Add,Subtract,Multiply}Fixnum.
   A raw native :add/:sub would silently wrap, so even the
   fixnum fast path must go through the promoting (AggressiveInlining) helper."
  (let ((helper (ecase op
                  (:add "Runtime.AddFixnum")
                  (:sub "Runtime.SubtractFixnum")
                  (:mul "Runtime.MultiplyFixnum"))))
    `(,@(compile-long-binop-spilled (first args) (second args))
      (:call ,helper))))

(defun compile-fixbit-binop (args op)
  "Emit native int64 bitwise binop (AND/OR/XOR), boxing result back to LispObject."
  `(,@(compile-long-binop-spilled (first args) (second args))
    (,op)
    (:call "Fixnum.Make")))

(defun compile-fixmod (args op)
  "Emit native int64 MOD/REM, boxing the result back to LispObject. The result
   is bounded by the divisor, so it is always a Fixnum -- no promotion check."
  `(,@(compile-long-binop-spilled (first args) (second args))
    (:call ,(if (eq op 'mod) "Runtime.ModFixnumL" "Runtime.RemFixnumL"))
    (:call "Fixnum.Make")))

(defun compile-fixbit-not (args)
  "Emit native int64 bitwise NOT, boxing result back to LispObject."
  `(,@(compile-as-long (first args))
    (:not)
    (:call "Fixnum.Make")))

(defun compile-ash-fast (args)
  "Emit native int64 ash when shift amount is a compile-time constant integer.
   Returns NIL if not applicable (non-constant shift)."
  (let ((x (first args))
        (n (second args)))
    (when (and (fixnum-exact-p x) (integerp n))
      (if (>= n 0)
          ;; Left shift can overflow int64 (and raw CIL shl masks count mod 64),
          ;; so route through Runtime.AshLeftLong which promotes to Bignum.
          `(,@(compile-as-long x)
            (:ldc-i4 ,n)
            (:call "Runtime.AshLeftLong"))
          ;; Right shift of a fixnum-typed long always fits: native SHR is safe
          ;; once the count is capped. CIL SHR takes the count mod 64, so
          ;; (ash x -64) came back as x; any count >= 63 leaves only the sign
          ;; (0 or -1), which is what SHR by 63 gives.
          `(,@(compile-as-long x)
            (:ldc-i4 ,(min 63 (- n)))
            (:shr)
            (:call "Fixnum.Make"))))))

(defun char-code-operand (expr)
  "Instructions leaving EXPR's character code on the stack as a raw int64, or
   NIL when EXPR is not something whose code is reachable without building a
   character object.

   Three shapes qualify, and only three:
   - a character literal, whose code is known here and becomes an immediate
   - a lexical local declared CHARACTER (see *CHARACTER-LOCALS*), read from its
     slot through CHAR-CODE-OF-L
   - (schar s i) / (char s i), which already has a typed entry that reads the
     code directly, so the character is never built at all

   Anything else is left to the generic comparison: this is an optimization,
   and declining it can only cost speed."
  (setf expr (fold-constant-symbol expr))
  (cond
    ((characterp expr) `((:ldc-i8 ,(char-code expr))))
    ;; A raw-code slot IS the code: the read is the whole operand. This is the
    ;; clause the native character representation exists for -- the one below
    ;; pays a call per comparison, and a scanner's whitespace test makes three
    ;; of them per input character.
    ((and (symbolp expr) expr (char-native-local-p expr) (lookup-local expr))
     `((:ldloc ,(lookup-local expr))))
    ((and (symbolp expr)
          expr
          (boundp '*character-locals*)
          (member (var-name expr) *character-locals* :test #'string=)
          (not (boxed-var-p expr))
          (lookup-local expr))
     `((:ldloc ,(lookup-local expr)) (:call "Runtime.CharCodeOfL")))
    ((and (consp expr) (member (car expr) '(schar char)) (= (length expr) 3)
          (not (local-function-entry (car expr))))
     (compile-string-char-code-as-long (cadr expr) (caddr expr)))
    (t nil)))

(defun compile-char-cmp (args op)
  "Emit a character comparison as an integer comparison of the codes. OP is the
   same keyword set COMPILE-FIXNUM-CMP takes, and the result is the same: an i4
   on the stack for :brfalse / :brtrue.

   CHAR= on two characters is EQL on their codes, so this is the same answer the
   generic entry gives -- without building the boolean object that entry returns
   and then asking IsTruthy about it."
  (let ((body (ecase op
                (:lt '((:clt)))
                (:gt '((:cgt)))
                (:eq '((:ceq)))
                (:le '((:cgt) (:ldc-i4 0) (:ceq)))
                (:ge '((:clt) (:ldc-i4 0) (:ceq)))
                (:ne '((:ceq) (:ldc-i4 0) (:ceq))))))
    ;; Both operands are pushed before the comparison, so each must be a
    ;; straight-line push; the two shapes that are not (a slot read and an
    ;; immediate are) go through COMPILE-STRING-CHAR-CODE-AS-LONG, which spills
    ;; to temps itself when its subexpressions are compound.
    `(,@(char-code-operand (first args))
      ,@(char-code-operand (second args))
      ,@body)))

(defun compile-fixnum-cmp (args op)
  "Emit native int64 comparison. OP is :lt :le :gt :ge :eq :ne.
   Leaves an i4 (0 or 1) on stack; suitable for :brfalse / :brtrue.
   Callers: compile-if-fused-comparison dispatch."
  (let ((body (ecase op
                (:lt '((:clt)))
                (:gt '((:cgt)))
                (:eq '((:ceq)))
                (:le '((:cgt) (:ldc-i4 0) (:ceq)))   ; not greater
                (:ge '((:clt) (:ldc-i4 0) (:ceq)))   ; not less
                (:ne '((:ceq) (:ldc-i4 0) (:ceq)))))) ; not equal
    `(,@(compile-long-binop-spilled (first args) (second args))
      ,@body)))

(defun literal-symbol-operand (expr)
  "If EXPR is a quoted symbol literal other than T and NIL, return that symbol.
   T and NIL are excluded because EQ and EQL bridge the T and NIL objects to
   the T and NIL symbols, which a plain reference comparison does not do."
  (and (consp expr)
       (eq (car expr) 'quote)
       (let ((s (cadr expr)))
         (and (symbolp s) (not (null s)) (not (eq s t)) s))))

(defun compile-sym-eq (args)
  "Emit an inline reference comparison for (EQ x 'SYM) / (EQL x 'SYM).
   Leaves an i4 (0 or 1) on stack; the same contract as COMPILE-FIXNUM-CMP.

   Runtime.IsTrueEq is Primary on both sides, ReferenceEquals, then a bridge
   that can only fire when one side is the T or NIL symbol; IsTrueEql adds the
   number and character cases after that. So against any other symbol literal
   the whole call reduces to Primary + a reference compare, and the literal
   side needs neither. What is saved is the call itself, which CASE pays once
   for every clause it walks past.

   CEQ is symmetric, but the operands are still emitted in source order.
   Loading a symbol literal is not a pure operation here: a keyword literal
   interns on every execution, and a literal in another package resolves (and
   can signal) at execution time. So compiling the other operand first would be
   observable if the package state changed between compilation and the call.
   When the literal comes first it goes through a temp rather than staying on
   the stack, because the other operand may compile to code containing a try
   region, and CIL only allows entering one with an empty stack."
  (let* ((lit-first (literal-symbol-operand (first args)))
         (lit (if lit-first (first args) (second args)))
         (other (if lit-first (second args) (first args)))
         (lit-instrs (let ((*in-tail-position* nil) (*in-mv-context* nil))
                       (compile-expr lit)))
         (other-instrs (let ((*in-tail-position* nil) (*in-mv-context* nil))
                         (compile-expr other))))
    (if lit-first
        (let ((tmp (gen-local "SE")))
          `((:declare-local ,tmp "LispObject")
            ,@lit-instrs
            (:stloc ,tmp)
            ,@other-instrs
            (:call "Runtime.Primary")
            (:ldloc ,tmp)
            (:ceq)))
        `(,@other-instrs
          (:call "Runtime.Primary")
          ,@lit-instrs
          (:ceq)))))

;;; ============================================================
;;; Double-float native arithmetic
;;; Parallel to the fixnum path above, but emits native r8 (IEEE 754
;;; double) arithmetic with a final newobj DoubleFloat to box the result.
;;; ============================================================

(defun %double-float-type-spec-p (ty)
  "T for a type specifier that names the double-float format: DOUBLE-FLOAT,
   LONG-FLOAT (the same format here, as the structure slot clause of
   DOUBLE-FLOAT-TYPED-P already assumes), or a range of either."
  (or (member ty '(double-float long-float))
      (and (consp ty) (member (car ty) '(double-float long-float)))))

(defun %single-float-type-spec-p (ty)
  "T for SINGLE-FLOAT or a range of it."
  (or (eq ty 'single-float)
      (and (consp ty) (eq (car ty) 'single-float))))

(defun double-float-typed-p (expr)
  "Return T if EXPR is statically known to produce a DoubleFloat value.
   Recognizes: (the double-float E), local vars declared double-float
   (via *double-float-locals*), recursive binary arithmetic (+, -, *, /)
   and unary negate (- x) whose operands are themselves double-float-typed.
   A literal whose value IS a double-float (e.g. 2.0d0) is recognized: the
   read object already has a definite type, so there is no single/double
   ambiguity; only the reader's choice for an unsuffixed 2.0 was ambiguous,
   and that produces a single-float object here, not a double."
  ;; A three-argument call means the same thing as nested two-argument ones
  ;; (CLHS 12.2), but every clause below matches a two-argument call, so
  ;; (* 2.0d0 z2 aux) was not recognized as double-typed and the enclosing
  ;; + fell off the native path with it. FOLD-NARY-ARITH is what the integer
  ;; side already does for the same reason.
  (let ((expr (fold-nary-arith expr)))
  (cond
    ;; Literal double-float constant: unambiguous (the object is a double).
    ((typep expr 'double-float) t)
    ;; aref on a proven double-float-backed array local: the element is a
    ;; double, readable as a native r8 (Runtime.ArefNum*D).
    ((eq (numeric-array-aref-float-kind expr) :double) t)
    ((and (symbolp expr)
          (boundp '*double-float-locals*)
          (member (var-name expr) *double-float-locals* :test #'string=)
          (not (boxed-var-p expr))
          (lookup-local expr))
     t)
    ((and (consp expr) (eq (car expr) 'the)
          (let ((ty (cadr expr)))
            (or (eq ty 'double-float)
                (and (consp ty) (eq (car ty) 'double-float)))))
     t)
    ;; A structure slot declared DOUBLE-FLOAT. The integer sibling of this
    ;; clause is in FIXNUM-TYPED-P and exists for the same reason: the slot's
    ;; storage is a raw double, so a read that is not known to be one goes
    ;; through the generic path and boxes the value on every read.
    ((and (consp expr) (= (length expr) 2) (symbolp (car expr))
          (not (local-function-entry (car expr)))
          (boundp '*struct-accessor-types*)
          (member (gethash (car expr) *struct-accessor-types*)
                  '(double-float long-float)))
     t)
    ;; Declared double-float function return: (name ...) where name has a
    ;; return type promising a double (see FUNCTION-RETURN-TYPE for which
    ;; inferred ones count). The fixnum sibling is the last clause of
    ;; FIXNUM-TYPED-P. COMPILE-AS-DOUBLE lowers it through its generic
    ;; fallback: the call, then an unbox of the returned DoubleFloat.
    ((and (consp expr) (symbolp (car expr))
          (%double-float-type-spec-p (function-return-type (car expr)))
          ;; Must not be shadowed by a local flet/labels function.
          (not (local-function-entry (car expr))))
     t)
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(+ - * /))
          (double-float-typed-p (cadr expr))
          (double-float-typed-p (caddr expr)))
     t)
    ;; (abs x) of a double is a double -- same format, no contagion to consider.
    ((and (consp expr) (= (length expr) 2) (eq (car expr) (quote abs))
          (double-float-typed-p (cadr expr)))
     t)
    ;; Unary negate: (- x) where x is double-typed -> double.
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '-)
          (double-float-typed-p (cadr expr)))
     t)
    (t nil))))

(defun float-native-local-kind (sym)
  "If SYM is a lexical local whose slot holds a native float (r8/r4) directly
   rather than a boxed DoubleFloat/SingleFloat, return :double / :single; else
   NIL. Gated (like the long-locals table) on a live lexical binding and non-boxed
   (captured vars keep the boxed slot, since env capture stores an object)."
  (cond
    ((and (boundp '*cstate*)
          (native-slot-p sym (cstate-native-double-locals)))
     :double)
    ((and (boundp '*cstate*)
          (native-slot-p sym (cstate-native-single-locals)))
     :single)
    (t nil)))

(defun compile-float-native-value (expr kind)
  "Compile EXPR leaving a native r8 (KIND :double) or r4 (KIND :single) on the
   stack, for storing into a native float slot. A float-typed EXPR lowers via
   compile-as-double/single (no box); anything else is compiled generically and
   unboxed; the slot's declaration promises a float, so a non-float value
   surfaces as a loud InvalidCast (matching the long-locals store contract)."
  (ecase kind
    (:double (if (double-float-typed-p expr)
                 (compile-as-double expr)
                 `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                       (compile-expr expr))
                   (:unbox-double))))
    (:single (if (single-float-typed-p expr)
                 (compile-as-single expr)
                 `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                       (compile-expr expr))
                   (:unbox-single))))))

(defun double-sqrt-form-p (expr)
  "T when EXPR is (SQRT X) with X statically a DOUBLE-FLOAT.

   Deliberately NOT a clause of DOUBLE-FLOAT-TYPED-P, which is where every other
   shape of this kind lives: the square root of a negative double is a COMPLEX,
   so (SQRT X) alone does not have a known type even when X does. The type is
   known only where the program has ALSO declared the result -- (THE
   DOUBLE-FLOAT (SQRT X)), a double-declared binding, a double-float array
   element. Those are the callers, and each of them supplies the half this
   predicate leaves out."
  (and (consp expr) (= (length expr) 2) (eq (car expr) 'sqrt)
       (not (local-function-entry 'sqrt))
       (double-float-typed-p (cadr expr))))

(defun double-sqrt-instr ()
  "The call a lowered (SQRT X) emits, which is where the declaration is either
   trusted or checked. At (safety 0) a declaration is a license, as it is for
   THE FIXNUM arithmetic, and a negative argument gives the NaN Math.Sqrt
   returns. From (safety 1) up it is an assertion, so a negative argument -- the
   one input whose true answer is a COMPLEX -- signals a TYPE-ERROR."
  (if (compiling-at-safety-0-p)
      '(:call "Math.SqrtDouble")
      '(:call "Runtime.SqrtDoubleChecked")))

(defun compile-as-double (expr)
  "Compile EXPR leaving a native r8 (double) on the stack.
   Caller must have verified double-float-typed-p."
  ;; Folded the same way DOUBLE-FLOAT-TYPED-P folds it, so what this walks is
  ;; the shape that was approved.
  (let ((expr (fold-nary-arith expr)))
  (cond
    ;; A double-float literal is an IL immediate (ldc.r8). Falling through to
    ;; the generic path instead compiles it as a constant-pool reference and
    ;; unboxes that: ldc.i4 idx; call GetConstant; castclass LispObject;
    ;; castclass DoubleFloat; call get_Value. Inside a loop that whole
    ;; sequence is paid per iteration, which is why declared double-float
    ;; arithmetic lagged the same loop written over fixnums, where a literal
    ;; already lowered to ldc.i8.
    ((typep expr 'double-float)
     `((:ldc-r8 ,expr)))
    ;; Native r8 slot (double-rep local): the raw double is already in the slot.
    ((eq (float-native-local-kind expr) :double)
     `((:ldloc ,(lookup-local expr))))
    ((and (symbolp expr)
          (boundp '*double-float-locals*)
          (member (var-name expr) *double-float-locals* :test #'string=)
          (lookup-local expr))
     `((:ldloc ,(lookup-local expr))
       (:unbox-double)))
    ;; A structure slot declared DOUBLE-FLOAT -> raw r8 read, no box. The
    ;; integer sibling is COMPILE-STRUCT-SLOT-AS-LONG.
    ((and (consp expr) (= (length expr) 2) (symbolp (car expr))
          (boundp '*struct-accessor-types*)
          (member (gethash (car expr) *struct-accessor-types*)
                  '(double-float long-float))
          (struct-slot-ref expr))
     (let* ((ref (struct-slot-ref expr))
            (hit (struct-backing-entry (car ref) (cdr ref))))
       (if (and hit (eql (cddr hit) 1))
           (%struct-backing-read hit (car ref) (cdr ref))
           `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                 (compile-expr (car ref)))
             (:ldc-i4 ,(cdr ref))
             (:call "Runtime.StructRefD")))))
    ;; aref on a double-float-backed array local -> raw r8 read, no box.
    ((eq (numeric-array-aref-float-kind expr) :double)
     (compile-numeric-aref-float (cadr expr) (cddr expr)
                                 (car (numeric-array-aref-entry expr))))
    ;; (the double-float E) where E is itself provably a double: lower E the
    ;; native way instead of compiling it generically and unboxing the result.
    ;; The declaration is the only thing that made the SQRT shape reachable, and
    ;; a boxed argument to it is exactly what this path exists to avoid. Gated on
    ;; DOUBLE-FLOAT-TYPED-P of the inner form, not on the declaration: THE
    ;; promises the VALUE's type, and (the double-float (+ i j)) over two fixnums
    ;; must still go through the generic add that produces that double.
    ((and (consp expr) (eq (car expr) 'the)
          (or (double-sqrt-form-p (caddr expr))
              (double-float-typed-p (caddr expr))))
     (compile-as-double (caddr expr)))
    ((and (consp expr) (eq (car expr) 'the))
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr (caddr expr)))
       (:unbox-double)))
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(+ - * /)))
     (let ((op (ecase (car expr) (+ :add) (- :sub) (* :mul) (/ :div))))
       `(,@(compile-as-double (cadr expr))
         ,@(compile-as-double (caddr expr))
         (,op))))
    ;; (abs x) -> raw magnitude, no box on the way in or out.
    ((and (consp expr) (= (length expr) 2) (eq (car expr) (quote abs)))
     `(,@(compile-as-double (cadr expr)) (:call "Math.AbsDouble")))
    ;; (sqrt x) -> raw root. Reached only from a context that has already
    ;; declared the result a double-float; see DOUBLE-SQRT-FORM-P.
    ((double-sqrt-form-p expr)
     `(,@(compile-as-double (cadr expr)) ,(double-sqrt-instr)))
    ;; Unary negate: (- x) -> native r8 neg.
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '-))
     `(,@(compile-as-double (cadr expr)) (:neg)))
    (t
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr expr))
       (:unbox-double))))))

(defun float-typed-p (expr)
  "T if EXPR is statically known to produce a float, of either format."
  (or (double-float-typed-p expr) (single-float-typed-p expr)))

(defun compile-as-double-widened (expr)
  "Compile a float-typed EXPR leaving a native r8, widening a single-float one.
   CLHS 12.1.4.1 compares a mixed pair in the longer format, and widening a
   single to a double is exact, so a comparison can take either format this way.
   COMPILE-AS-DOUBLE alone cannot: on a single-typed expression it falls through
   to the generic path and unboxes a SingleFloat as a DoubleFloat."
  (if (double-float-typed-p expr)
      (compile-as-double expr)
      `(,@(compile-as-single expr) (:conv-r8))))

(defun compile-double-binop (args op)
  "Emit: compile-as-double a, compile-as-double b, <op>, newobj DoubleFloat."
  `(,@(compile-as-double (first args))
    ,@(compile-as-double (second args))
    (,op)
    (:newobj "DoubleFloat")))

(defun compile-double-cmp (args op)
  "Emit native r8 comparison. OP is :lt :le :gt :ge :eq :ne.
   Leaves an i4 (0 or 1) on stack.

   NaN is unordered: <, >, <= and >= are all false against it, and only /= is
   true. <= and >= therefore negate the UNORDERED comparison (cgt.un/clt.un),
   which is true when the operands are unordered, so the negation is false --
   negating the ordered form answered true for a NaN, which is how a declared
   DOUBLE-FLOAT comparison came to disagree with the same comparison undeclared."
  (let ((body (ecase op
                (:lt (quote ((:clt))))
                (:gt (quote ((:cgt))))
                (:eq (quote ((:ceq))))
                (:le (quote ((:cgt-un) (:ldc-i4 0) (:ceq))))
                (:ge (quote ((:clt-un) (:ldc-i4 0) (:ceq))))
                (:ne '((:ceq) (:ldc-i4 0) (:ceq))))))
    `(,@(compile-as-double-widened (first args))
      ,@(compile-as-double-widened (second args))
      ,@body)))

;;; ============================================================
;;; Decimal native arithmetic
;;; Parallel to the double-float path, but emits native System.Decimal ops
;;; (decimal.op_Addition etc.) and boxes with newobj LispDecimal. The point is
;;; SEMANTIC as well as speed: System.Decimal arithmetic preserves scale, so in a
;;; (declare (type decimal ...)) scope 1.50m+2.25m stays 3.75m: the undeclared
;;; tower path instead treats the decimal by its rational value (scale lost).
;;; ============================================================

;; Whether the running host knows the DECIMAL type. Evaluated at load time in the
;; TARGET (dotcl -> T): not baked by a #+dotcl read-conditional, which would freeze
;; the cross-compile host's answer (SBCL -> NIL) into the shipped compiler. ignore-errors
;; makes the probe safe on the XC host, where DECIMAL is an unknown type specifier.
(defvar *decimal-type-available*
  (ignore-errors (progn (typep nil 'decimal) t)))

(defun decimal-literal-p (expr)
  "T if EXPR is a first-class decimal object (a #m literal). NIL (never a type error)
   on a host without the DECIMAL type, e.g. the SBCL cross-compile host."
  (and *decimal-type-available* (typep expr 'decimal)))

;; Same load-time probe shape as *decimal-type-available*: the predicate lives in
;; the DOTCL package, which the SBCL cross-compile host does not have, so it is
;; looked up by name at load time rather than read as a package-qualified symbol.
(defvar *known-type-name-p-fn*
  (ignore-errors
   (let ((sym (find-symbol "KNOWN-TYPE-NAME-P" "DOTCL")))
     (and sym (fboundp sym) (symbol-function sym)))))

;; Same shape, same reason: proclamations live on the runtime's symbols, and the
;; host has neither the package nor the flags.
(defvar *declaration-name-p-fn*
  (ignore-errors
   (let ((sym (find-symbol "DECLARATION-NAME-P" "DOTCL")))
     (and sym (fboundp sym) (symbol-function sym)))))

(defvar *warned-unknown-types* (make-hash-table :test #'equal :synchronized t)
  "Names already reported by WARN-UNKNOWN-DECLARED-TYPES, so a type that is
   declared in many places is mentioned once rather than once per binding form.")

(defun %declared-type-known-p (spec)
  "How SPEC is known as a type (:DEFTYPE / :BUILTIN / :CLASS), or NIL.
   Only bare symbols are judged: a compound specifier's head is a separate
   question, and NIL/T reach here as their own objects rather than symbols."
  (cond ((null *known-type-name-p-fn*) :builtin)   ; cross-compile host: never warn
        ((not (symbolp spec)) :builtin)
        (t (funcall *known-type-name-p-fn* spec))))

(defun warn-unknown-declared-types (body)
  "Style-warn for each type named in BODY's leading declarations that nothing
   defines as a type. Such a declaration is silently dropped, the compiler has
   nothing to attach it to, so a typo, or DECIMAL where DOTCL:DECIMAL was meant,
   costs the declaration (and any optimization or check it implied) without a
   word. Reported once per name per session."
  (when *known-type-name-p-fn*
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare))
        (return))
      (dolist (decl (cdr form))
        (when (consp decl)
          (let ((head (car decl)))
            (cond
              ;; (type SPEC var...)
              ((and (symbolp head) (string= (var-name head) "TYPE"))
               (%warn-if-unknown-type (cadr decl) nil))
              ;; (SPEC var...) shorthand, and the variable-less (SPEC) that is
              ;; the shape of a user-proclaimed DECLARATION identifier. Both are
              ;; reported: neither names anything the compiler can attach, and a
              ;; proclaimed declaration name is excluded by the warner itself.
              ((and (symbolp head)
                    (every #'symbolp (cdr decl))
                    (not (member (var-name head)
                                 '("IGNORE" "IGNORABLE" "OPTIMIZE" "SPECIAL"
                                   "DYNAMIC-EXTENT" "INLINE" "NOTINLINE"
                                   "DECLARATION" "FTYPE" "VALUES")
                                 :test #'string=)))
               (%warn-if-unknown-type head (null (cdr decl)))))))))))

(defun %warn-if-unknown-type (spec lone-p)
  "Warn when a declared type name resolves to nothing. LONE-P is true for a
   variable-less (declare (spec)), where the likely intent is a declaration
   identifier rather than a type, so the message names the way to say that.
   Three ways to get here and they look identical in source:

   1. nothing defines it; a typo
   2. it is a dotcl extension type named through the WRONG symbol, e.g. a bare
      DECIMAL where DOTCL:DECIMAL was meant. Those are gated on symbol identity
      (KNOWN-TYPE-NAME-P answers NIL for a foreign DECIMAL), because the
      declaration-driven native paths key on the DOTCL symbol; so the
      declaration buys nothing and the arithmetic quietly degrades
   3. it is meant to be a declaration identifier that was never proclaimed
      (LONE-P). CLHS leaves an unproclaimed one undefined, and every other
      implementation warns

   Either way the compiler drops the declaration, so say so. Once per name."
  (when (and (symbolp spec)
             (null (%declared-type-known-p spec))
             ;; A symbol proclaimed (declaration foo) names no type by
             ;; definition, CLHS forbids it from naming both, so it is not an
             ;; unknown type, it is a declaration the user told us about.
             (null (and *declaration-name-p-fn*
                        (funcall *declaration-name-p-fn* spec))))
    (let ((pkg (let ((p (symbol-package spec)))
                 (if p (package-name p) "#")))
          (suggestion (%known-type-in-other-package spec)))
      (let ((key (format nil "~a::~a" pkg (symbol-name spec))))
        (unless (gethash key *warned-unknown-types*)
          (setf (gethash key *warned-unknown-types*) t)
          (if lone-p
              (warn "~a::~a is neither a known type nor a proclaimed declaration; ~
                     the declaration is ignored~@[ (did you mean ~a?)~] ~;
                     proclaim it with (declaim (declaration ~a))"
                    pkg (symbol-name spec) suggestion (symbol-name spec))
              (warn "declared type ~a::~a names no known type; the declaration is ignored~@[ (did you mean ~a?)~]"
                    pkg (symbol-name spec) suggestion)))))))

(defun %known-type-in-other-package (spec)
  "A same-named symbol from another package that IS known as a type, rendered
   as PKG:NAME. The DOTCL one wins when several exist; that is the case worth
   pointing at (a bare DECIMAL meaning DOTCL:DECIMAL)."
  (when *known-type-name-p-fn*
    (let ((hits '()))
      (dolist (p (list-all-packages))
        (multiple-value-bind (sym status) (find-symbol (symbol-name spec) p)
          (when (and status (not (eq sym spec)) (symbol-package sym)
                     (funcall *known-type-name-p-fn* sym))
            (pushnew sym hits :test #'eq))))
      (let ((best (or (find-if (lambda (s)
                                 (string= (package-name (symbol-package s)) "DOTCL"))
                               hits)
                      (and (= (length hits) 1) (first hits)))))
        (when best
          (format nil "~a:~a" (package-name (symbol-package best))
                  (symbol-name best)))))))

(defun decimal-typed-p (expr)
  "T if EXPR is statically known to produce a decimal (LispDecimal) value:
   a #m literal, a decimal-declared local (via *decimal-locals*), (the decimal E),
   or recursive binary +,-,*,/ / unary - whose operands are all decimal-typed."
  (cond
    ((decimal-literal-p expr) t)
    ((and (symbolp expr)
          (boundp '*decimal-locals*)
          (member (var-name expr) *decimal-locals* :test #'string=)
          (not (boxed-var-p expr))
          (lookup-local expr))
     t)
    ((and (consp expr) (eq (car expr) 'the)
          (let ((ty (cadr expr)))
            (or (decimal-type-name-p ty)
                (and (consp ty) (decimal-type-name-p (car ty))))))
     t)
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(+ - * /))
          (decimal-typed-p (cadr expr))
          (decimal-typed-p (caddr expr)))
     t)
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '-)
          (decimal-typed-p (cadr expr)))
     t)
    (t nil)))

(defun decimal-strong-typed-p (expr)
  "Like DECIMAL-TYPED-P, but a bare #m literal does NOT count on its own. A native
   decimal op fires only when a declared decimal or (the decimal E) is involved, so
   undeclared literal arithmetic, (+ #m1.5 #m1.5), stays on the standard tower and
   degrades to a rational (conservative-extension invariant #1: standard ops/literals
   do not spontaneously yield extended-type values). Literals are still valid OPERANDS
   of a strong decimal expression; compile-as-decimal loads them fine."
  (cond
    ((and (symbolp expr)
          (boundp '*decimal-locals*)
          (member (var-name expr) *decimal-locals* :test #'string=)
          (not (boxed-var-p expr))
          (lookup-local expr))
     t)
    ((and (consp expr) (eq (car expr) 'the)
          (let ((ty (cadr expr)))
            (or (decimal-type-name-p ty)
                (and (consp ty) (decimal-type-name-p (car ty))))))
     t)
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(+ - * /))
          (decimal-typed-p (cadr expr))
          (decimal-typed-p (caddr expr))
          (or (decimal-strong-typed-p (cadr expr))
              (decimal-strong-typed-p (caddr expr))))
     t)
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '-)
          (decimal-strong-typed-p (cadr expr)))
     t)
    (t nil)))

(defun decimal-float-mix-p (args)
  "T if the two ARGS are a declared/`the` decimal on one side and a statically
   float-typed value on the other. Such a mix has no lossless meaning (decimal is
   base-10 exact, float is binary approximate; .NET itself forbids implicit
   decimal<->double), so a declared scope rejects it and requires an explicit coerce
   ((float d) / (rational d)) rather than silently widening to double."
  (and (= (length args) 2)
       (let ((a (first args)) (b (second args)))
         (flet ((floatp* (e) (or (double-float-typed-p e) (single-float-typed-p e))))
           (or (and (decimal-strong-typed-p a) (floatp* b))
               (and (decimal-strong-typed-p b) (floatp* a)))))))

(defparameter *decimal-float-mix-error*
  "cannot mix a decimal with a float in one arithmetic op; .NET forbids implicit
 decimal<->double; coerce explicitly, e.g. (+ (rational d) x) or (float d)")

(defun decimal-native-local-p (sym)
  "T if SYM's slot holds a raw System.Decimal rather than a boxed LispDecimal.
   Gated on a live lexical binding and non-boxed, like the float slots: a
   captured variable keeps the boxed slot because env capture stores an object."
  (and (boundp '*cstate*)
       (native-slot-p sym (cstate-native-decimal-locals))))

(defun char-native-local-p (sym)
  "T if SYM's slot holds a raw character code rather than a boxed LispChar.
   Gated on a live lexical binding and non-boxed, like the float slots."
  (and (boundp '*cstate*)
       (native-slot-p sym (cstate-native-char-locals))))

(defun character-typed-p (expr)
  "T if EXPR is statically known to produce a CHARACTER **and** its code is
   reachable without building one. The second half is why this is narrower than
   the type question: it decides whether a binding may take a raw-code slot, so
   an expression whose code only exists inside a LispChar buys nothing.

   Two shapes qualify:
   - a character literal
   - (SCHAR S I), which COMPILE-STRING-CHAR-CODE-AS-LONG already reads as a code

   (CHAR S I) is deliberately NOT here even though its value is just as much a
   character: CHAR accepts strings SCHAR does not, and while the typed entry
   falls back correctly for them, a raw slot is worth having only where the init
   path is typed end to end. Widening this to CHAR later is one symbol.

   (CODE-CHAR N) is excluded for a harder reason: CLHS lets it answer NIL for a
   code at or above CHAR-CODE-LIMIT, and NIL is not a value a raw code slot can
   hold. The integer and float slots have no equivalent of that."
  (setf expr (fold-constant-symbol expr))
  (or (characterp expr)
      (and (consp expr) (= (length expr) 3) (eq (car expr) 'schar)
           (not (local-function-entry 'schar)))))

(defun compile-char-native-value (expr)
  "Compile EXPR leaving a raw character code on the stack as an int64, for
   storing into a native character slot. CHARACTER-TYPED-P has already accepted
   EXPR.

   int64 rather than the int32 the code actually needs, because that is the width
   every other character code on this path already has: CHAR-CODE-OPERAND's other
   two shapes leave an i8, and the comparison that consumes them cannot mix
   widths. Keeping the slot at i8 makes the hot read -- the comparison -- a bare
   LDLOC, and puts the one narrowing on the generic read, which is the rare one."
  (setf expr (fold-constant-symbol expr))
  (cond
    ((characterp expr) `((:ldc-i8 ,(char-code expr))))
    ((character-typed-p expr)
     (compile-string-char-code-as-long (cadr expr) (caddr expr)))
    ;; Anything else is compiled generically and reduced to its code. Reached
    ;; from SETQ, not from the binding: the binding only takes a raw slot when
    ;; CHARACTER-TYPED-P accepted its init, but a later assignment may be any
    ;; expression. The slot's declaration promises a character, and
    ;; CHAR-CODE-OF-L reports a value that is not one as the TYPE-ERROR the
    ;; broken promise is, rather than as a .NET cast failure.
    (t `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
             (compile-expr expr))
         (:call "Runtime.CharCodeOfL")))))

(defun compile-as-decimal (expr)
  "Compile EXPR leaving a native System.Decimal (valuetype) on the stack.
   Caller must have verified decimal-typed-p."
  (cond
    ;; Native decimal slot: the raw value is already there.
    ((decimal-native-local-p expr)
     `((:ldloc ,(lookup-local expr))))
    ((and (symbolp expr)
          (boundp '*decimal-locals*)
          (member (var-name expr) *decimal-locals* :test #'string=)
          (lookup-local expr))
     `((:ldloc ,(lookup-local expr))
       (:unbox-decimal)))
    ((and (consp expr) (eq (car expr) 'the))
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr (caddr expr)))
       (:unbox-decimal)))
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(+ - * /)))
     (let ((op (ecase (car expr) (+ :decimal-add) (- :decimal-sub)
                                 (* :decimal-mul) (/ :decimal-div))))
       `(,@(compile-as-decimal (cadr expr))
         ,@(compile-as-decimal (caddr expr))
         (,op))))
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '-))
     `(,@(compile-as-decimal (cadr expr)) (:decimal-neg)))
    (t
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr expr))
       (:unbox-decimal)))))

(defun compile-decimal-native-value (expr)
  "Compile EXPR leaving a raw System.Decimal on the stack, for storing into a
   native decimal slot. A decimal-typed EXPR lowers through compile-as-decimal
   (no box); anything else is compiled generically and unboxed; the slot's
   declaration promises a decimal, so a non-decimal value surfaces as a loud
   InvalidCast, matching the native float slots' store contract."
  (if (decimal-typed-p expr)
      (compile-as-decimal expr)
      `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
            (compile-expr expr))
        (:unbox-decimal))))

(defun compile-decimal-binop (args op)
  "Emit: compile-as-decimal a, compile-as-decimal b, <decimal op>, newobj LispDecimal."
  `(,@(compile-as-decimal (first args))
    ,@(compile-as-decimal (second args))
    (,op)
    (:newobj "LispDecimal")))

;;; ============================================================
;;; Single-float native arithmetic
;;; Parallel to the double-float path above, but emits native r4 (IEEE 754
;;; single) arithmetic with a final conv.r4 + newobj SingleFloat to box.
;;; ============================================================

(defun single-float-typed-p (expr)
  "Return T if EXPR is statically known to produce a SingleFloat value.
   Recognizes: (the single-float E), local vars declared single-float
   (via *single-float-locals*), and recursive arithmetic (+, -, *, /)
   whose operands are themselves single-float-typed. A literal single-float
   object (e.g. 2.0f0, or 2.0 under the default read format) is recognized;
   its type is already definite."
  ;; See DOUBLE-FLOAT-TYPED-P: a three-argument call means nested two-argument
  ;; ones, and every clause below matches only the two-argument shape.
  (let ((expr (fold-nary-arith expr)))
  (cond
    ;; Literal single-float constant: unambiguous (the object is a single).
    ((typep expr 'single-float) t)
    ;; aref on a proven single-float-backed array local: element is a single,
    ;; readable via Runtime.ArefNum*D (double) narrowed back with conv.r4.
    ((eq (numeric-array-aref-float-kind expr) :single) t)
    ((and (symbolp expr)
          (boundp '*single-float-locals*)
          (member (var-name expr) *single-float-locals* :test #'string=)
          (not (boxed-var-p expr))
          (lookup-local expr))
     t)
    ((and (consp expr) (eq (car expr) 'the)
          (let ((ty (cadr expr)))
            (or (eq ty 'single-float)
                (and (consp ty) (eq (car ty) 'single-float)))))
     t)
    ;; Declared single-float function return; see the same clause in
    ;; DOUBLE-FLOAT-TYPED-P.
    ((and (consp expr) (symbolp (car expr))
          (%single-float-type-spec-p (function-return-type (car expr)))
          (not (local-function-entry (car expr))))
     t)
    ((and (consp expr) (= (length expr) 3)
          (member (car expr) '(+ - * /))
          (single-float-typed-p (cadr expr))
          (single-float-typed-p (caddr expr)))
     t)
    ;; (abs x) of a single is a single.
    ((and (consp expr) (= (length expr) 2) (eq (car expr) (quote abs))
          (single-float-typed-p (cadr expr)))
     t)
    ;; Unary negate: (- x) where x is single-typed -> single.
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '-)
          (single-float-typed-p (cadr expr)))
     t)
    (t nil))))

(defun compile-as-single (expr)
  "Compile EXPR leaving a native r4 (float) on the stack.
   Caller must have verified single-float-typed-p."
  ;; Folded the same way SINGLE-FLOAT-TYPED-P folds it.
  (let ((expr (fold-nary-arith expr)))
  (cond
    ;; Literal, as an IL immediate (ldc.r4) -- see the note in
    ;; COMPILE-AS-DOUBLE for what the generic constant-pool path costs.
    ((typep expr 'single-float)
     `((:ldc-r4 ,expr)))
    ;; Native r4 slot (single-rep local): the raw float is already in the slot.
    ((eq (float-native-local-kind expr) :single)
     `((:ldloc ,(lookup-local expr))))
    ((and (symbolp expr)
          (boundp '*single-float-locals*)
          (member (var-name expr) *single-float-locals* :test #'string=)
          (lookup-local expr))
     `((:ldloc ,(lookup-local expr))
       (:unbox-single)))
    ;; aref on a single-float-backed array local -> raw r8 read (widened float),
    ;; narrowed back to r4 with conv.r4 (exact for single-float values).
    ((eq (numeric-array-aref-float-kind expr) :single)
     `(,@(compile-numeric-aref-float (cadr expr) (cddr expr)
                                     (car (numeric-array-aref-entry expr)))
       (:conv-r4)))
    ((and (consp expr) (eq (car expr) 'the))
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr (caddr expr)))
       (:unbox-single)))
    ((and (consp expr) (= (length expr) 3) (member (car expr) '(+ - * /)))
     (let ((op (ecase (car expr) (+ :add) (- :sub) (* :mul) (/ :div))))
       `(,@(compile-as-single (cadr expr))
         ,@(compile-as-single (caddr expr))
         (,op))))
    ;; (abs x) -> raw magnitude, no box on the way in or out.
    ((and (consp expr) (= (length expr) 2) (eq (car expr) (quote abs)))
     `(,@(compile-as-single (cadr expr)) (:conv-r4) (:call "Math.AbsSingle")))
    ;; Unary negate: (- x) -> native r4 neg.
    ((and (consp expr) (= (length expr) 2) (eq (car expr) '-))
     `(,@(compile-as-single (cadr expr)) (:neg)))
    (t
     `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
           (compile-expr expr))
       (:unbox-single))))))

(defun compile-single-binop (args op)
  "Emit: compile-as-single a, compile-as-single b, <op>, conv.r4, newobj SingleFloat."
  `(,@(compile-as-single (first args))
    ,@(compile-as-single (second args))
    (,op)
    (:conv-r4)
    (:newobj "SingleFloat")))

(defun compile-single-cmp (args op)
  "Emit native r4 comparison. OP is :lt :le :gt :ge :eq :ne.
   Leaves an i4 (0 or 1) on stack.

   See COMPILE-DOUBLE-CMP for why <= and >= negate the unordered comparison."
  (let ((body (ecase op
                (:lt (quote ((:clt))))
                (:gt (quote ((:cgt))))
                (:eq (quote ((:ceq))))
                (:le (quote ((:cgt-un) (:ldc-i4 0) (:ceq))))
                (:ge (quote ((:clt-un) (:ldc-i4 0) (:ceq))))
                (:ne '((:ceq) (:ldc-i4 0) (:ceq))))))
    `(,@(compile-as-single (first args))
      ,@(compile-as-single (second args))
      ,@body)))

(defun %nary-fixnum-foldable-p (op args)
  "True when (OP . ARGS) should keep folding into binary calls past the arity
   cutoff below, because every step will take the native fixnum path.

   The cutoff exists to stop a long argument list from expanding into a chain of
   binary calls; the array form is one call instead. But it is keyed on the
   count alone, so a fixnum-declared (+ x x ... x) of nine arguments falls to
   Runtime.AddN, which takes LispObject[] -- and building that array costs MORE
   than the chain it replaced. A fixnum declaration then made the code allocate
   where the undeclared form allocated nothing, which is backwards.

   Every argument has to be fixnum-typed for this: the fold re-tests the pair at
   each step, and one generic argument puts the accumulator back on the boxed
   path, where the chain has no advantage over the array.

   Overflow is NOT asked about here, and must not be. Whether the whole sum fits
   int64 is exactly what the range prover cannot show for an unbounded fixnum,
   so requiring it would decline every case this exists for. The fold does not
   need it: each binary step tests FIXNUM-ARITH-UNBOXED-SAFE-P for itself and
   takes the promoting Runtime.Add when the proof fails, so an overflowing
   intermediate still becomes a bignum. All this predicate decides is chain
   versus array, and the chain is never less correct than the array."
  (declare (ignore op))
  (and (fixnum-typed-p (first args))
       (fixnum-typed-p (second args))
       (every #'fixnum-typed-p (cddr args))))

(defun compile-add (args)
  (case (length args)
    (0 (emit-fixnum 0))
    (1 (let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args))))
    (2
     (cond
       ;; Both args known decimal -> native System.Decimal add (scale preserved).
       ((and (decimal-typed-p (first args)) (decimal-typed-p (second args))
             (or (decimal-strong-typed-p (first args)) (decimal-strong-typed-p (second args))))
        (compile-decimal-binop args :decimal-add))
       ((decimal-float-mix-p args)
        (compile-static-program-error *decimal-float-mix-error*))
       ;; Fast path: both args known double-float -> native r8 add
       ((and (double-float-typed-p (first args)) (double-float-typed-p (second args)))
        (compile-double-binop args :add))
       ;; Fast path: both args known single-float -> native r4 add
       ((and (single-float-typed-p (first args)) (single-float-typed-p (second args)))
        (compile-single-binop args :add))
       ;; Fast path: both args known fixnum AND no intermediate int64 overflow
       ;; provable -> native int64 add. Otherwise fall to the promoting Runtime.Add
       ;; so an overflowing result becomes a bignum.
       ((fixnum-binop-native-p '+ args)
        (compile-fixnum-binop args :add))
       ;; Optimize (+ x 1) and (+ 1 x) to Increment
       ((eql (second args) 1)
        (compile-unary-call (list (first args)) "Runtime.Increment" "1+"))
       ((eql (first args) 1)
        (compile-unary-call (list (second args)) "Runtime.Increment" "1+"))
       (t (compile-binary-call args "Runtime.Add"))))
    (t (if (or (<= (length args) 8) (%nary-fixnum-foldable-p '+ args))
           (compile-expr (cons '+ (cons (list '+ (first args) (second args)) (cddr args))))
           `(,@(compile-args-array args) (:call "Runtime.AddN"))))))

(defun compile-sub (args)
  (case (length args)
    (0 (compile-static-program-error "-: too few arguments: 0 (expected at least 1)"))
    (1 (cond
         ;; Native unary negate for a strongly-decimal operand (scale preserved). A bare
         ;; #m literal is NOT strong, so (- #m1.5) degrades to a rational like other
         ;; undeclared literal arithmetic.
         ((decimal-strong-typed-p (first args))
          `(,@(compile-as-decimal (first args)) (:decimal-neg) (:newobj "LispDecimal")))
         ;; Native unary negate for float-typed operand.
         ((double-float-typed-p (first args))
          `(,@(compile-as-double (first args)) (:neg) (:newobj "DoubleFloat")))
         ((single-float-typed-p (first args))
          `(,@(compile-as-single (first args)) (:neg) (:conv-r4) (:newobj "SingleFloat")))
         ;; Unary (- x): Subtract(0, x) would collapse -0.0 (0.0-0.0=+0.0), so use Negate
         ;; (IEEE sign flip). Needed for signed-zero correctness of conjugate/eql.
         (t (compile-unary-call (list (first args)) "Runtime.Negate" "-"))))
    (2
     (cond
       ((and (decimal-typed-p (first args)) (decimal-typed-p (second args))
             (or (decimal-strong-typed-p (first args)) (decimal-strong-typed-p (second args))))
        (compile-decimal-binop args :decimal-sub))
       ((decimal-float-mix-p args)
        (compile-static-program-error *decimal-float-mix-error*))
       ((and (double-float-typed-p (first args)) (double-float-typed-p (second args)))
        (compile-double-binop args :sub))
       ((and (single-float-typed-p (first args)) (single-float-typed-p (second args)))
        (compile-single-binop args :sub))
       ((fixnum-binop-native-p '- args)
        (compile-fixnum-binop args :sub))
       ((eql (second args) 1)
        (compile-unary-call (list (first args)) "Runtime.Decrement" "1-"))
       (t (compile-binary-call args "Runtime.Subtract"))))
    (t (if (or (<= (length args) 8) (%nary-fixnum-foldable-p '- args))
           (compile-expr (cons '- (cons (list '- (first args) (second args)) (cddr args))))
           `(,@(compile-args-array args) (:call "Runtime.SubtractN"))))))

(defun compile-mul (args)
  (case (length args)
    (0 (emit-fixnum 1))
    (1 (let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args))))
    (2
     (cond
       ((and (decimal-typed-p (first args)) (decimal-typed-p (second args))
             (or (decimal-strong-typed-p (first args)) (decimal-strong-typed-p (second args))))
        (compile-decimal-binop args :decimal-mul))
       ((decimal-float-mix-p args)
        (compile-static-program-error *decimal-float-mix-error*))
       ((and (double-float-typed-p (first args)) (double-float-typed-p (second args)))
        (compile-double-binop args :mul))
       ((and (single-float-typed-p (first args)) (single-float-typed-p (second args)))
        (compile-single-binop args :mul))
       ((fixnum-binop-native-p '* args)
        (compile-fixnum-binop args :mul))
       (t (compile-binary-call args "Runtime.Multiply"))))
    (t (if (or (<= (length args) 8) (%nary-fixnum-foldable-p '* args))
           (compile-expr (cons '* (cons (list '* (first args) (second args)) (cddr args))))
           `(,@(compile-args-array args) (:call "Runtime.MultiplyN"))))))

(defun compile-div (args)
  (case (length args)
    (0 (compile-static-program-error "/: too few arguments: 0 (expected at least 1)"))
    (1 (compile-binary-call (list 1 (first args)) "Runtime.Divide"))
    (2
     (cond
       ;; Both decimal -> native decimal divide (System.Decimal semantics: rounds
       ;; to 28 significant digits, unlike CL's exact rational division).
       ((and (decimal-typed-p (first args)) (decimal-typed-p (second args))
             (or (decimal-strong-typed-p (first args)) (decimal-strong-typed-p (second args))))
        (compile-decimal-binop args :decimal-div))
       ((decimal-float-mix-p args)
        (compile-static-program-error *decimal-float-mix-error*))
       ((and (double-float-typed-p (first args)) (double-float-typed-p (second args)))
        (compile-double-binop args :div))
       ((and (single-float-typed-p (first args)) (single-float-typed-p (second args)))
        (compile-single-binop args :div))
       (t (compile-binary-call args "Runtime.Divide"))))
    (t (if (or (<= (length args) 8) (%nary-fixnum-foldable-p '/ args))
           (compile-expr (cons '/ (cons (list '/ (first args) (second args)) (cddr args))))
           `(,@(compile-args-array args) (:call "Runtime.DivideN"))))))

;;; ============================================================
;;; Call helpers
;;; ============================================================

(defun compile-static-program-error (msg)
  "Emit CIL that unconditionally signals PROGRAM-ERROR with MSG."
  `((:ldstr ,msg)
    (:newobj "LispProgramError")
    (:newobj "LispErrorException")
    (:throw)))

(defun compile-nary-comparison (args op method)
  "Compile N-arg comparison (op a b c ...) as (and (op a b) (op b c) ...).
   For N=2, directly emit binary call. For N>2, expand to let* + and."
  (let ((n (length args)))
    (cond
      ((= n 0) (compile-static-program-error
                 (format nil "~A: wrong number of arguments: 0 (expected >= 1)" op)))
      ((= n 1)
       ;; (< a) evaluates arg for side effects, returns T
       `(,@(compile-expr (car args)) (:pop) ,@(emit-t)))
      ((= n 2) (compile-binary-call args method))
      (t
       ;; Expand (op a b c ...) -> (let* ((t0 a) (t1 b) (t2 c) ...) (and (op t0 t1) (op t1 t2) ...))
       (let* ((tmps (loop for i below n collect (intern (format nil "%%NARGT~A" i))))
              (bindings (loop for tmp in tmps for arg in args collect `(,tmp ,arg)))
              (pairs (loop for (t1 t2) on tmps while t2 collect `(,op ,t1 ,t2))))
         (compile-expr `(let* ,bindings (and ,@pairs))))))))

(defun simple-expr-p (expr)
  "Return T if EXPR compiles to simple stack ops without try blocks.
   Safe to leave on the evaluation stack while compiling subsequent exprs."
  (cond ((null expr) t)
        ((eq expr t) t)
        ((numberp expr) t)
        ((characterp expr) t)
        ((stringp expr) t)
        ((symbolp expr)
         ;; Simple if it's a lexical variable (not boxed, not symbol-macro)
         (and (not (lookup-symbol-macro expr))
              (lookup-local expr)
              (not (boxed-var-p expr))))
        ((and (consp expr) (eq (car expr) 'the))
         (simple-expr-p (caddr expr)))
        ((and (consp expr) (eq (car expr) 'quote)) t)
        (t nil)))

(defun compile-binary-call (args method &optional (fn-name ""))
  ;; Pre-evaluate both args to temps so the stack is empty
  ;; when each is compiled (CIL requires empty stack at try-block entry).
  ;; Args are single-valued: never in tail position, never in MV context.
  (unless (= (length args) 2)
    (return-from compile-binary-call
      (compile-static-program-error
       (format nil "~a: wrong number of arguments: ~a (expected 2)" fn-name (length args)))))
  ;; Fast path: if both args are simple, push directly without temp locals
  (if (and (simple-expr-p (first args)) (simple-expr-p (second args)))
      `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (second args)))
        (:call ,method))
      (let ((t1 (gen-local "BA")) (t2 (gen-local "BB")))
        `((:declare-local ,t1 "LispObject")
          (:declare-local ,t2 "LispObject")
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))  (:stloc ,t1)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (second args))) (:stloc ,t2)
          (:ldloc ,t1) (:ldloc ,t2)
          (:call ,method)))))

(defun compile-ternary-call (args method &optional (fn-name ""))
  (unless (= (length args) 3)
    (return-from compile-ternary-call
      (compile-static-program-error
       (format nil "~a: wrong number of arguments: ~a (expected 3)" fn-name (length args)))))
  (if (every #'simple-expr-p args)
      `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (second args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (third args)))
        (:call ,method))
      (let ((t1 (gen-local "TA")) (t2 (gen-local "TB")) (t3 (gen-local "TC")))
        `((:declare-local ,t1 "LispObject")
          (:declare-local ,t2 "LispObject")
          (:declare-local ,t3 "LispObject")
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))  (:stloc ,t1)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (second args))) (:stloc ,t2)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (third args)))  (:stloc ,t3)
          (:ldloc ,t1) (:ldloc ,t2) (:ldloc ,t3)
          (:call ,method)))))

(defun compile-quaternary-call (args method &optional (fn-name ""))
  (unless (= (length args) 4)
    (return-from compile-quaternary-call
      (compile-static-program-error
       (format nil "~a: wrong number of arguments: ~a (expected 4)" fn-name (length args)))))
  (if (every #'simple-expr-p args)
      `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (second args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (third args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (fourth args)))
        (:call ,method))
      (let ((t1 (gen-local "QA")) (t2 (gen-local "QB")) (t3 (gen-local "QC")) (t4 (gen-local "QD")))
        `((:declare-local ,t1 "LispObject")
          (:declare-local ,t2 "LispObject")
          (:declare-local ,t3 "LispObject")
          (:declare-local ,t4 "LispObject")
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))  (:stloc ,t1)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (second args))) (:stloc ,t2)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (third args)))  (:stloc ,t3)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (fourth args))) (:stloc ,t4)
          (:ldloc ,t1) (:ldloc ,t2) (:ldloc ,t3) (:ldloc ,t4)
          (:call ,method)))))

(defun compile-quinary-call (args method &optional (fn-name ""))
  (unless (= (length args) 5)
    (return-from compile-quinary-call
      (compile-static-program-error
       (format nil "~a: wrong number of arguments: ~a (expected 5)" fn-name (length args)))))
  (if (every #'simple-expr-p args)
      `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (second args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (third args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (fourth args)))
        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (fifth args)))
        (:call ,method))
      (let ((t1 (gen-local "QA")) (t2 (gen-local "QB")) (t3 (gen-local "QC"))
            (t4 (gen-local "QD")) (t5 (gen-local "QE")))
        `((:declare-local ,t1 "LispObject")
          (:declare-local ,t2 "LispObject")
          (:declare-local ,t3 "LispObject")
          (:declare-local ,t4 "LispObject")
          (:declare-local ,t5 "LispObject")
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))  (:stloc ,t1)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (second args))) (:stloc ,t2)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (third args)))  (:stloc ,t3)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (fourth args))) (:stloc ,t4)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (fifth args)))  (:stloc ,t5)
          (:ldloc ,t1) (:ldloc ,t2) (:ldloc ,t3) (:ldloc ,t4) (:ldloc ,t5)
          (:call ,method)))))

(defun compile-unary-call (args method &optional (fn-name ""))
  (unless (= (length args) 1)
    (let ((errmsg (format nil "~a: wrong number of arguments: ~a (expected 1)" fn-name (length args))))
      (return-from compile-unary-call
        (compile-static-program-error errmsg))))
  ;; Arg is single-valued: never in tail position, never in MV context
  `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))
    (:call ,method)))

(defun compile-append (args)
  "Compile (append ...) with 0+ arguments.
   Reduces multi-arg append to nested binary Runtime.Append calls."
  (cond
    ((null args) (emit-nil))
    ((null (cdr args)) (compile-for-single-value (car args)))
    ((null (cddr args)) (compile-binary-call args "Runtime.Append"))
    (t ;; 3+ args: fold left into nested binary appends
       (let ((acc (car args)))
         (dolist (a (cdr args))
           (setq acc (list 'append acc a)))
         (compile-form acc)))))

(defun compile-aref-native-index-call (arr idx-exprs method &optional (val nil val-p))
  "Compile an array read/write call whose index arguments are lowered to raw
   int64 on the stack (no Fixnum boxing); METHOD is one of the Runtime.*L
   variants taking long indices. ARR (and VAL for setters) stay boxed
   LispObject. Callers gate on every index being fixnum-typed-p. Mirrors the
   compile-N-ary-call discipline: all-simple args push directly; otherwise
   each arg is evaluated to a temp (Int64 temps for indices) so the stack is
   empty whenever a non-simple subexpression is compiled."
  (let ((all (append (list arr) idx-exprs (when val-p (list val)))))
    (if (every #'simple-expr-p all)
        `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
              (compile-expr arr))
          ,@(loop for idx in idx-exprs append (compile-expr-to-long idx))
          ,@(when val-p
              (let ((*in-tail-position* nil) (*in-mv-context* nil))
                (compile-expr val)))
          (:call ,method))
        (let ((arr-tmp (gen-local "NAA"))
              (idx-tmps (mapcar (lambda (i) (declare (ignore i)) (gen-local "NAI"))
                                idx-exprs))
              (val-tmp (when val-p (gen-local "NAV"))))
          `((:declare-local ,arr-tmp "LispObject")
            ,@(mapcar (lambda (tk) `(:declare-local ,tk "Int64")) idx-tmps)
            ,@(when val-p `((:declare-local ,val-tmp "LispObject")))
            ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                (compile-expr arr))
            (:stloc ,arr-tmp)
            ,@(loop for idx in idx-exprs
                    for tk in idx-tmps
                    append `(,@(compile-expr-to-long idx) (:stloc ,tk)))
            ,@(when val-p
                `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                      (compile-expr val))
                  (:stloc ,val-tmp)))
            (:ldloc ,arr-tmp)
            ,@(mapcar (lambda (tk) `(:ldloc ,tk)) idx-tmps)
            ,@(when val-p `((:ldloc ,val-tmp)))
            (:call ,method))))))

(defun %string-backing-read (entry idx fall-instrs boxp)
  "A character read against STR's hoisted char[] buffer, with FALL-INSTRS as the
   arm that runs when there is no buffer. BOXP leaves a character object
   (LispChar.Make); otherwise the raw code stays as a native int.

   The buffer slot is NIL whenever Runtime.BackingChars declined -- a LispString
   still holding a System.String, or a character vector that is not simple
   (adjustable, fill-pointered, displaced) or not rank 1. A simple character
   LispVector, which is what (MAKE-ARRAY n :element-type 'CHARACTER) builds, is
   fetched like a LispString's char[]. FALL-INSTRS is the typed call that ran before this
   path existed, so declining costs the speed and nothing else. Only one arm
   runs, so a subscript with a side effect is evaluated exactly once."
  (let* ((back (third entry))
         (fall (gen-label "SBFALL"))
         (done (gen-label "SBDONE"))
         (fast (if (simple-expr-p idx)
                   `((:ldloc ,back)
                     ,@(compile-index-to-long idx)
                     (:conv-i)
                     (:ldelem-u2)
                     ,@(when boxp '((:call "LispChar.Make"))))
                   (let ((tk (gen-local "SBI")))
                     `((:declare-local ,tk "Int64")
                       ,@(compile-index-to-long idx)
                       (:stloc ,tk)
                       (:ldloc ,back)
                       (:ldloc ,tk)
                       (:conv-i)
                       (:ldelem-u2)
                       ,@(when boxp '((:call "LispChar.Make"))))))))
    (ecase *backing-arm*
      (:fast fast)
      (:helper fall-instrs)
      (:both `((:ldloc ,back)
               (:brfalse ,fall)
               ,@fast
               (:br ,done)
               (:label ,fall)
               ,@fall-instrs
               (:label ,done))))))

(defun compile-string-char-code-as-long (str idx)
  "Emit (char-code (schar STR IDX)) as a raw int64. When STR has a hoisted
   char[] buffer the read is a bare ldelem against it with the typed call as the
   other arm; otherwise it is the typed call alone."
  (let ((back (string-backing-entry str)))
    (if back
        (%string-backing-read back idx (%string-char-code-call str idx) nil)
        (%string-char-code-call str idx))))

(defun %string-char-code-call (str idx)
  "The typed call form: (char-code (schar STR IDX)) as a raw int64 (Runtime.CharCodeAtL).
   STR stays a LispObject -- the runtime entry decides whether it is the simple
   string it can read directly -- and IDX is lowered the way an array subscript
   is. A non-simple subexpression is evaluated into a temp first, so the stack is
   empty whenever one compiles."
  (if (and (simple-expr-p str) (simple-expr-p idx))
      `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
            (compile-expr str))
        ,@(compile-index-to-long idx)
        (:call "Runtime.CharCodeAtL"))
      (let ((str-tmp (gen-local "SCS"))
            (idx-tmp (gen-local "SCI")))
        `((:declare-local ,str-tmp "LispObject")
          (:declare-local ,idx-tmp "Int64")
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
              (compile-expr str))
          (:stloc ,str-tmp)
          ,@(compile-index-to-long idx)
          (:stloc ,idx-tmp)
          (:ldloc ,str-tmp)
          (:ldloc ,idx-tmp)
          (:call "Runtime.CharCodeAtL")))))

(defun compile-string-char-as-object (str idx)
  "Emit (schar STR IDX), leaving the character on the stack. Hoisted buffer with
   the typed call as the other arm, exactly as the code read is."
  (let ((back (string-backing-entry str)))
    (if back
        (%string-backing-read back idx (%string-char-call str idx) t)
        (%string-char-call str idx))))

(defun %string-char-call (str idx)
  "The typed call form: (schar STR IDX) as one call (Runtime.CharAtL), leaving the
   character on the stack.

   The value-position counterpart of COMPILE-STRING-CHAR-CODE-AS-LONG, and it
   spills the same way. What it removes is what a generic call to a two-argument
   builtin pays per element: the subscript boxed into a Fixnum, the symbol
   lookup, and the virtual Invoke. STR stays a LispObject -- the runtime entry
   is the one that decides whether it is the simple string it can read
   directly -- and IDX is lowered the way an array subscript is, so an
   undeclared index still goes in raw."
  (if (and (simple-expr-p str) (simple-expr-p idx))
      `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
            (compile-expr str))
        ,@(compile-index-to-long idx)
        (:call "Runtime.CharAtL"))
      (let ((str-tmp (gen-local "SAS"))
            (idx-tmp (gen-local "SAI")))
        `((:declare-local ,str-tmp "LispObject")
          (:declare-local ,idx-tmp "Int64")
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
              (compile-expr str))
          (:stloc ,str-tmp)
          ,@(compile-index-to-long idx)
          (:stloc ,idx-tmp)
          (:ldloc ,str-tmp)
          (:ldloc ,idx-tmp)
          (:call "Runtime.CharAtL")))))

(defun compile-backing-aref-as-long (entry idx fall-instrs)
  "Emit (AREF V IDX) as a bare element load against V's hoisted buffer, leaving
   a raw int64, with FALL-INSTRS as the arm that runs when there is no buffer.
   The subscript is narrowed to a native int rather than an i4, so the CLR's own
   bounds check still rejects a subscript outside the array (conv.i4 would wrap
   one into range first).

   The buffer slot is NIL whenever the fetch declined -- a fill-pointered or
   adjustable vector, whose storage VECTOR-PUSH-EXTEND and ADJUST-ARRAY replace,
   and anything that is not the declared array at all. FALL-INSTRS is the
   per-element helper that ran before this path existed, so declining costs the
   speed and nothing else. Only one arm runs, so a subscript with a side effect
   is still evaluated exactly once."
  (let ((back (third entry))
        (kind (cdddr entry))
        (fall (gen-label "ABFALL"))
        (done (gen-label "ABDONE")))
    (let ((fast (if (simple-expr-p idx)
                    `((:ldloc ,back)
                      ,@(compile-index-to-long idx)
                      (:conv-i)
                      (,(%backing-ldelem kind)))
                    (let ((tk (gen-local "NAI")))
                      `((:declare-local ,tk "Int64")
                        ,@(compile-index-to-long idx)
                        (:stloc ,tk)
                        (:ldloc ,back)
                        (:ldloc ,tk)
                        (:conv-i)
                        (,(%backing-ldelem kind)))))))
      (ecase *backing-arm*
        (:fast fast)
        (:helper fall-instrs)
        (:both `((:ldloc ,back)
                 (:brfalse ,fall)
                 ,@fast
                 (:br ,done)
                 (:label ,fall)
                 ,@fall-instrs
                 (:label ,done)))))))

(defun compile-backing-aref-set (entry idx val fall-instrs)
  "Emit (SETF (AREF V IDX) VAL) against V's hoisted buffer, with FALL-INSTRS as
   the arm that runs when there is no buffer. The value is range-checked for the
   narrow kinds first -- the opcode's narrowing would wrap an out-of-width value
   where the boxed store path signals -- and left on the stack as a RAW int64,
   as FALL-INSTRS must leave it too. The caller boxes it once, after the arms
   join: boxed in each arm, the box sat before the join label, where the
   peephole that drops a box nobody reads (statement position) cannot see it.

   Only one arm runs, so VAL is evaluated exactly once even though both arms
   contain code to evaluate it."
  (let* ((back (third entry))
         (kind (cdddr entry))
         (chk (%backing-store-check kind))
         (tk (gen-local "NAI"))
         (vk (gen-local "NAV"))
         (fall (gen-label "ABFALL"))
         (done (gen-label "ABDONE"))
         (fast `((:declare-local ,tk "Int64")
                 (:declare-local ,vk "Int64")
                 ,@(compile-index-to-long idx)
                 (:stloc ,tk)
                 ,@(compile-expr-to-long val)
                 ,@(when chk `((:call ,chk)))
                 (:stloc ,vk)
                 (:ldloc ,back)
                 (:ldloc ,tk)
                 (:conv-i)
                 (:ldloc ,vk)
                 (,(%backing-stelem kind))
                 (:ldloc ,vk))))
    (ecase *backing-arm*
      (:fast fast)
      (:helper fall-instrs)
      (:both `((:ldloc ,back)
               (:brfalse ,fall)
               ,@fast
               (:br ,done)
               (:label ,fall)
               ,@fall-instrs
               (:label ,done))))))


(defun %backing-duplicable-p (form)
  "T when FORM may be compiled twice. Compiling is not a pure function of the
   source: LOAD-TIME-VALUE takes a slot, DEFMETHOD and DEFSTRUCT run at assembly
   time whether or not their branch is taken, and EVAL-WHEN can do anything. A
   body containing any of those would get two of whatever it does, so it keeps
   the per-access test instead. Crude in the safe direction: an unfamiliar
   operator is assumed unsafe only if it is on this list, and the list is of
   things whose compile-time effect is known, so a false YES is possible and is
   caught by the test suites rather than by this."
  (cond ((not (consp form)) t)
        ((and (symbolp (car form))
              (member (symbol-name (car form))
                      '("LOAD-TIME-VALUE" "DEFMETHOD" "DEFGENERIC" "DEFSTRUCT"
                        "DEFCLASS" "EVAL-WHEN" "DEFCONSTANT" "DEFPACKAGE"
                        "DEFINE-COMPILER-MACRO" "DEFSETF" "DEFINE-SETF-EXPANDER"
                        "DEFMACRO" "IN-PACKAGE")
                      :test #'string=))
         nil)
        (t (and (%backing-duplicable-p (car form))
                (%backing-duplicable-p (cdr form))))))

(defun unswitch-array-backing (entries body thunk)
  "Emit BODY's instructions twice, once with every hoisted buffer known present
   and once with none of them used, behind a single test per buffer.

   The test a hoisted access would otherwise carry is loop-invariant, and
   leaving it at the access is what costs. Measured on the array-walk kernel:
   the per-access form turned the inner loop from a 7-instruction single block
   with a fall-through back-edge into 11 instructions across 4 blocks, because
   the test also stopped the JIT hoisting the data base and the element count
   out of the loop. Both are consequences of where the test is, not of its
   existence, so it moves here and the fast copy goes back to the original
   block.

   All buffers are tested together rather than one unswitch per buffer: n
   buffers would otherwise want 2^n copies, and a body that hoists two arrays
   and gets one of them is rare enough not to pay for that."
  (if (or (null entries)
          (not (%backing-duplicable-p body))
          (notany (lambda (e)
                    (%var-iterated-access-p
                     body (first e) nil
                     (if (eq (cdddr e) :char)
                         (function %tree-finds-schar-p)
                         (function %tree-finds-aref-p))))
                  entries))
      (funcall thunk)
      (let ((slow (gen-label "ABUSLOW"))
            (done (gen-label "ABUDONE")))
        `(,@(loop for e in entries
                  append `((:ldloc ,(third e)) (:brfalse ,slow)))
          ,@(let ((*backing-arm* :fast)) (funcall thunk))
          (:br ,done)
          (:label ,slow)
          ,@(let ((*backing-arm* :helper)) (funcall thunk))
          (:label ,done)))))

(defun array-backing-prologue (entries)
  "Instructions that fetch each hoisted element buffer once, right after the
   variables are bound. At (safety 0) the fetch answers NIL when it declines --
   a fill-pointered or adjustable vector, whose storage VECTOR-PUSH-EXTEND and
   ADJUST-ARRAY replace, a displaced one -- and every access carries an arm that
   runs the per-element helper for exactly that case, so a declaration this
   cannot honour costs the speed and never the answer. Above (safety 0) such a
   value is a TYPE-ERROR from the fetch itself (see %BACKING-FETCH-CALL)."
  (loop for e in entries
        for kind = (cdddr e)
        append `((:declare-local ,(third e) ,(%backing-slot-type kind))
                 (:ldloc ,(second e))
                 (:call ,(%backing-fetch-call kind))
                 (:stloc ,(third e)))))

(defun %aref-arr-temp (arr)
  "Split the array operand of a numeric AREF into three instruction lists:
   what to DECLARE, what to evaluate UP FRONT, and how to LOAD it where the
   call wants it.

   A plain lexical local is pure, so it declares nothing, costs nothing up
   front and is simply loaded in place. A structure accessor call is not pure
   and must be evaluated FIRST, into a temp: the emitters below lower
   non-simple subscripts into temps before they load the array, and moving a
   call past a subscript would evaluate the two in the wrong order."
  (if (simple-expr-p arr)
      (values '() '()
              (let ((*in-tail-position* nil) (*in-mv-context* nil))
                (compile-expr arr)))
      (let ((tmp (gen-local "NAA")))
        (values `((:declare-local ,tmp "LispObject"))
                `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                      (compile-expr arr))
                  (:stloc ,tmp))
                `((:ldloc ,tmp))))))

(defun compile-numeric-aref-as-long (arr idxs rank)
  "Emit (aref ARR IDX...) on a numeric-backed array as a raw int64.

   When the variable has a hoisted element buffer, the read is a bare ldelem
   against it with the per-element helper as the other arm: the buffer slot is
   NIL whenever the fetch declined, and declining has to cost speed rather than
   correctness. Everything else compiles to the helper alone."
  (let ((helper (%numeric-aref-helper-as-long arr idxs rank))
        (back (and (= rank 1) (array-backing-entry (list* 'aref arr idxs)))))
    (if back
        (compile-backing-aref-as-long back (first idxs) helper)
        helper)))

(defun %numeric-aref-helper-as-long (arr idxs rank)
  "Emit (aref ARR IDX...) on a proven numeric-backed array as a raw int64 on
   the stack (Runtime.ArefNum*L). ARR is a plain lexical local or a structure
   accessor call (numeric-array-aref-info); non-simple indices are evaluated to
   Int64 temps first so the stack is empty whenever a non-simple subexpression
   compiles, and ARR goes into a temp ahead of them when it is not a local."
  (let ((method (ecase rank
                  (1 "Runtime.ArefNumL")
                  (2 "Runtime.ArefNum2DL")
                  (3 "Runtime.ArefNum3DL"))))
    (if (every #'simple-expr-p idxs)
        `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
              (compile-expr arr))
          ,@(loop for idx in idxs append (compile-index-to-long idx))
          (:call ,method))
        (multiple-value-bind (arr-decl arr-pre arr-load) (%aref-arr-temp arr)
          (let ((idx-tmps (mapcar (lambda (i) (declare (ignore i)) (gen-local "NAI"))
                                  idxs)))
            `(,@arr-decl
              ,@(mapcar (lambda (tk) `(:declare-local ,tk "Int64")) idx-tmps)
              ,@arr-pre
              ,@(loop for idx in idxs
                      for tk in idx-tmps
                      append `(,@(compile-index-to-long idx) (:stloc ,tk)))
              ,@arr-load
              ,@(mapcar (lambda (tk) `(:ldloc ,tk)) idx-tmps)
              (:call ,method)))))))

(defun compile-numeric-aref-set (arr idxs val rank)
  "Emit (setf (aref ARR IDX...) VAL) on a numeric-backed array, boxed value on
   the stack. Hoisted buffer with a helper arm, exactly as the read is."
  (let ((helper (%numeric-aref-helper-set arr idxs val rank))
        (back (and (= rank 1) (array-backing-entry (list* 'aref arr idxs)))))
    `(,@(if back
            (compile-backing-aref-set back (first idxs) val helper)
            helper)
      (:call "Fixnum.Make"))))

(defun %numeric-aref-helper-set (arr idxs val rank)
  "Emit (setf (aref ARR IDX...) VAL) on a numeric-backed array with the value
   lowered to a raw int64 (Runtime.ArefSetNum*L; range-checked store, returns
   the stored long). Leaves that RAW long on the stack; COMPILE-NUMERIC-AREF-SET
   boxes it for the general setf contract, and in statement position the
   peephole (P5 family) deletes the box."
  (let ((method (ecase rank
                  (1 "Runtime.ArefSetNumL")
                  (2 "Runtime.ArefSetNum2DL")
                  (3 "Runtime.ArefSetNum3DL"))))
    (if (and (every #'simple-expr-p idxs) (simple-expr-p val))
        `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
              (compile-expr arr))
          ,@(loop for idx in idxs append (compile-index-to-long idx))
          ,@(compile-expr-to-long val)
          (:call ,method))
        (multiple-value-bind (arr-decl arr-pre arr-load) (%aref-arr-temp arr)
          (let ((idx-tmps (mapcar (lambda (i) (declare (ignore i)) (gen-local "NAI"))
                                  idxs))
                (val-tmp (gen-local "NAV")))
            `(,@arr-decl
              ,@(mapcar (lambda (tk) `(:declare-local ,tk "Int64")) idx-tmps)
              (:declare-local ,val-tmp "Int64")
              ,@arr-pre
              ,@(loop for idx in idxs
                      for tk in idx-tmps
                      append `(,@(compile-index-to-long idx) (:stloc ,tk)))
              ,@(compile-expr-to-long val)
              (:stloc ,val-tmp)
              ,@arr-load
              ,@(mapcar (lambda (tk) `(:ldloc ,tk)) idx-tmps)
              (:ldloc ,val-tmp)
              (:call ,method)))))))

(defun compile-numeric-aref-float (arr idxs rank)
  "Emit (aref ARR IDX...) on a proven float-backed numeric array as a native
   r8 (double) on the stack (Runtime.ArefNum*D). Mirrors
   compile-numeric-aref-as-long but leaves a raw double and never boxes.
   single-float backing widens to double here; callers narrow with conv.r4."
  (let ((method (ecase rank
                  (1 "Runtime.ArefNumD")
                  (2 "Runtime.ArefNum2DD")
                  (3 "Runtime.ArefNum3DD"))))
    (if (every #'simple-expr-p idxs)
        `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
              (compile-expr arr))
          ,@(loop for idx in idxs append (compile-index-to-long idx))
          (:call ,method))
        (multiple-value-bind (arr-decl arr-pre arr-load) (%aref-arr-temp arr)
          (let ((idx-tmps (mapcar (lambda (i) (declare (ignore i)) (gen-local "NAI"))
                                  idxs)))
            `(,@arr-decl
              ,@(mapcar (lambda (tk) `(:declare-local ,tk "Int64")) idx-tmps)
              ,@arr-pre
              ,@(loop for idx in idxs
                      for tk in idx-tmps
                      append `(,@(compile-index-to-long idx) (:stloc ,tk)))
              ,@arr-load
              ,@(mapcar (lambda (tk) `(:ldloc ,tk)) idx-tmps)
              (:call ,method)))))))

(defun compile-numeric-aref-set-float (arr idxs val rank kind)
  "Emit (setf (aref ARR IDX...) VAL) on a float-backed numeric array. VAL is
   lowered to a native r8 (single-typed values widen with conv.r8) and stored
   via Runtime.ArefSetNum*D (narrows to float for single backing). Leaves the
   value BOXED for the setf contract; peephole removes it in statement position."
  (let ((method (ecase rank
                  (1 "Runtime.ArefSetNumD")
                  (2 "Runtime.ArefSetNum2DD")
                  (3 "Runtime.ArefSetNum3DD")))
        (box (ecase kind
               (:single '((:conv-r4) (:newobj "SingleFloat")))
               (:double '((:newobj "DoubleFloat"))))))
    (flet ((val-as-double ()
             (if (eq kind :single)
                 `(,@(compile-as-single val) (:conv-r8))
                 (compile-as-double val))))
      (if (and (every #'simple-expr-p idxs) (simple-expr-p val))
          `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                (compile-expr arr))
            ,@(loop for idx in idxs append (compile-index-to-long idx))
            ,@(val-as-double)
            (:call ,method)
            ,@box)
          (multiple-value-bind (arr-decl arr-pre arr-load) (%aref-arr-temp arr)
            (let ((idx-tmps (mapcar (lambda (i) (declare (ignore i)) (gen-local "NAI"))
                                    idxs))
                  (val-tmp (gen-local "NAV")))
              `(,@arr-decl
                ,@(mapcar (lambda (tk) `(:declare-local ,tk "Int64")) idx-tmps)
                (:declare-local ,val-tmp "Double")
                ,@arr-pre
                ,@(loop for idx in idxs
                        for tk in idx-tmps
                        append `(,@(compile-index-to-long idx) (:stloc ,tk)))
                ,@(val-as-double)
                (:stloc ,val-tmp)
                ,@arr-load
                ,@(mapcar (lambda (tk) `(:ldloc ,tk)) idx-tmps)
                (:ldloc ,val-tmp)
                (:call ,method)
                ,@box)))))))

;;; LIST / LIST* build their conses inline for a small, fixed argument count.
;;;
;;; Both used to hand their arguments to a runtime function through an array, so
;;; the call cost 24 + 8n bytes on top of the conses it returned. Writing the same
;;; conses by hand already cost exactly the conses and nothing else. Measured
;;; against SBCL -- whose cons is 16 bytes to a .NET object's 32, so 2x is the
;;; floor here -- (cons a (cons b nil)) sat exactly on that floor while
;;; (list a b) was 40 bytes above it. LIST is in every library there is.
;;;
;;; Left-to-right argument order is preserved: nested CONS evaluates its car
;;; before its cdr. The bound keeps the emitted nesting from growing without
;;; limit; past it the array is the cheaper shape.

(defparameter +inline-list-max-args+ 8
  "Longest LIST / LIST* call whose conses are emitted inline instead of through
   the args-array runtime entry.")

(defun %nested-cons-form (args tail)
  "(CONS a1 (CONS a2 ... TAIL)) for ARGS = (a1 a2 ...)."
  (let ((form tail))
    (dolist (a (reverse args) form)
      (setq form `(cons ,a ,form)))))

(defun compile-list-call (args)
  (cond
    ((null args) (emit-nil))
    ((<= (length args) +inline-list-max-args+)
     (compile-expr (%nested-cons-form args nil)))
    (t `(,@(compile-args-array args)
         (:call "Runtime.List")
         (:call "MultipleValues.Primary")))))

(defun compile-list-star-call (args)
  (cond
    ((null args) (error "LIST* requires at least one argument"))
    ;; (LIST* x) is x. Compiling it as such removes a call, and there is no cons
    ;; to build.
    ((null (cdr args)) (compile-for-single-value (car args)))
    ((<= (length args) +inline-list-max-args+)
     (compile-expr (%nested-cons-form (butlast args) (car (last args)))))
    (t `(,@(compile-args-array args)
         (:call "Runtime.ListStar")))))

(defun compile-gethash (args)
  (let ((nargs (length args)))
    (cond
      ((< nargs 2) (compile-static-program-error "GETHASH: too few arguments (expected 2-3)"))
      ((> nargs 3) (compile-static-program-error "GETHASH: too many arguments (expected 2-3)"))
      (t
       ;; The arguments are single-valued wherever the GETHASH form is: a
       ;; table from a function that returns more than one value is its
       ;; primary value, also when the GETHASH is in tail position or inside
       ;; MULTIPLE-VALUE-BIND.
       `(,@(compile-value-args (list (first args) (second args)
                                     (if (= nargs 3) (third args) '(:code (:ldnull)))))
         (:call "Runtime.Gethash"))))))

(defun compile-value-args (items)
  "Push the single value of each of ITEMS, left to right, for a known-function
   intrinsic call. An item is a form, or (:CODE . instructions) pushed as is.
   Code that leaves a protected region or opens one (CATCH, HANDLER-CASE,
   UNWIND-PROTECT, a loop exited with LEAVE) needs an empty evaluation stack,
   and the values of the items before it would be on it, so when any item after
   the first has such code every value is taken into a temp first."
  (let ((codes (let ((*in-tail-position* nil) (*in-mv-context* nil))
                 (mapcar (lambda (it)
                           (if (and (consp it) (eq (car it) :code))
                               (cdr it)
                               (compile-expr it)))
                         items))))
    (if (notany #'%instrs-need-empty-stack-p (cdr codes))
        (apply #'append codes)
        (let ((temps (loop repeat (length codes) collect (gen-local "VA"))))
          (append (loop for code in codes
                        for tmp in temps
                        append `((:declare-local ,tmp "LispObject") ,@code (:stloc ,tmp)))
                  (loop for tmp in temps collect `(:ldloc ,tmp)))))))

(defun compile-puthash (args)
  "Compile (puthash key table value)."
  `(,@(compile-value-args args)
    (:call "Runtime.Puthash")))

(defun compile-values-call (args)
  (cond
    ;; (values): a shared marker, no array and no wrapper to build.
    ((null args) '((:call "MultipleValues.Values0")))
    ;; (values x) is "the primary value of x, published as the only value", which
    ;; is exactly what MULTIPLEVALUES.PRIMARY does -- and it publishes through a
    ;; per-thread cached array, so nothing is allocated. Going through the general
    ;; entry built a LispObject[1] the callee then threw away, 32 bytes a call.
    ((null (cdr args))
     `(,@(let ((*in-mv-context* t)) (compile-expr (car args)))
       (:call "MultipleValues.Primary")))
    ;; In a tail position with a value mode (the tail of a body whose caller
    ;; passed one, or the form of a capture in this body): the values in the
    ;; form the mode names, without an MvReturn (MultipleValues.ValuesNMode).
    ((and *in-tail-position* (<= 2 (length args) 4) (%mv-mode-local))
     (let* ((da (compile-direct-call-args args))
            (temps (car da)))
       `(,@(cdr da)
         ,@(loop for tmp in temps append `((:ldloc ,tmp)))
         ,@(%mv-mode-push)
         (:call ,(ecase (length args)
                   (2 "MultipleValues.Values2Mode")
                   (3 "MultipleValues.Values3Mode")
                   (4 "MultipleValues.Values4Mode"))))))
    ;; Two values: the shape TRUNCATE, FLOOR, ROUND, GETHASH and INTERN return, and
    ;; the one the argument array cost the most on (40 of the 64 bytes a
    ;; multiple-value return allocated). RUNTIME.VALUES2 takes them as arguments.
    ((= (length args) 2)
     `(,@(compile-binary-call args "Runtime.Values2")))
    (t `(,@(compile-args-array args)
         (:call "Runtime.Values")))))

(defun compile-subseq (args)
  "Compile (subseq seq start &optional end)."
  (let ((nargs (length args)))
    (cond
      ((or (< nargs 2) (> nargs 3))
       ;; Wrong number of args -> PROGRAM-ERROR
       (compile-expr '(error 'program-error)))
      (t
       ;; Single-valued arguments wherever the SUBSEQ form is (see GETHASH).
       `(,@(compile-value-args (list (first args) (second args) (third args)))
         (:call "Runtime.Subseq"))))))

(defun compile-concatenate (args)
  "Compile (concatenate result-type seq1 seq2 ...)."
  (if (null args)
      (compile-expr '(error 'program-error))
      (let ((rt-tmp (gen-local "RTTYP"))
            (arr-tmp (gen-local "CATARR")))
        `((:declare-local ,rt-tmp "LispObject")
          (:declare-local ,arr-tmp "LispObject[]")
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args)))
          (:stloc ,rt-tmp)
          ,@(compile-args-array (cdr args))
          (:stloc ,arr-tmp)
          (:ldloc ,rt-tmp)
          (:ldloc ,arr-tmp)
          (:call "Runtime.Concatenate")))))

(defun compile-args-array (args)
  "Compile arguments into a LispObject[] array on the stack.
   Pre-evaluates all args to temps so the stack is empty during
   each compile-expr (CIL requires empty stack at try-block entry).
   Args are never in tail position and receive a single value each."
  (let ((n (length args)))
    (if (zerop n)
        `((:ldc-i4 0) (:newarr "LispObject"))
        (let ((temps (loop for a in args collect (gen-local "AA"))))
          `(,@(loop for arg in args
                    for tmp in temps
                    append `((:declare-local ,tmp "LispObject")
                             ,@(let ((*in-tail-position* nil)
                                     (*in-mv-context* nil))
                                 (compile-expr arg)) (:stloc ,tmp)))
            (:ldc-i4 ,n)
            (:newarr "LispObject")
            ,@(loop for tmp in temps
                    for i from 0
                    append `((:dup) (:ldc-i4 ,i) (:ldloc ,tmp) (:stelem-ref))))))))
