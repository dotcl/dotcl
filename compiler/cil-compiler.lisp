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

;; %INLINE-CS-SPLICED is the dispatch symbol for the dotnet-cs:inline-cs
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

(defvar *array-backing-tested* '()
  "The BACKING-SYMBOLs of the element buffers the enclosing UNSWITCH-ARRAY-
   BACKING tested before taking its :FAST copy. *BACKING-ARM* is :FAST for
   everything compiled inside that copy, including an access through a buffer a
   nested LET fetched itself, which nobody tested: such an access keeps its own
   test (see %BACKING-ARM-FOR). An alias of a tested buffer shares its symbol
   and so counts as tested.")

(defvar *raw-array-plan* '()
  "Entries (KEY BACKING KIND N NAME) for the LET bindings in scope whose
   MAKE-ARRAY is held only as the bare .NET array in BACKING (%RAW-ARRAY-PLAN).")

(defvar *raw-array-backings* '()
  "The BACKING symbols of *RAW-ARRAY-PLAN*. Such a buffer is never null.")

(defvar *bounded-index* '()
  "Entries (KEY . REFS): in the body of a counting loop, the variable whose
   binding is slot KEY is a valid subscript of every buffer in REFS, a list of
   (BACKING-SYMBOL REF-LOCAL M): REF-LOCAL holds a reference to the buffer's
   first element, and M elements from the subscript on are in the buffer. The
   loop compared its limit with their lengths before it started.")

(defvar *bounded-loop-candidate* nil
  "(TAGBODY-FORM VAR LIMIT INCLUSIVE DELTA START) for the counting loop the
   enclosing LET is compiling (see %COMPILE-BOUNDED-LOOP). START is VAR's initial
   value when it is a known integer, else NIL.")

(defun %bounded-backing-ref (entry idxs &optional (span 1))
  "The element reference local for an (AREF V I) through ENTRY that
   *BOUNDED-INDEX* proves in range, SPAN elements from I on, or NIL."
  (and *bounded-index*
       (null (backing-entry-dims entry))
       (consp idxs) (null (cdr idxs))
       (let ((i (car idxs)))
         (and i (symbolp i)
              (not (boxed-var-p i))
              (not (nth-value 1 (lookup-symbol-macro i)))
              (let ((key (lookup-local i)))
                (and key
                     (loop for b in *bounded-index*
                           thereis (and (eq (car b) key)
                                        (let ((r (assoc (third entry) (cdr b) :test #'eq)))
                                          (and r (>= (third r) span) (second r)))))))))))

(defun %backing-ref-suffix (kind)
  (ecase kind (1 "U8") (2 "U16") (3 "I32") ((4 :u64) "I64") (:f64 "F64") (:f32 "F32")))

(defun %backing-ref-type (kind)
  (ecase kind (1 "Byte&") (2 "UInt16&") (3 "Int32&") ((4 :u64) "Int64&") (:f64 "Double&") (:f32 "Single&")))

(defun %backing-arm-for (entry)
  "The arm an access through ENTRY's buffer emits: *BACKING-ARM*, except that
   :FAST becomes :BOTH for a buffer that was not among those tested. The fetch
   of a rank 2 or 3 buffer never declines (it signals instead), so its accesses
   are always :FAST, as are those of a raw array (never null)."
  (cond ((backing-entry-dims entry) :fast)
        ((member (third entry) *raw-array-backings* :test #'eq) :fast)
        ((and (eq *backing-arm* :fast)
              (not (member (third entry) *array-backing-tested* :test #'eq)))
         :both)
        (t *backing-arm*)))

(defun %raw-array-entry-p (entry)
  (and entry (member (third entry) *raw-array-backings* :test #'eq) t))

(defvar *struct-backing-known* '()
  "Structure backing entries (see STRUCT-BACKING-ENTRY) whose raw slot array the
   code being compiled is known to have, and to be long enough: the copy of a
   body that UNSWITCH-ARRAY-BACKING emits behind that test. An access through
   one of these is the bare element load or store, with no null arm and no
   fallback call. Kept apart from *BACKING-ARM* because it names the entries
   that were actually tested: a binding outside the unswitched body was not.")

(defun struct-backing-known-p (entry)
  (and (member entry *struct-backing-known* :test #'eq) t))

(defvar *compile-file-mode* nil
  "T when compiling via compile-file. Controls eval-when behavior per CLHS 3.2.3.1:
   :compile-toplevel -> eval at compile time, :load-toplevel -> emit CIL for load.")

(defvar *same-file-functions* nil
  "While COMPILE-FILE compiles one file: the DEFUNs compiled so far from it
   whose body a call can enter directly while the name still holds the function
   the file defined (name -> number of arguments).")

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
;; The symbols themselves are uninterned (MAKE-SYMBOL), so no shared state is
;; touched at all. Left as plain incf to avoid per-gensym atomic overhead on
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

(defvar *typed-vector-struct-accessors* (make-hash-table :test #'eq :synchronized t)
  "Accessor symbol -> slot index, for the readers of a DEFSTRUCT with
   (:TYPE VECTOR) or (:TYPE (VECTOR ELT)). Such a reader is defined as
   (DEFUN ACC (OBJ) (AREF OBJ IDX)), and a call to it is compiled as that AREF
   (SBCL does the same with a source transform), so the typed paths see the
   element type of a vector declared (SIMPLE-ARRAY (UNSIGNED-BYTE 64) (8)) and
   so on instead of a full call returning an unknown object. Recorded by the
   DEFSTRUCT expansion after the reader DEFUNs, at compile and load time, and
   dropped when the name is redefined by another DEFUN.")

(defun %register-typed-vector-struct-accessors (accessors indices)
  "Record ACCESSORS, the readers of a (:TYPE VECTOR) structure, with the slot
   INDICES they read (see *TYPED-VECTOR-STRUCT-ACCESSORS*)."
  (loop for acc in accessors
        for idx in indices
        do (setf (gethash acc *typed-vector-struct-accessors*) idx)))

(defvar *function-arg-types* (make-hash-table :test #'eq :synchronized t)
  "symbol -> the argument type list of its declaimed FTYPE, (FUNCTION ARGS RET)'s
   ARGS as written. Recorded beside *FUNCTION-RETURN-TYPES* by DECLAIM and by a
   loaded fasl's PROCLAIM. A DEFUN of the name whose arguments and result are all
   numeric types gets a typed entry (TYPED-ENTRY-SPECS).")

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

(defun %setf-function-name-p (name)
  "Is NAME a function name of the form (SETF symbol)?"
  (and (consp name) (eq (car name) 'setf)
       (consp (cdr name)) (symbolp (cadr name)) (null (cddr name))))

(defun extract-notinline (body)
  "Collect function names from (declare (notinline f ...)) forms at the head of
   BODY. Returns a list of function names (possibly empty): symbols, and
   (SETF symbol) lists, which only MAYBE-EXPAND-INLINE looks for."
  (let ((result '())
        (rest body))
    (loop while (and rest (consp (car rest)) (eq (caar rest) 'declare))
          do (dolist (decl (cdar rest))
               (when (and (consp decl) (eq (car decl) 'notinline))
                 (dolist (f (cdr decl))
                   (when (or (symbolp f) (%setf-function-name-p f))
                     (push f result)))))
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

(defvar *inline-return-ranges* (make-hash-table :test #'eq :synchronized t)
  "F -> (LO . HI) for a function F whose definition is in *INLINE-DEFS* and
   whose result form was proved, where the DEFUN was compiled, to be an integer
   in that range (%RECORD-INLINE-RETURN-RANGE). CALL-INT-RETURN-RANGE answers it
   for a call that is substituted.")

(defvar *inline-defs* (make-hash-table :test #'equal :synchronized t)
  "Function name (a symbol, or a list (SETF symbol)) -> (LAMBDA-LIST . BODY)
   for functions whose DEFUN was
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

;;; RyuJIT compiles a method without optimization (MinOpts: no inlining, no loop
;;; optimization, no register allocation across statements) when it is past any
;;; of 60,000 IL bytes, 20,000 IL instructions, 2,000 basic blocks, 2,000 locals
;;; or 8,000 references to locals and arguments. These are constants of the JIT,
;;; the same from .NET 6 to 10, and a release runtime has no setting that moves
;;; them; bench/jit-minopts counts the methods that cross them. Code that copies
;;; or expands a body takes its size budget from here.
(defconstant +jit-optimize-max-il-instructions+ 20000
  "IL instructions past which RyuJIT stops optimizing a method.")

(defconstant +jit-optimize-max-il-bytes+ 60000)
(defconstant +jit-optimize-max-local-refs+ 8000)
(defconstant +jit-optimize-max-basic-blocks+ 2000)

(defun %sil-stack-temp-effect (op)
  "(POPS . PUSHES) of OP for the assembler's single-use temporary pass
   (CilAssembler.StackTemps.cs), or NIL where that pass stops."
  (case op
    ((:declare-local :line) '(0 . 0))
    ((:not :neg :conv-i :conv-i4 :conv-i8 :conv-u :conv-u1 :conv-u2 :conv-u4 :conv-u8
      :conv-i1 :conv-i2 :conv-r8 :conv-r4 :conv-r-un :castclass :isinst :ldlen)
     '(1 . 1))
    ((:ldloc :ldarg :ldc-i8 :ldc-i4 :ldc-r8 :ldc-r4 :ldnull
      :load-sym :load-sym-pkg :load-sym-keyword :load-const)
     '(0 . 1))
    (:dup '(1 . 2))
    ((:stloc :pop) '(1 . 0))
    ((:add :sub :mul :div :rem :div-un :rem-un :and :or :xor :shl :shr :shr-un
      :ceq :cgt :clt :cgt-un :clt-un :ldelem-i8 :ldelem-i4 :ldelem-u1 :ldelem-ref
      :ldelem-r8 :ldelem-r4 :ldelem-i1 :ldelem-i2 :ldelem-u2 :ldelem-u4 :ldelem-i)
     '(2 . 1))
    (t nil)))

(defun %sil-stack-temp-pairs (instrs)
  "How many STLOC / LDLOC pairs the assembler's single-use temporary pass would
   drop from INSTRS: the same rules, on this list instead of the whole method."
  (let* ((ins (coerce instrs 'vector))
         (n (length ins))
         (declared (make-hash-table :test #'eq))
         (loads (make-hash-table :test #'eq))
         (stores (make-hash-table :test #'eq))
         (spoiled (make-hash-table :test #'eq)))
    (loop for i across ins
          when (and (consp i) (eq (car i) :declare-local))
            do (setf (gethash (cadr i) declared) t))
    (labels ((spoil (x)
               (cond ((consp x) (spoil (car x)) (spoil (cdr x)))
                     ((and x (symbolp x) (gethash x declared))
                      (setf (gethash x spoiled) t)))))
      (dotimes (k n)
        (let ((i (aref ins k)))
          (when (and (consp i) (not (eq (car i) :declare-local)))
            (if (and (member (car i) '(:ldloc :stloc)) (consp (cdr i)) (null (cddr i))
                     (gethash (cadr i) declared))
                (let ((tab (if (eq (car i) :ldloc) loads stores)))
                  (if (gethash (cadr i) tab)
                      (setf (gethash (cadr i) spoiled) t)
                      (setf (gethash (cadr i) tab) k)))
                (unless (eq (car i) :load-const) (spoil (cdr i))))))))
    (let ((cands '()) (removed (make-array n :initial-element nil)) (pairs 0))
      (maphash (lambda (nm st)
                 (let ((ld (gethash nm loads)))
                   (when (and ld (not (gethash nm spoiled)) (< st ld) (<= (- ld st) 96))
                     (push (cons st ld) cands))))
               stores)
      (setf cands (sort cands #'< :key #'car))
      (loop repeat 8
            for changed = nil
            do (dolist (c cands)
                 (unless (aref removed (car c))
                   (let ((depth 0) (ok t))
                     (loop for k from (1+ (car c)) below (cdr c)
                           unless (aref removed k)
                             do (let ((e (let ((i (aref ins k)))
                                           (and (consp i) (%sil-stack-temp-effect (car i))))))
                                  (when (or (null e) (< depth (car e)))
                                    (setf ok nil) (return))
                                  (incf depth (- (cdr e) (car e)))))
                     (when (and ok (= depth 0))
                       (setf (aref removed (car c)) t (aref removed (cdr c)) t
                             changed t)
                       (incf pairs)))))
               (unless changed (return)))
      pairs)))

(defun %sil-past-jit-limits-p (instrs)
  "True when the method code INSTRS would be past one of the JIT's limits, by an
   estimate from the SIL. The bodies of nested functions, methods of their own,
   are not counted."
  (let ((n 0) (bytes 0) (blocks 0) (refs 0))
    (labels ((walk (list)
               (dolist (i list)
                 (when (and (consp i) (keywordp (car i)))
                   (case (car i)
                     ((:declare-local))
                     (:label (incf blocks))
                     ((:make-function :make-function-direct :make-closure)
                      (incf n) (incf bytes 5))
                     (t
                      (incf n)
                      (incf bytes
                            (case (car i)
                              (:ldc-i8 9)
                              ((:ldloc :stloc :ldarg :starg) (incf refs) 2)
                              ((:call :callvirt :newobj :br :brtrue :brfalse :leave
                                :ldc-i4 :ldstr :ldsfld :stsfld :ldfld :stfld :castclass :isinst)
                               5)
                              (:unbox-fixnum (incf n) 10)
                              (t 1)))
                      (dolist (a (cdr i))
                        (when (and (consp a) (consp (car a)) (keywordp (caar a)))
                          (walk a)))))))))
      (walk instrs))
    (let ((pairs (%sil-stack-temp-pairs instrs)))
      (or (> (- n (* 2 pairs)) +jit-optimize-max-il-instructions+)
          (> (- refs (* 2 pairs)) +jit-optimize-max-local-refs+)
          (> (- bytes (* 4 pairs)) +jit-optimize-max-il-bytes+)
          (> blocks +jit-optimize-max-basic-blocks+)))))

(defvar *unswitch-body-limit* (floor +jit-optimize-max-il-instructions+ 8)
  "Most instructions UNSWITCH-ARRAY-BACKING copies regardless of size. The
   unswitch emits a body twice; a body this large is not the tight loop the copy
   is for, and two of it push the method toward the JIT's limits, past which the
   whole method runs unoptimized. Past this the smaller shape is taken.")

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
   Populated in cil-forms-dispatch.lisp after all compile-* helpers are defined.
   Provides O(1) dispatch for ~250+ common operators in compile-form.")

(defvar *macroexpand-cache* nil
  "A cons whose car is the hash table (form -> ((scope . expansion) ...)) for
   memoizing macro expansions within one top-level compile, made on first use.
   Keyed by eq (cons-cell identity) on the form so
   that the same source form object is never expanded twice even when it appears
   in both the analysis pass and the code-gen pass.  NIL outside a compile-toplevel
   call, which disables caching (eval-time macro calls are not cached).")

(defvar *macrolet-source-defs* '()
  "The definition lists of the enclosing MACROLETs at the current lexical
   position, innermost first, as written in the source. A DEFUN proclaimed
   INLINE inside a MACROLET whose body uses those macros is recorded wrapped in
   the same MACROLETs (%INLINE-BODY-WITH-MACROLETS), so it can be substituted
   outside them.")

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

(defvar *lexical-decl-frames* '()
  "The binding and declaration scopes around the current position, innermost
   first, for the CLtL2 environment queries. Each is (KIND NAMES . BODY): KIND
   tells how to read the names NAMES binds, and BODY is the scope's body as
   written, whose declarations are parsed only when a query asks.")

(defun %push-decl-frame (kind names body)
  "*LEXICAL-DECL-FRAMES* with the frame (KIND NAMES . BODY) added, to bind it to."
  (cons (list* kind names body) *lexical-decl-frames*))

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

(defvar *expansion-sm-dependent* nil
  "Set to T while a macro expander runs in the mutation walk, when it was given
   an &ENVIRONMENT there while symbol macros are visible. That walk does not
   track every variable binding inside the form it walks (lambda lists, for
   one), so a variable that shadows one of those symbol macros may be missing
   from the environment, and an expander that asks the environment about the
   name may answer differently than where the form is compiled.
   CACHED-MACROEXPAND then keeps the expansion together with the resolution of
   every visible symbol macro name (%SM-SNAPSHOT) and reuses it only where the
   names resolve the same way. An expansion embedding such an expansion is kept
   the same way.")

(defvar *sm-dependent-marker* (list '#:sm-dependent)
  "Heads the value of a *MACROEXPAND-CACHE* entry kept under
   *EXPANSION-SM-DEPENDENT*: (MARKER EXPANSION . SNAPSHOT), SNAPSHOT from
   %SM-SNAPSHOT.")

(defvar *expansion-unreusable* nil
  "Set to T by a macro expander whose result depends on the type declarations
   in scope where the form is expanded (DOTIMES asks whether its count form is
   a fixnum). The analysis walks expand forms before the bindings around them
   are established -- a LET's walk sees its body in the context outside the
   LET -- so an expansion they make may answer for the wrong binding of a
   name. CACHED-MACROEXPAND then does not store the result (nor that of an
   expansion embedding it), and the compiler expands the form again where it
   compiles it. Unlike *EXPANSION-UNCACHEABLE*, the analysis summaries of a
   form containing such an expansion are still kept: the declarations decide
   only the types in the expansion, not which variables it reads or assigns.")

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
;; interpreter defines it (cil-forms-interp.lisp).
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

(defvar *type-env-epoch* 0
  "Counts the points where the compilation environment the integer-type
   predicates read may have changed: every COMPILE-FORM and COMPILE-EXPR-RAW,
   and every macro expander run. *INT-TYPE-SPAN* drops its answers when this
   moves.")

(defvar *int-type-span* nil
  "Answers of the integer-type predicates kept across the queries of one
   stretch of native int64 compilation: a vector (EPOCH INT-TYPE-MEMO
   LONG-DECL-MEMO), bound by the outermost COMPILE-LONG-BINOP /
   COMPILE-LONG-BINOP-SPILLED. Compiling (+ 1 (+ 1 ...)) asks at every level
   about the whole operand below it, and each of those was a query of its own
   that walked the tree again, so N levels cost N^2.
   An answer depends on the form, *FIXNUM-WRAP-LICENSED* (part of the key) and
   the compilation environment: declarations, local functions and macros,
   native slots, proclamations. None of that changes while the operands of a
   native chain are compiled without going through COMPILE-FORM: whatever binds
   or proclaims anything runs inside COMPILE-FORM. So COMPILE-FORM binds this
   to NIL for its extent, and the epoch (*TYPE-ENV-EPOCH*) empties the tables
   once anything has been compiled or expanded since they were filled.")

(defun %int-type-span-table (index)
  "Table INDEX (1: INT-TYPE-MEMO, 2: LONG-DECL-MEMO) of *INT-TYPE-SPAN*, made
   on first use and made anew when the environment may have changed since it
   was filled; NIL outside a span."
  (let ((s *int-type-span*))
    (when s
      (unless (eql (svref s 0) *type-env-epoch*)
        (setf (svref s 0) *type-env-epoch*
              (svref s 1) nil
              (svref s 2) nil))
      (or (svref s index)
          (setf (svref s index) (make-hash-table :test 'eq))))))

(defun cached-macroexpand (form expander)
  "Expand FORM using EXPANDER, memoizing the result per (FORM, *macroexpand-scope*)
   in *macroexpand-cache*.  If the cache is NIL (outside a compile-toplevel) falls
   through to a plain funcall so that eval / macrolet restore paths are unaffected.
   An expansion kept under *EXPANSION-SM-DEPENDENT* is reused only where the
   visible symbol macro names resolve as they did when it was made."
  (if *macroexpand-cache*
      (let ((by-scope (let ((table (car *macroexpand-cache*)))
                        (if table (gethash form table '()) '())))
            (sig :unset))
        (dolist (c by-scope)
          (when (%macroexpand-scope= (car c) *macroexpand-scope*)
            (let ((v (cdr c)))
              (if (and (consp v) (eq (car v) *sm-dependent-marker*))
                  (progn
                    (when (eq sig :unset) (setf sig (%sm-snapshot)))
                    (when (%sm-snapshot-match-p sig (cddr v))
                      (setf *expansion-sm-dependent* t)
                      (return-from cached-macroexpand (cadr v))))
                  (return-from cached-macroexpand v)))))
        (let ((result nil) (uncacheable nil) (unreusable nil) (dependent nil))
          (let ((*expansion-uncacheable* nil)
                (*expansion-unreusable* nil)
                (*expansion-sm-dependent* nil))
            (incf *type-env-epoch*)
            (setf result (with-phase "macroexpand" (funcall expander form))
                  uncacheable *expansion-uncacheable*
                  unreusable *expansion-unreusable*
                  dependent *expansion-sm-dependent*))
          ;; An enclosing expansion that embeds this one is just as
          ;; context-dependent, so it must not be cached either.
          (when uncacheable (setf *expansion-uncacheable* t))
          (when unreusable (setf *expansion-unreusable* t))
          (when dependent (setf *expansion-sm-dependent* t))
          (unless (or uncacheable unreusable)
            (setf (gethash form (or (car *macroexpand-cache*)
                                    (setf (car *macroexpand-cache*)
                                          (make-hash-table :test #'eq))))
                  (cons (cons *macroexpand-scope*
                              (if dependent
                                  (list* *sm-dependent-marker* result
                                         (if (eq sig :unset) (%sm-snapshot) sig))
                                  result))
                        by-scope)))
          result))
      (progn (incf *type-env-epoch*)
             (funcall expander form))))

;;; A macro whose expander signals an ERROR while a form is compiled does not
;;; stop the compilation. As SBCL does, the error is reported and the macro form
;;; is compiled into one that signals a PROGRAM-ERROR (COMPILED-PROGRAM-ERROR)
;;; saying so when it is evaluated. So (eval '(ignore-errors (bad) 'x)) returns
;;; NIL and that condition, and a function whose body uses the macro is still
;;; defined and signals only when called.
;;;
;;; Under COMPILE-FILE (*COMPILE-FILE-MODE*) the report is a WARNING, so
;;; COMPILE-FILE answers T for both WARNINGS-P and FAILURE-P, and the rest of the
;;; file is still compiled. Elsewhere (EVAL, COMPILE, LOAD of a source file) the
;;; report is written to *ERROR-OUTPUT* without signalling, as SBCL prints it.
;;;
;;; The expander's own error comes out unchanged where SBCL lets it out too: for
;;; a macro form at top level of what EVAL is given (SBCL's evaluator expands
;;; those itself), for code COMPILE-FILE evaluates at compile time only (EVAL-WHEN
;;; :COMPILE-TOPLEVEL), and while cross-compiling the compiler, where an error is
;;; a broken build.
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
   stands in for FORM in the compiled code."
  (let ((form-text (or (ignore-errors
                        (let ((*print-length* 10) (*print-level* 4))
                          (prin1-to-string form)))
                       "#<form>"))
        (message (or (ignore-errors (princ-to-string condition))
                     "#<error>")))
    ;; The wording is SBCL's, so a log reads the same under both.
    (if *compile-file-mode*
        (progn
          (incf *macroexpansion-failures*)
          (warn "caught ERROR:~%  (during macroexpansion of ~A)~%  ~A" form-text message))
        (ignore-errors
         (format *error-output* "~&; caught ERROR:~%;   (during macroexpansion of ~A)~%;   ~A~%"
                 form-text message)))
    `(error 'compiled-program-error
            :format-control "Execution of a form compiled with errors.~%Form:~%  ~A~%Compile-time error:~%  (during macroexpansion of ~A)~%~A"
            :format-arguments (list ,form-text ,form-text ,message))))

(defun %macroexpand-for-compile (form expander)
  "CACHED-MACROEXPAND, except that an ERROR from the expander becomes a report
   and a stand-in form (see %MACROEXPANSION-FAILED) where SBCL does the same."
  (if (if *compile-file-mode*
          (not *evaluating-at-compile-time*)
          (not (or *cross-compiling* *compile-was-toplevel*)))
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

(defun %note-mv-tail-function (name)
  (when (symbolp name)
    (setf (gethash name *mv-tail-functions*) t)))

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
(defparameter +cs-u64-locals+ 28)
(defparameter +cs-typed-self+ 29)
(defparameter +cs-simd-locals+ 30)
(defparameter +cstate-empty+
  (vector '() '() '() '() '() '() '() nil nil nil nil nil nil nil nil nil nil nil
          '() '() '() '() '() nil '() '() '() nil '() nil '()))

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

(defun cstate-typed-self ()
  "While compiling the body of a typed entry (see TYPED-ENTRY-SPECS): the list
   (SYMBOL ARG-SPECS RET-SPEC), whose calls from this body to SYMBOL go straight
   to the body method with raw arguments and a raw result. NIL elsewhere,
   including the bodies of closures and local functions inside it."
  (svref *cstate* +cs-typed-self+))

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

(defvar *single-assumed* nil
  "Alist (VAR . SINGLE-P) of the variables of the forms SINGLE-FLOAT-TYPED-P is
   looking inside (a LET's bindings, a substituted local function's
   parameters), innermost first.")

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

(defun cstate-u64-locals ()
  "List of SLOT KEYS whose local slots hold the bits of an (unsigned-byte 64)
   as a raw Int64 instead of a boxed integer (a Bignum at or above 2^63). Set by
   compile-let for (unsigned-byte 64)-declared, non-special, non-captured
   lexicals with an init. The 64-bit lane reads the slot as an operand already
   known to lie in [0, 2^64), COMPILE-SETQ stores the bits of the new value
   (checked to be an (unsigned-byte 64)), and COMPILE-VAR-REF boxes with
   Runtime.BoxU64 where the value crosses into generic code."
  (svref *cstate* +cs-u64-locals+))

(defun cstate-simd-locals ()
  "Alist (SLOT-KEY . KIND) of locals whose slots hold a raw Vector128 of the
   DOTCL-SIMD pack type KIND (a +SIMD-KINDS+ entry) instead of a boxed pack."
  (svref *cstate* +cs-simd-locals+))

(defun simd-native-local (sym)
  "(SLOT-KEY . KIND) when SYM's slot holds a raw Vector128, else NIL."
  (and (boundp '*cstate*)
       (symbolp sym)
       (cstate-simd-locals)
       (not (boxed-var-p sym))
       (let ((key (lookup-local sym)))
         (and key (assoc key (cstate-simd-locals))))))

(defun u64-native-local-p (sym)
  "The slot key when SYM's slot holds (unsigned-byte 64) bits, else NIL."
  (and (boundp '*cstate*)
       (native-slot-p sym (cstate-u64-locals))))

(defun cstate-array-backing-locals ()
  "Alist (NAME-STRING KEY BACKING-SYMBOL KIND NARROW) of locals declared
   (SIMPLE-ARRAY <integer type> (DIM)) whose element buffer was fetched once at
   binding time into BACKING-SYMBOL, a slot of the concrete array type. KIND is
   the runtime _numKind (1 = byte[], 2 = ushort[], 3 = int[], 4 = long[]).
   NARROW is the element type's width when the buffer is wider than the type
   (%ELEMENT-NARROW-WIDTH), which a store checks, else NIL.
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
  "Generate a fresh local variable symbol named PREFIX_N.
   The symbol is uninterned: interning it put every temporary of every form
   ever compiled into DOTCL.CIL-COMPILER for good, so a long-running image grew
   without bound. Nothing looks one up by name in a package: the compiler keeps
   the symbol itself, and the assembler keys locals by SYMBOL-NAME, which the
   counter keeps unique within a method.
   PREFIX is required (every call site passes one) so the lambda list stays
   required-only and the compiler routes calls through the direct-delegate
   fast path instead of the args-array entry."
  (let ((sym (make-symbol (format nil "~a_~d" prefix (incf *var-counter*)))))
    sym))

(defvar *compiler-name-package* (find-package "DOTCL.CIL-COMPILER"))

(defvar *compiler-name-syms* nil
  "A cons whose car is a table, made on first use, of name string -> symbol for
   the names COMPILER-NAME-SYM makes up while one top-level form is compiled.
   Bound by the outermost COMPILE-TOPLEVEL /
   COMPILE-TOPLEVEL-EVAL on the compiling thread and shared by compiles nested
   in it, so within one compile a name always gives the same symbol, and two
   threads never share a table. NIL outside a compile.")

(defvar *make-unregistered-symbol*
  ;; A runtime primitive, absent on the SBCL cross-compile host; resolved by name
  ;; at load time for the same reason as *UNINTERNED-VAR-COUNTER* below. The
  ;; host keeps NIL and interns, which costs nothing in a one-shot build.
  (let ((s (and (find-package "DOTCL-INTERNAL")
                (find-symbol "%MAKE-UNREGISTERED-SYMBOL" "DOTCL-INTERNAL"))))
    (and s (fboundp s) (symbol-function s))))

(defun compiler-name-sym (name)
  "The symbol for NAME, a name the compiler builds to stand for a binding of its
   own (a LABELS function's box, a block tag or tagbody id), as it would be if
   interned in DOTCL.CIL-COMPILER: an existing symbol of that name there is
   returned as it is, and otherwise the symbol's home package is
   DOTCL.CIL-COMPILER, which the name-matching of LOCAL-ENTRY relies on. But a
   new name is not added to the package: inside a compile it comes from
   *COMPILER-NAME-SYMS*, so names built from gensyms do not pile up in the
   package for as long as the image runs."
  (multiple-value-bind (sym status) (find-symbol name *compiler-name-package*)
    (cond (status sym)
          ((and *compiler-name-syms* *make-unregistered-symbol*)
           (let ((table (or (car *compiler-name-syms*)
                            (setf (car *compiler-name-syms*)
                                  (make-hash-table :test #'equal)))))
             (or (gethash name table)
                 (setf (gethash name table)
                       (funcall *make-unregistered-symbol* name *compiler-name-package*)))))
          (t (intern name *compiler-name-package*)))))


(defvar *uninterned-var-names* (make-hash-table :test #'eq :synchronized t)
  "Uninterned variable symbol -> unique effective-name string. The outermost
   COMPILE-TOPLEVEL / COMPILE-TOPLEVEL-EVAL binds a cons whose car is the table
   for its compile, made on first use
   (shared by compiles nested in it, like *COMPILER-NAME-SYMS*): a variable's
   name only has to be the same wherever its scope reaches, which is inside one
   top-level form. A global table kept every gensym any macro ever made, and the
   name string made for it, alive for as long as the image ran. The global
   value, synchronized, serves calls made outside a compile.")
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
      (let ((table (if (consp *uninterned-var-names*)
                       (or (car *uninterned-var-names*)
                           (setf (car *uninterned-var-names*) (make-hash-table :test #'eq)))
                       *uninterned-var-names*)))
        (or (gethash sym table)
            (setf (gethash sym table)
                  (format nil "~a#:~d" (symbol-name sym)
                          (uninterned-var-counter-next)))))))

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
  "Generate a fresh, uninterned label symbol named PREFIX_N (see GEN-LOCAL).
   PREFIX is required (every call site passes one) so the lambda list stays
   required-only and calls take the direct-delegate fast path (see GEN-LOCAL)."
  (let ((sym (make-symbol (format nil "~a_~d" prefix (incf *label-counter*)))))
    sym))

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

(defvar *%symbol-special-p-fn* nil
  "DOTCL-INTERNAL::%SYMBOL-SPECIAL-P once %RUNTIME-SPECIAL-P has found it.")

(defun %runtime-special-p (sym)
  "Check if the runtime marks SYM as special (via IsSpecial flag).
   Returns NIL during cross-compilation or if %SYMBOL-SPECIAL-P is unavailable.
   The function is looked up once: FIND-SYMBOL reads the package registry under
   its lock, and the tree-walk evaluator asks this for every variable it binds."
  (when (not *cross-compiling*)
    (let ((fn (or *%symbol-special-p-fn*
                  (multiple-value-bind (s status)
                      (find-symbol "%SYMBOL-SPECIAL-P" "DOTCL-INTERNAL")
                    (when (and s status (fboundp s))
                      (setq *%symbol-special-p-fn* (symbol-function s)))))))
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
                  ;; (unsigned-byte 64) slot: the bits, boxed as the integer
                  ;; they are (a Bignum at or above 2^63).
                  ((u64-native-local-p sym)
                   `((:ldloc ,key) (:call "Runtime.BoxU64")))
                  ((simd-native-local sym)
                   `((:ldloc ,key) ,(%simd-call "Box" (cdr (simd-native-local sym)))))
                  (t `((:ldloc ,key))))))
        ;; No local binding: check symbol-macro (let/let* shadow these too)
        (multiple-value-bind (sm-exp found) (lookup-symbol-macro sym)
          (if found
              (compile-expr sm-exp)
              ;; No lexical binding: check special (includes locally declared specials)
              `(,@(compile-sym-lookup sym)
                (:castclass "Symbol")
                (:call ,(if (%global-constant-symbol-p sym)
                            "DynamicBindings.GetConstant"
                            "DynamicBindings.Get"))))))))

(defun %global-constant-symbol-p (sym)
  "A constant variable cannot be bound, so its value is the global one."
  (and sym (symbolp sym) (not (eq sym t)) (not (keywordp sym))
       (not *cross-compiling*)
       (constantp sym)))

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
  (incf *type-env-epoch*)
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
                      (struct-slot-read-emitted-p expr code)
                      (code-ends-unwrapped-p code))
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
        (if (or (struct-slot-read-emitted-p expr code)
                (code-ends-unwrapped-p code))
            code
            `(,@code (:call "Runtime.UnwrapMv"))))))

;;; ============================================================
;;; Form dispatch
;;; ============================================================

(defvar *compile-depth* 0)

;; Nesting of forms COMPILE-FORM follows before it reports a runaway macro
;; expansion. It counts every nested form, each expansion included, so it is a
;; bound on the depth of the source as well as on expansion chains. The value is
;; well below what the 256MB Lisp thread stack holds for the deepest-framed
;; shapes measured (nested LET / WHEN / LAMBDA), and a runaway expansion still
;; stops within a couple of seconds. Generated code that nests a few thousand
;; forms deep (one statement per element of a literal sequence) is legitimate.
;; Running out of stack before this is reached is reported separately, by the
;; stack probe in COMPILE-FORM, so a smaller thread stack gets an error rather
;; than a process-killing overflow. *MACRO-EXPAND-DEPTH-LIMIT* in
;; cil-analysis.lisp starts from this value and must not be smaller.
(defvar *compile-depth-limit* 10000)


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
   expansion is marked *EXPANSION-SM-DEPENDENT*: the compiler reuses it only
   where the symbol macro names resolve the same way, and otherwise expands the
   form again with *LOCALS* in place."
  (when (and *sm-walker-context* (eq *sm-walker-bound* :none)
             (or *symbol-macros* (plusp (hash-table-count *global-symbol-macros*))))
    (setf *expansion-sm-dependent* t))
  (%symbol-macro-shadow-list *symbol-macros* (cstate-locals) *sm-walker-bound*))

(defun %symbol-macro-shadow-list (symbol-macros locals bound)
  "%SYMBOL-MACRO-SHADOWS for the symbol macro alist SYMBOL-MACROS, the locals
   alist LOCALS and the walk's bound names BOUND (or :NONE)."
  (let ((out '()))
    (when (or symbol-macros (plusp (hash-table-count *global-symbol-macros*)))
      (when (listp bound)
        (let ((*symbol-macros* symbol-macros))
          (dolist (v bound)
            (cond ((stringp v)
                   (when (%symbol-macro-string-name-p v) (push v out)))
                  ((and v (symbolp v) (symbol-package v) (%symbol-macro-name-p v))
                   (push v out))))))
      (when (plusp (hash-table-count *global-symbol-macros*))
        (dolist (e locals)
          (let ((v (car e)))
            (when (and v (symbolp v) (symbol-package v)
                       (nth-value 1 (gethash v *global-symbol-macros*))
                       (not (assoc v symbol-macros :test #'eq)))
              (push v out))))))
    (nreverse out)))

(defvar *global-symbol-macros-epoch* 0
  "Incremented whenever the compiler enters a global symbol macro, so that a
   position recorded by %SM-SNAPSHOT before it can tell it saw another table.")

(defun %sm-snapshot ()
  "What decides the symbol macro environment an expander gets here: the
   *SYMBOL-MACROS* alist, the locals alist, the walk's bound names and the
   global table's epoch. All of them are only replaced, never changed in place,
   so keeping the objects keeps the environment."
  (list* *global-symbol-macros-epoch* *symbol-macros* (cstate-locals) *sm-walker-bound*))

(defun %sm-signature (snapshot)
  "How every symbol macro name visible at SNAPSHOT resolves in the environment a
   macro expander gets there: a flat list NAME KIND VALUE ..., KIND :LEX for a
   SYMBOL-MACROLET binding (VALUE its expansion), :GLOBAL for a global symbol
   macro (VALUE its expansion), :SHADOW for a name a variable hides (VALUE NIL).
   Reads the global table as it is now."
  (destructuring-bind (epoch symbol-macros locals . bound) snapshot
    (declare (ignore epoch))
    (let ((shadows (%symbol-macro-shadow-list symbol-macros locals bound))
          (seen '())
          (out '()))
      (flet ((shadowed-p (name)
               (dolist (v shadows nil)
                 (when (if (stringp v)
                           (string= v (symbol-name name))
                           (eq v name))
                   (return t)))))
        (dolist (e symbol-macros)
          (let ((name (car e)))
            (unless (member name seen :test #'eq)
              (push name seen)
              (if (shadowed-p name)
                  (setf out (list* nil :shadow name out))
                  (setf out (list* (cdr e) :lex name out))))))
        (maphash (lambda (name v)
                   (unless (member name seen :test #'eq)
                     (if (shadowed-p name)
                         (setf out (list* nil :shadow name out))
                         (setf out (list* v :global name out)))))
                 *global-symbol-macros*))
      out)))

(defun %sm-snapshot-match-p (a b)
  "True when snapshots A and B (see %SM-SNAPSHOT) give an expander the same
   answer for every symbol it looks up: every symbol macro name resolves to the
   same (EQ) expansion, or is hidden by a variable, at both."
  (and (eql (car a) (car b))
       (or (and (eq (cadr a) (cadr b))
                (eq (caddr a) (caddr b))
                (eq (cdddr a) (cdddr b)))
           (let ((sa (%sm-signature a)) (sb (%sm-signature b)))
             (loop (cond ((and (null sa) (null sb)) (return t))
                         ((or (atom sa) (atom sb)) (return nil))
                         ((eq (car sa) (car sb)) (setf sa (cdr sa) sb (cdr sb)))
                         (t (return nil))))))))

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
    (when (or *symbol-macros* *lexical-operators* shadows (%lexinfo-wanted-p))
      (%lexical-macro-environment shadows))))

(defvar *lexenv-marker* (list '#:lexenv)
  "The key of the entry an environment's operator alist starts with when it
   carries the compiler's lexical state for the CLtL2 queries:
   (MARKER FRAMES . CSTATE). Not a symbol, so no operator lookup matches it.")

(defun %lexinfo-wanted-p ()
  "True while a form is being compiled (*MACROEXPAND-CACHE* is bound then), also
   at top level and in the analysis walks: a query there marks the expansion
   unreusable, so it is made again where the form is compiled."
  (and (not *cross-compiling*)
       (or *macroexpand-cache* *lexical-decl-frames* (cstate-locals))
       t))

(defun %lexical-macro-environment (&optional (shadows (%symbol-macro-shadows)))
  "The (lexical-operators . symbol-macros-ht) environment for the current
   position, even when both are empty. The CAR is *LEXICAL-OPERATORS*, which
   MACROEXPAND-1 searches before the global macro tables, headed by the
   *LEXENV-MARKER* entry when there is lexical state to report."
  (cons (if (%lexinfo-wanted-p)
            (cons (list* *lexenv-marker* *lexical-decl-frames* *cstate*)
                  *lexical-operators*)
            *lexical-operators*)
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

(defun call-int-return-range (expr)
  "For EXPR a call (NAME ...) to a global function whose return type is declared
   (or inferred) as an integer type that fits int64, that type's range (LO . HI);
   else NIL. FIXNUM answers the full int64 range. A narrower type such as
   (UNSIGNED-BYTE 32), the way 32-bit modular helpers are usually declared,
   answers its own range, so arithmetic around the call can be proven to stay in
   int64. A local function of the same name is a different function."
  (and (consp expr) (car expr) (symbolp (car expr))
       (if (local-function-entry (car expr))
           (return-from call-int-return-range (%local-inline-call-range expr))
           t)
       (let ((ty (function-return-type (car expr))))
         (if ty
             (let ((r (if (eq ty 'fixnum)
                          (cons +int64-min+ +int64-max+)
                          (declared-integer-range ty))))
               (and r (range-fits-int64-p r) r))
             (%inline-call-return-range expr)))))

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

;;; --- Compiler tables that have to survive a fasl ---
;;;
;;; A declaimed FTYPE, the body of a DEFUN proclaimed INLINE and a DEFSTRUCT's
;;; accessors are what later compilations use to emit typed calls, substitutions
;;; and slot accesses. All of them used to be
;;; recorded only in the image that compiled them, so code compiled in an
;;; image that had LOADed the fasl instead got the generic call. The compiled
;;; output now carries a PROCLAIM of each: the runtime's PROCLAIM hands any
;;; declaration it does not handle itself to %PROCLAIM-COMPILER-INFO. A runtime
;;; whose PROCLAIM does not do that ignores these declarations, so such a fasl
;;; still loads there; it only lacks the information, as before.

(defvar *proclaimed-variable-types* (make-hash-table :test #'eq :synchronized t)
  "Variable name -> the type a (DECLAIM (TYPE ...)) proclaimed for it.")

(defvar *proclaimed-policy* '()
  "Alist (QUALITY . VALUE) of the OPTIMIZE qualities proclaimed so far.")

(defun %type-declaration-spec-p (spec)
  "True when SPEC is (TYPE ty var...) or the (ty var...) shorthand."
  (and (consp spec)
       (let ((head (car spec)))
         (or (eq head 'type)
             (consp head)
             (and head (symbolp head)
                  (not (member head '(special ftype inline notinline optimize declaration
                                      ignore ignorable dynamic-extent values)))
                  *known-type-name-p-fn*
                  (funcall *known-type-name-p-fn* head)
                  (not (and *declaration-name-p-fn*
                            (funcall *declaration-name-p-fn* head))))))))

(defun %note-global-proclamation (spec)
  "Record the TYPE and OPTIMIZE proclamations the CLtL2 queries report."
  (cond ((not (consp spec)) nil)
        ((eq (car spec) 'optimize)
         (dolist (q (cdr spec))
           (multiple-value-bind (quality value)
               (if (consp q) (values (car q) (cadr q)) (values q 3))
             (when (and (symbolp quality) (integerp value))
               (setq *proclaimed-policy*
                     (acons quality value
                            (remove quality *proclaimed-policy* :key #'car)))))))
        ((%type-declaration-spec-p spec)
         (multiple-value-bind (type vars)
             (if (eq (car spec) 'type)
                 (values (cadr spec) (cddr spec))
                 (values (car spec) (cdr spec)))
           (dolist (v vars)
             (when (and v (symbolp v))
               (setf (gethash v *proclaimed-variable-types*) type)))))))

;;; The DOTCL-CLTL2 environment queries. The runtime's wrappers (Cltl2.cs) call
;;; these by name.

(defun %cltl2-global-variable-decls (name)
  (multiple-value-bind (type found) (gethash name *proclaimed-variable-types*)
    (and found (list (cons 'type type)))))

(defun %cltl2-global-function-decls (name)
  (let ((decls '()))
    (when (symbolp name)
      (cond ((%global-notinline-p name) (push '(inline . notinline) decls))
            ((%global-inline-p name) (push '(inline . inline) decls)))
      (multiple-value-bind (args found) (gethash name *function-arg-types*)
        (when (and found
                   (not (nth-value 1 (gethash name *inferred-return-type-names*))))
          (multiple-value-bind (ret rfound) (gethash name *function-return-types*)
            (when rfound
              (setq decls (append decls `((ftype function ,args ,ret)))))))))
    decls))

(defun %cltl2-global-function-kind (name)
  (cond ((and (symbolp name) (special-operator-p name)) :special-form)
        ((and (symbolp name) (macro-function name)) :macro)
        ((fboundp name) :function)
        ((and (symbolp name) (nth-value 1 (gethash name *function-arg-types*)))
         :function)))

;;; An environment from %LEXICAL-MACRO-ENVIRONMENT or AUGMENT-ENVIRONMENT
;;; carries the frames of *LEXICAL-DECL-FRAMES* and the *CSTATE* of its
;;; position. A query walks the frames innermost first, collecting what their
;;; declarations say about the name, up to the frame that binds it.

(defun %env-lexinfo (env)
  "(FRAMES . CSTATE) carried by ENV, or NIL."
  (and (consp env)
       (let ((ops (car env)))
         (and (consp ops) (consp (car ops))
              (eq (caar ops) *lexenv-marker*)
              (cdar ops)))))

(defun %lambda-list-variables (ll)
  (let ((out '()) (section '&required))
    (loop
      (cond ((null ll) (return out))
            ((atom ll) (return (cons ll out)))
            (t
             (let ((x (pop ll)))
               (cond ((member x lambda-list-keywords) (setq section x))
                     ((symbolp x) (push x out))
                     ((not (consp x)))
                     ((eq section '&required)
                      (setq out (append (%lambda-list-variables x) out)))
                     (t
                      (let ((v (car x)))
                        (cond ((symbolp v) (push v out))
                              ((and (eq section '&key) (consp v))
                               (if (symbolp (cadr v))
                                   (push (cadr v) out)
                                   (setq out (append (%lambda-list-variables (cadr v)) out))))
                              ((consp v)
                               (setq out (append (%lambda-list-variables v) out))))
                        (when (and (consp (cdr x)) (consp (cddr x)) (symbolp (caddr x)))
                          (push (caddr x) out)))))))))))

(defun %frame-variables (frame)
  (let ((names (cadr frame)))
    (case (car frame)
      ((:let :symbol-macrolet) (mapcar #'car names))
      (:lambda (%lambda-list-variables names))
      (:augment (append (first names) (mapcar #'car (second names)))))))

(defun %frame-functions (frame)
  (let ((names (cadr frame)))
    (case (car frame)
      ((:flet :macrolet) (mapcar #'car names))
      (:augment (append (third names) (mapcar #'car (fourth names)))))))

(defun %body-declaration-specs (body)
  "The declaration specifiers of BODY's leading DECLAREs, also when BODY is a
   single BLOCK holding them."
  (let ((specs '()))
    (loop
      (cond ((and (consp body) (stringp (car body)) (consp (cdr body)))
             (pop body))
            ((and (consp body) (consp (car body)) (eq (caar body) 'declare))
             (setq specs (append specs (cdar body)))
             (pop body))
            ((and (null specs) (consp body) (null (cdr body))
                  (consp (car body)) (eq (caar body) 'block) (consp (cdar body)))
             (setq body (cddar body)))
            (t (return specs))))))

(defun %variable-decl-entry (spec name)
  "What SPEC says about the variable NAME: :SPECIAL, an alist entry, or NIL."
  (when (consp spec)
    (case (car spec)
      (special (and (member name (cdr spec)) :special))
      (ignore (and (member name (cdr spec)) '(ignore . t)))
      (dynamic-extent (and (member name (cdr spec)) '(dynamic-extent . t)))
      (type (and (member name (cddr spec)) (cons 'type (cadr spec))))
      (t (and (%type-declaration-spec-p spec)
              (member name (cdr spec))
              (cons 'type (car spec)))))))

(defun %function-decl-entry (spec name)
  (when (consp spec)
    (case (car spec)
      (ftype (and (member name (cddr spec) :test #'equal) (cons 'ftype (cadr spec))))
      (inline (and (member name (cdr spec) :test #'equal) '(inline . inline)))
      (notinline (and (member name (cdr spec) :test #'equal) '(inline . notinline))))))

(defun %merge-decls (local global)
  (append local (remove-if (lambda (e) (assoc (car e) local)) global)))

(defun %cltl2-variable-information (name env global-kind)
  (let ((lexinfo (%env-lexinfo env)))
    (unless lexinfo
      (return-from %cltl2-variable-information
        (values global-kind nil (%cltl2-global-variable-decls name))))
    ;; The answer depends on where the macro is expanded; see *EXPANSION-UNREUSABLE*.
    (setf *expansion-unreusable* t)
    (let ((decls '()) (special nil) (binder nil))
      (dolist (frame (car lexinfo))
        (dolist (spec (%body-declaration-specs (cddr frame)))
          (let ((e (%variable-decl-entry spec name)))
            (cond ((eq e :special) (setq special t))
                  ((and e (not (assoc (car e) decls)))
                   (setq decls (append decls (list e)))))))
        (when (member name (%frame-variables frame))
          (setq binder frame)
          (return)))
      (cond ((and binder
                  (or (eq (car binder) :symbol-macrolet)
                      (and (eq (car binder) :augment)
                           (assoc name (second (cadr binder))))))
             (values :symbol-macro t nil))
            ((or special (eq global-kind :special))
             (values :special nil (%cltl2-global-variable-decls name)))
            ((or binder
                 (let ((*cstate* (cdr lexinfo))) (lookup-local name)))
             (values :lexical t decls))
            (t (values global-kind nil
                       (%merge-decls decls (%cltl2-global-variable-decls name))))))))

(defun %cltl2-function-information (name env)
  (let ((lexinfo (%env-lexinfo env)))
    (unless lexinfo
      (return-from %cltl2-function-information
        (values (%cltl2-global-function-kind name) nil
                (%cltl2-global-function-decls name))))
    (setf *expansion-unreusable* t)
    (let ((decls '()) (binder nil))
      (dolist (frame (car lexinfo))
        (dolist (spec (%body-declaration-specs (cddr frame)))
          (let ((e (%function-decl-entry spec name)))
            (when (and e (not (assoc (car e) decls)))
              (setq decls (append decls (list e))))))
        (when (member name (%frame-functions frame) :test #'equal)
          (setq binder frame)
          (return)))
      (cond ((and binder
                  (or (eq (car binder) :macrolet)
                      (and (eq (car binder) :augment)
                           (assoc name (fourth (cadr binder)) :test #'equal))))
             (values :macro t nil))
            ((or binder
                 (let ((*cstate* (cdr lexinfo))) (local-function-entry name)))
             (values :function t decls))
            (t (values (%cltl2-global-function-kind name) nil
                       (%merge-decls decls (%cltl2-global-function-decls name))))))))

(defun %cltl2-policy (specs)
  "The OPTIMIZE alist CLtL2 reports: each standard quality from the innermost
   of SPECS (OPTIMIZE declaration specifiers, innermost first) that sets it,
   else from the proclamations, else 1."
  (flet ((declared (quality)
           (dolist (spec specs nil)
             (dolist (q (cdr spec))
               (cond ((eq q quality) (return-from declared 3))
                     ((and (consp q) (eq (car q) quality) (integerp (cadr q)))
                      (return-from declared (cadr q))))))))
    (mapcar (lambda (quality)
              (list quality
                    (or (declared quality)
                        (cdr (assoc quality *proclaimed-policy*))
                        1)))
            '(speed space safety debug compilation-speed))))

(defun %cltl2-declaration-information (decl-name env)
  (case decl-name
    (optimize
     (let ((lexinfo (%env-lexinfo env)))
       (when lexinfo (setf *expansion-unreusable* t))
       (%cltl2-policy
        (loop for frame in (car lexinfo)
              append (remove-if-not (lambda (spec)
                                      (and (consp spec) (eq (car spec) 'optimize)))
                                    (%body-declaration-specs (cddr frame)))))))
    (declaration
     (let ((names '()))
       (when *declaration-name-p-fn*
         (do-all-symbols (s)
           (when (funcall *declaration-name-p-fn* s)
             (pushnew s names))))
       names))
    (t nil)))

;;; AUGMENT-ENVIRONMENT adds a frame for the new bindings, and puts the macros
;;; and symbol macros where MACROEXPAND-1 looks: the operator alist and the
;;; symbol-macro table.
(defun %cltl2-augment-environment (env variable symbol-macro function macro declare)
  (let* ((lexinfo (or (%env-lexinfo env) (cons '() +cstate-empty+)))
         (ops (if (consp env) (car env) '()))
         (ops (if (and (consp ops) (consp (car ops)) (eq (caar ops) *lexenv-marker*))
                  (cdr ops)
                  ops))
         (old-sm (and (consp env) (hash-table-p (cdr env)) (cdr env)))
         (sm (and (or old-sm symbol-macro variable) (make-hash-table :test #'equal)))
         (frame (list* :augment (list variable symbol-macro function macro)
                       (list (cons 'declare declare)))))
    (when old-sm
      (maphash (lambda (k v) (setf (gethash k sm) v)) old-sm))
    (dolist (v variable)
      (setf (gethash v sm) (cons *symbol-macro-shadow-marker* v)))
    (dolist (b symbol-macro)
      (setf (gethash (car b) sm) (cadr b)))
    (dolist (f (reverse function))
      (push (list f) ops))
    (dolist (m (reverse macro))
      (push (cons (car m) (cadr m)) ops))
    (cons (cons (list* *lexenv-marker* (cons frame (car lexinfo)) (cdr lexinfo)) ops)
          sm)))

(defun %cltl2-parse-macro (name lambda-list body)
  (let ((decls '()))
    (loop while (or (and (stringp (car body)) (cdr body))
                    (and (consp (car body)) (eq (caar body) 'declare)))
          do (let ((f (pop body)))
               (unless (stringp f) (push f decls))))
    (%macrolet-expander-form
     lambda-list
     `(,@(nreverse decls) (block ,(if (consp name) (cadr name) name) ,@body)))))

(defun %cltl2-enclose (lambda-expression env)
  "LAMBDA-EXPRESSION as a function closed in the macros and symbol macros of ENV."
  (let ((macros '()) (symbol-macros '()))
    (when (consp env)
      (dolist (e (reverse (car env)))
        (when (and (consp e) (symbolp (car e)) (functionp (cdr e)))
          (let ((w (gensym "WHOLE")) (en (gensym "ENV")) (r (gensym "REST")))
            (push `(,(car e) (&whole ,w &environment ,en &rest ,r)
                    (declare (ignore ,r))
                    (funcall ',(cdr e) ,w ,en))
                  macros))))
      (when (hash-table-p (cdr env))
        (maphash (lambda (k v)
                   (when (and (symbolp k) (not (%symbol-macro-shadow-entry-p v)))
                     (push (list k v) symbol-macros)))
                 (cdr env))))
    (eval `(macrolet ,macros
             (symbol-macrolet ,symbol-macros
               (function ,lambda-expression))))))

(defun %proclaim-compiler-info (spec)
  "Record the compiler side of the proclamation SPEC. Called by PROCLAIM for
   every declaration the runtime does not handle itself.
   (FTYPE (FUNCTION args ret) name...): RET as the declared return type of each
   NAME, as DECLAIM does at compile time.
   (%INLINE-EXPANSION name lambda-list body range): the recorded inline
   definition of NAME (see *INLINE-DEFS*) and its proved result range (NIL:
   none), as written by %INLINE-EXPANSION-PROCLAMATION.
   (%STRUCT-ACCESSORS name version accessor-names slots): what a DEFSTRUCT
   expansion records for its accessors, as written by
   %STRUCT-ACCESSORS-PROCLAMATION.
   Anything else is ignored."
  (when (consp spec)
    (%note-global-proclamation spec)
    (case (car spec)
      (ftype
       (let ((fty (cadr spec)))
         (when (and (consp fty) (eq (car fty) 'function) (cddr fty))
           (let ((ret (car (last fty))))
             (dolist (name (cddr spec))
               (when (and name (symbolp name))
                 (setf (gethash name *function-return-types*) ret)
                 (setf (gethash name *function-arg-types*) (cadr fty))
                 (remhash name *inferred-return-type-names*)))))))
      (%inline-expansion
       (destructuring-bind (name params body range) (cdr spec)
         (when (and (or (and name (symbolp name)) (%setf-function-name-p name))
                    (listp params) (listp body))
           (setf (gethash name *inline-defs*) (cons params body))
           (if (and (consp range) (integerp (car range)) (integerp (cdr range)))
               (setf (gethash name *inline-return-ranges*) range)
               (remhash name *inline-return-ranges*)))))
      (%struct-accessors
       (destructuring-bind (name version accessor-names slots) (cdr spec)
         (when (and name (symbolp name) (integerp version)
                    (listp accessor-names) (listp slots))
           (%load-struct-accessors name version accessor-names slots))))))
  nil)

(defun %fasl-dumpable-source-p (form &optional (limit 100000))
  "Can FORM, a piece of source code, be written into a fasl as a literal and
   read back meaning the same: built only of conses, symbols, numbers,
   characters and strings, and no larger than LIMIT conses (which also stops a
   circular form)."
  (let ((n 0))
    (labels ((ok (x)
               (cond ((consp x)
                      (and (<= (incf n) limit)
                           (ok (car x))
                           (ok (cdr x))))
                     ((symbolp x) t)
                     ((numberp x) t)
                     ((characterp x) t)
                     ((stringp x) (= (array-rank x) 1))
                     (t nil))))
      (ok form))))

(defun %inline-expansion-proclamation (name)
  "For a DEFUN of NAME compiled by COMPILE-FILE: the form that records NAME's
   inline definition when the fasl is loaded, or NIL when there is nothing to
   record (NAME is not recorded, its body is larger than any call site would
   substitute, or the body holds an object a fasl cannot carry as a literal)."
  (let ((def (and *compile-file-mode* (not *cross-compiling*)
                  (or (and (symbolp name) name) (%setf-function-name-p name))
                  (gethash name *inline-defs*))))
    (when (and def
               (<= (%form-size (%strip-macrolet-defs (cdr def))
                               (1+ *inline-body-size-limit*))
                   *inline-body-size-limit*)
               (%fasl-dumpable-source-p def))
      `(proclaim '(%inline-expansion ,name ,(car def) ,(cdr def)
                   ,(gethash name *inline-return-ranges*))))))

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

(defun %inline-body-with-macrolets (body params)
  "BODY, a DEFUN body that uses macros bound by enclosing MACROLETs, with its
   forms wrapped in those MACROLETs (outermost first), or NIL when it cannot be
   recorded that way.

   A top-level MACROLET around a group of DEFUNs is how a library names the
   pieces of a hot inner function (ironclad's (s-box-0 s-boxes i) for an AREF),
   and an INLINE function among them used to be recorded not at all, so every
   call to it stayed a call. SBCL substitutes such a function with its macro
   environment; this does the same by carrying the MACROLETs with the body. The
   leading declarations stay in front, where MAYBE-EXPAND-INLINE takes them for
   the LET of the parameters.

   Only MACROLETs can be carried: refused when the body could be captured by an
   enclosing SYMBOL-MACROLET or local function, when a local function is
   visible to the macro definitions, when a lexical variable is in scope (a
   macro's expansion may refer to it), and when a MACROLET name is in scope
   whose definition is not known here."
  (when (and (null *compile-time-flet-defs*)
             (null (cstate-locals))
             (null (cstate-local-functions))
             *macrolet-source-defs*
             (every (lambda (n)
                      (some (lambda (frame) (assoc n frame)) *macrolet-source-defs*))
                    *macrolet-shadowed*))
    (let ((hit nil))
      (labels ((walk (x)
                 (cond (hit nil)
                       ((symbolp x)
                        (when (and x (not (member x params)) (assoc x *symbol-macros*))
                          (setf hit t)))
                       ((consp x)
                        (unless (eq (car x) 'quote)
                          (walk (car x))
                          (walk (cdr x)))))))
        (walk body))
      (unless hit
        (let ((decls '()) (rest body))
          (when (and (stringp (car rest)) (cdr rest))
            (pop rest))
          (loop while (and (consp rest) (consp (car rest))
                           (eq (caar rest) 'declare))
                do (push (pop rest) decls))
          (let ((wrapped rest))
            (dolist (frame *macrolet-source-defs*)
              (setf wrapped `((macrolet ,frame ,@wrapped))))
            (append (nreverse decls) wrapped)))))))

(defun inline-guard-stack (name)
  "*INLINING-STACK* for the body of a BLOCK named NAME: with NAME pushed when it
   names a recorded inline function. Every inline expansion is
   (LET ((param arg) ...) (BLOCK name body...)), so pushing at the BLOCK keeps a
   call to NAME inside its own body from being substituted again (which would
   not terminate) while a call in an ARGUMENT, (mod32+ (mod32+ a b) c), is
   substituted like any other: it is evaluated outside the BLOCK. The guard used
   to be pushed around the whole expansion, which left every nested call of the
   same function as a call."
  (let ((stack *inlining-stack*))
    (when (and (symbolp name) name)
      ;; The expansion of a (SETF NAME) function is a BLOCK named NAME too.
      (let ((setf-name (list 'setf name)))
        (when (gethash setf-name *inline-defs*)
          (push setf-name stack)))
      (when (gethash name *inline-defs*)
        (push name stack)))
    stack))

(defun %strip-macrolet-defs (body)
  "BODY with the definition lists of the MACROLETs %INLINE-BODY-WITH-MACROLETS
   wrapped around it left out, for measuring it against
   *INLINE-BODY-SIZE-LIMIT*: the definitions are expanders, not code that is
   substituted, and counting them refused every body that carries any."
  (mapcar (lambda (f)
            (loop while (and (consp f) (eq (car f) 'macrolet) (consp (cdr f)))
                  do (setf f (if (cdddr f) (cons 'progn (cddr f)) (caddr f))))
            f)
          body))

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
             ;; BOUND: names a MACROLET inside BODY binds for the forms it
             ;; encloses (a body recorded by %INLINE-BODY-WITH-MACROLETS carries
             ;; its own); the caller's bindings of those names cannot reach them.
             (labels ((walk (x bound)
                        (cond (hit nil)
                              ((symbolp x)
                               (when (and x (not (member x params))
                                          (not (member x bound))
                                          (or (member x *macrolet-shadowed*)
                                              (assoc x *symbol-macros*)
                                              (and locals (local-function-entry x))))
                                 (setf hit t)))
                              ((consp x)
                               (cond ((eq (car x) 'quote))
                                     ((and (eq (car x) 'macrolet) (consp (cdr x))
                                           (listp (cadr x))
                                           (every #'consp (cadr x)))
                                      (let ((inner (append (mapcar #'car (cadr x)) bound)))
                                        (dolist (d (cadr x)) (walk (cddr d) inner))
                                        (walk (cddr x) inner)))
                                     (t (walk (car x) bound)
                                        (walk (cdr x) bound)))))))
               (walk body '()))
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
             (or (symbolp op) (%setf-function-name-p op))
             (not (local-function-entry op))
             (not (member op *notinline-functions* :test #'equal))
             (not (member op *inlining-stack* :test #'equal))
             (not (%global-notinline-p op)))
    (let ((def (gethash op *inline-defs*)))
      (when def
        (let ((params (car def))
              (body (cdr def))
              (args (cdr expr)))
          (when (and (every #'symbolp params)
                     (notany (lambda (p) (member p lambda-list-keywords)) params)
                     (= (length args) (length params))
                     (< (%form-size (%strip-macrolet-defs body)
                                    (1+ *inline-body-size-limit*))
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
                 ;; A (SETF F) function's body is a BLOCK named F.
                 (block ,(if (consp op) (cadr op) op) ,@rest)))))))))

(defun %inline-call-return-range (expr)
  "The range recorded in *INLINE-RETURN-RANGES* for the function EXPR calls,
   when this call is substituted (MAYBE-EXPAND-INLINE accepts it here); else
   NIL. The range was proved for the recorded body, under its own declarations,
   and a substituted call evaluates exactly that body. A call that stays a call
   reaches whatever the global definition is by then, so it gets nothing."
  (let ((r (gethash (car expr) *inline-return-ranges*)))
    (and r
         (maybe-expand-inline (car expr) expr)
         r)))

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
  "How many call sites one local function binding is substituted into when the
   FLET's body, substituted at every call, would be past the JIT's limits
   (%SIL-PAST-JIT-LIMITS-P).")

(defvar *local-inline-probe* nil
  "True while MAYBE-EXPAND-LOCAL-INLINE is asked whether a call would be
   substituted, without counting it.")

(defvar *local-inline-capped* nil
  "True while an FLET body is compiled again with each local function
   substituted at no more than *LOCAL-INLINE-CALL-LIMIT* call sites.")

(defun %local-inline-bindings (sym)
  "Everything SYM could name in the current scope: variable, local function,
   block, go tag, symbol macro. The tuple a local inline plan compares, so the
   list of things a substitution has to keep meaning has one definition."
  (list (local-entry sym)
        (local-function-entry sym)
        (assoc sym (cstate-block-tags))
        (assoc sym (cstate-go-tags))
        (assoc sym *symbol-macros*)
        (gethash sym *macros*)))

(defvar *local-inline-snapshot-limit* 20000
  "How many conses %LOCAL-INLINE-SNAPSHOT walks, macro expansions included,
   before it gives up on the definition (no plan, ordinary calls).")

(defun %local-inline-snapshot (body params)
  "For every symbol BODY mentions other than PARAMS, what it resolves to here,
   or :REFUSE when that cannot be worked out. PARAMS are excluded because the
   expansion's own LET binds them. QUOTE subforms are skipped: a quoted symbol
   names no binding.

   The text of BODY is not all it names. A macro form names whatever its
   expansion does: (GETX) expanding to a variable X reads the X bound where it
   is compiled. So the expansion of every macro form outside COMMON-LISP is
   walked as well (an expander that signals refuses the definition). The CL
   macros are not expanded: what they introduce is CL operators and fresh
   names, except the block RETURN leaves, so a RETURN, like a
   (RETURN-FROM NIL ...), records NIL's binding. NIL is otherwise skipped, as
   every list ends in it."
  (let ((seen '())
        (result '())
        (budget *local-inline-snapshot-limit*))
    (labels ((note (x)
               (unless (or (member x params :test #'eq)
                           (member x seen :test #'eq))
                 (push x seen)
                 (push (cons x (%local-inline-bindings x)) result)))
             (expansion (x)
               (let ((op (car x)))
                 (when (and (symbolp op)
                            op
                            (not (let ((pkg (symbol-package op)))
                                   (and pkg (string= (package-name pkg) "COMMON-LISP"))))
                            (or (not (local-function-entry op))
                                (%lexical-macro-p op)))
                   (let ((expander (find-macro-expander op)))
                     (when expander
                       (list (handler-case (funcall expander x)
                               (error ()
                                 (throw '%local-inline-refuse :refuse)))))))))
             (walk (x)
               (when (minusp (decf budget))
                 (throw '%local-inline-refuse :refuse))
               (cond ((and x (symbolp x)) (note x))
                     ((consp x)
                      (unless (eq (car x) 'quote)
                        (when (or (eq (car x) 'return)
                                  (and (eq (car x) 'return-from)
                                       (consp (cdr x))
                                       (null (cadr x))))
                          (note nil))
                        (let ((exp (expansion x)))
                          (when exp (walk (car exp))))
                        (walk (car x))
                        (walk (cdr x)))))))
      (catch '%local-inline-refuse
        (walk body)
        result))))

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

   Refused when the argument count does not match, when the FLET is compiled
   capped (*LOCAL-INLINE-CAPPED*) and OP has used its call sites, when OP is
   NOTINLINE here, when OP is already being substituted, and -- the one that
   does the work -- when any name the body mentions resolves to a different
   binding at this call site. A MACROLET between the definition and the call is
   one more way to rebind a name (as a macro), checked the same way. Every
   refusal leaves an ordinary call, so none of them can be wrong, only slower."
  (let ((plan (nth 5 (local-function-entry op))))
    (when (and plan
               (not (member op *inlining-stack*))
               (not (member op *notinline-functions*)))
      (destructuring-bind (name params decls body blockp snapshot count) plan
        (let* ((args (cdr expr))
               (ok (and (= (length args) (length params))
                        (or (not *local-inline-capped*)
                            (< (car count) *local-inline-call-limit*))
                        (%local-inline-env-matches-p snapshot))))
          (when (and ok (not *local-inline-probe*)) (incf (car count)))
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

;; A definer compiled anywhere in a form LOAD or COMPILE-FILE is processing
;; is located at that top level form.
(defun %maybe-note-definition (op expr)
  (let ((n (symbol-name op)))
    (when (and (> (length n) 5)
               (char= (char n 0) #\D) (char= (char n 1) #\E) (char= (char n 2) #\F))
      (let ((note (find-symbol "%NOTE-DEFINITION" "DOTCL")))
        (when (and note (fboundp note))
          (funcall note expr))))))

(defun compile-form (expr)
  "Compile a list form (op args...).
   Dispatch: quote fast-path -> flet-override check -> hash table (O(1), ~250 ops)
   -> cons-op cases -> string=-based ops -> macro expansion -> named-call."
  ;; Guard to catch runaway macro expansion loops (see *COMPILE-DEPTH-LIMIT*),
  ;; and, on a thread whose stack runs out first, to report that as a Lisp
  ;; error while there is still room to signal one.
  (when (> *compile-depth* *compile-depth-limit*)
    (error (format nil "Compile depth limit exceeded at depth ~D, form head: ~S"
                   *compile-depth* (if (consp expr) (car expr) expr))))
  (unless (%stack-space-available-p)
    (error (format nil "Compile stack exhausted at depth ~D, form head: ~S"
                   *compile-depth* (if (consp expr) (car expr) expr))))
  (incf *type-env-epoch*)
  (let ((*compile-depth* (1+ *compile-depth*))
        (*int-type-span* nil)
        (op (car expr))
        (*compile-was-toplevel* *at-toplevel*)
        (*at-toplevel* nil))
    (when *dotnet-type-references* (%note-dotnet-type-references expr))
    (when (and (symbolp op) (not *cross-compiling*))
      (%maybe-note-definition op expr))
    (when (and *compile-was-toplevel* (not *cross-compiling*))
      (maybe-note-unused-variables expr))
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

              ((%simd-node expr) (compile-simd-root expr))
              ((%rotate-byte-32-long-p expr)
               `(,@(compile-as-long expr) (:call "Fixnum.Make")))
              ((and (eql (%rotate-byte-width expr) 64) (try-compile-u64 expr)))
              ((eq op '%simd-store) (compile-simd-store-form expr))
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
                           ;; The recursion guard is pushed by the BLOCK of the
                           ;; expansion (INLINE-GUARD-STACK), so it covers the
                           ;; body and not the arguments. A call whose body is
                           ;; 64-bit word arithmetic is computed in that lane
                           ;; together with the inline calls it is made of. One
                           ;; known to return a fixnum keeps the int64 paths.
                           (let ((*at-toplevel* nil)
                                 (ow (octet-word-ref-info expr)))
                             (cond
                               ;; A word of adjacent octets: one wide read.
                               (ow (compile-octet-word-boxed ow))
                               ((and (not (fixnum-typed-p expr))
                                     (try-compile-u64 expr)))
                               (t (compile-expr inl))))
                           (compile-named-call op (cdr expr)))))))

              (t (error "Cannot compile form: ~s" expr)))))))))



;;; REMOVED: The ~250 (eq op 'foo) cond branches that previously followed here
;;; have been moved to *compile-form-handlers* hash table in cil-forms-dispatch.lisp.
