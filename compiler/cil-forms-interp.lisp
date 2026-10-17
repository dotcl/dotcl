;;; cil-forms-interp.lisp: Tree-walk evaluator (%MINI-EVAL)
;;; Part of the CIL compiler (A2 instruction list architecture)

(in-package :dotcl.cil-compiler)

;;;; Mini S-expression interpreter for compile-macrolet
;;;; Interprets the expander lambda without Reflection.Emit (NativeAOT/IL2CPP safe).

;; puthash is a compiler intrinsic (no CL standard equivalent). Define a Lisp
;; wrapper here (in dotcl.cil-compiler package) so %mini-eval can call it via
;; symbol-function. The body compiles to (:call "Runtime.Puthash") directly.
(defun puthash (key ht val) (puthash key ht val))

;; Same rationale as PUTHASH: these are the lowering targets SETF of SYMBOL-VALUE
;; and of GET expand into (cil-macros.lisp), emitted inline by the compiler
;; through the special-form table and therefore never given a function binding.
;; The tree-walk evaluator resolves an operator through SYMBOL-FUNCTION, so an
;; interpreted (setf (symbol-value s) v) died with "Undefined function:
;; %SET-SYMBOL-VALUE" and (setf (get s k) v) with "PUT-PROP". Each body compiles
;; to the corresponding (:call "Runtime.X"), so the compiled path is unchanged;
;; the name test in the emit table still wins there.
(defun %set-symbol-value (sym val) (%set-symbol-value sym val))
(defun put-prop (sym ind val) (put-prop sym ind val))

(defun %mini-lambda-list-shape (params)
  "Describe PARAMS for argument checking:
   (values nreq nopt restp keyp allow-other-keys-p keywords checkablep).
   CHECKABLEP is NIL when the list contains a lambda-list keyword this does not
   model; the caller then skips checking rather than risk rejecting a valid
   call."
  (let ((state :required) (nreq 0) (nopt 0) (restp nil) (keyp nil)
        (aok nil) (kws '()) (checkable t))
    (dolist (p params)
      (cond
        ((eq p '&optional) (setq state :optional))
        ;; &body is &rest with a different name (it reaches here only from a
        ;; lambda list that was not routed through DESTRUCTURING-BIND).
        ((or (eq p '&rest) (eq p '&body)) (setq state :rest restp t))
        ((eq p '&key) (setq state :key keyp t))
        ((eq p '&aux) (setq state :aux))
        ((eq p '&allow-other-keys) (setq aok t))
        ;; Anything else spelled like a lambda-list keyword (&whole, &environment,
        ;; an implementation's own) is not modelled here: stop claiming to know
        ;; the shape instead of counting it as a required parameter, which would
        ;; reject calls that are perfectly legal.
        ((and (symbolp p) p (char= (char (symbol-name p) 0) #\&))
         (setq checkable nil))
        ((eq state :required) (incf nreq))
        ((eq state :optional) (incf nopt))
        ((eq state :key)
         (let* ((head (if (consp p) (car p) p))
                (var (if (consp head) (cadr head) head))
                (kw (if (consp head) (car head)
                        (intern (symbol-name var) :keyword))))
           (push kw kws)))
        (t nil)))                       ; :rest / :aux consume no arguments
    (values nreq nopt restp keyp aok (nreverse kws) checkable)))

;;; The shape of a lambda list, #(nreq nopt restp keyp aok kws checkable), is
;;; remembered per PARAMS list. A lambda list does not change, but describing it
;;; returns seven values, and returning seven values conses a list; on every
;;; interpreted call. Keyed by the params list itself, which an interpreted
;;; function holds for its lifetime, and weakly, so a lambda list built by EVAL
;;; and dropped does not pin the entry.
;;;
;;; The memo is %MINI-MEMO / %SET-MINI-MEMO, the evaluator's runtime table of
;;; what it works out once per list (also a HANDLER-CASE's rewrite), which every
;;; thread reads without a lock: every interpreted call reads it, and interpreted
;;; code runs on whatever threads the program makes. A synchronized hash table
;;; here made those threads wait on each other on every call. (An unsynchronized
;;; one was worse: two threads interpreting at once corrupted it.) The
;;; cross-compile host has its own (cil-host.lisp).

(defun %mini-shape (params)
  "The shape vector for PARAMS (see %MINI-MEMO above)."
  (or (%mini-memo params)
      (%set-mini-memo
       params
       (multiple-value-bind (nreq nopt restp keyp aok kws checkable)
           (%mini-lambda-list-shape params)
         (vector nreq nopt restp keyp aok kws checkable)))))

(defun %mini-callee-name ()
  "Name of the interpreted function whose arguments are being checked, as a
   string, or \"anonymous\". The caller pushed the callee's frame before binding
   its parameters, so the innermost backtrace entry is that callee. Read only
   when an argument check has already failed, so the backtrace walk costs
   nothing on a call that is about to succeed."
  (let ((frames (ignore-errors (backtrace))))
    (if (and (consp frames) (stringp (first frames)))
        (first frames)
        "anonymous")))

(defun %mini-arg-error (control &rest arguments)
  "Signal the PROGRAM-ERROR an interpreted call's argument check raises, naming
   the callee and what was wrong. Compiled calls report through
   Runtime.CheckArityExact and read like

     FOO: wrong number of arguments: 3 (expected 1)

   while the interpreter used to signal a bare PROGRAM-ERROR that printed as
   #<PROGRAM-ERROR>. Same fault, no way to tell which call it came from; and on
   an emit-free build, where the interpreter is the only evaluator, that is every
   argument mistake in the program."
  (error 'program-error
         :format-control (concatenate 'string "~a: " control)
         :format-arguments (cons (%mini-callee-name) arguments)))

(defun %mini-check-args (params args)
  "Signal PROGRAM-ERROR if ARGS does not match the lambda list PARAMS.
   Interpreted closures previously accepted any argument count, so a missing
   required argument silently bound NIL and an unknown :KEY was ignored. That is
   the largest single source of ansi-test failures under the interpreter; every
   structure whose DEFSTRUCT was created through EVAL gets its accessors as
   interpreted closures, so (FOO-P) with no argument answered NIL instead of
   signalling (CLHS 3.5.1.2 / 3.5.1.4 / 3.5.1.6)."
  (let* ((shape (%mini-shape params))
         (nreq (svref shape 0))
         (nopt (svref shape 1))
         (restp (svref shape 2))
         (keyp (svref shape 3))
         (aok (svref shape 4))
         (kws (svref shape 5))
         (checkable (svref shape 6)))
    (when checkable
      (let ((n (length args)))
        ;; Wording follows the compiled path (Runtime.CheckArityExact / -Min /
        ;; -Max and the keyword check), so the same mistake reads the same way
        ;; whichever evaluator ran it: a required-only function reports an exact
        ;; count, anything else reports a bound.
        (let ((exactp (and (zerop nopt) (not restp) (not keyp))))
          (when (< n nreq)
            (if exactp
                (%mini-arg-error "wrong number of arguments: ~d (expected ~d)" n nreq)
                (%mini-arg-error "too few arguments: ~d (expected at least ~d)" n nreq)))
          (when (and (not restp) (not keyp) (> n (+ nreq nopt)))
            (if exactp
                (%mini-arg-error "wrong number of arguments: ~d (expected ~d)" n nreq)
                (%mini-arg-error "too many arguments: ~d (expected at most ~d)"
                                 n (+ nreq nopt)))))
        (when keyp
          (let ((tail (nthcdr (+ nreq nopt) args)))
            ;; The keyword portion must be an even-length list of keyword/value
            ;; pairs, and every keyword must be accepted unless other keys are
            ;; allowed: by &allow-other-keys or by :ALLOW-OTHER-KEYS non-NIL in
            ;; the call itself (CLHS 3.5.1.4).
            (when (oddp (length tail))
              (%mini-arg-error "odd number of keyword arguments"))
            ;; CLHS 3.4.1.4.1: when :ALLOW-OTHER-KEYS appears more than once, the
            ;; value of the FIRST pair is the one that counts. Scanning for "any
            ;; pair with a true value" made
            ;;   (:allow-other-keys nil :allow-other-keys t :foo t)
            ;; accept :FOO, where the leftmost NIL should have rejected it
            ;; (ansi-test COMPLEMENT.ERROR.6). Stop at the first pair, whatever
            ;; its value.
            (let ((caller-aok (do ((a tail (cddr a))) ((null a) nil)
                                (when (eq (car a) :allow-other-keys)
                                  (return (and (cadr a) t))))))
              (unless (or aok caller-aok)
                (do ((a tail (cddr a))) ((null a) nil)
                  (unless (or (member (car a) kws)
                              (eq (car a) :allow-other-keys))
                    (%mini-arg-error "unrecognized keyword argument ~s" (car a))))))))))))

(defun %mini-body-special-decls (body)
  "The names BODY's (DECLARE (SPECIAL ...)) forms mention. Scans every form, the
   same way %MINI-EVAL-PROGN does, so the two agree on what counts as declared."
  (let ((r '()))
    (dolist (f body r)
      (when (and (consp f) (eq (car f) 'declare))
        (dolist (d (cdr f))
          (when (and (consp d) (eq (car d) 'special))
            (dolist (v (cdr d)) (push v r))))))))

;;; SAFETY in the interpreter, at the granularity the compiler has it.
;;;
;;; The compiler decides SAFETY once per function body: the body's own leading
;;; (declare (optimize (safety N))), else the DECLAIM in force (see
;;; SAFEPOINTS-OFF-P / COMPILING-AT-SAFETY-0-P). Checks it elides under safety 0
;;; -- a structure slot's declared :TYPE, (THE FIXNUM ...) -- have to be elided
;;; here under the same condition, or the same function stores a value in one
;;; evaluator and signals in the other. An emit-free build interprets every
;;; DEFUN, so this is what that build does with (safety 0).
;;;
;;; The policy travels in ENV under %MINI-SAFETY, set where a user-visible
;;; function begins (%MINI-MAKE-CLOSURE): the declared value, or NIL for "no
;;; declaration here, the DECLAIM decides". A nested LAMBDA without its own
;;; declaration therefore does not inherit the outer function's, exactly as a
;;; compiled closure starts from a fresh CSTATE. Declarations on inner LET /
;;; LOCALLY forms are ignored, as the compiler ignores them.
(defun %mini-safety-0-p (env)
  "T when code evaluated in ENV runs at (safety 0): the enclosing function's own
   declaration if it made one, else the DECLAIM in force."
  (let ((b (assoc '%mini-safety env)))
    (if (and b (cdr b))
        (eql (cdr b) 0)
        (compiling-at-safety-0-p))))

(defun %mini-safety-plainly-above-0-p (env)
  "True when code evaluated in ENV is known not to run at (safety 0): what
   %MINI-SAFETY-0-P answers NIL for, decided only where that is cheap and cannot
   fail. NIL means %MINI-SAFETY-0-P has to be asked. A *CSTATE* that is not the
   compiler's vector (a test may bind it to anything) is left to it."
  (let ((b (assoc '%mini-safety env)))
    (cond ((and b (cdr b)) (not (eql (cdr b) 0)))
          ((and (boundp '*cstate*) *cstate*)
           (and (simple-vector-p *cstate*) (not (cstate-no-safepoint))))
          (t (not (eql *optimize-safety* 0))))))

(defun %mini-policy-env (body env)
  "ENV for the body of a function whose forms are BODY: carries BODY's declared
   SAFETY, or resets an inherited one so the DECLAIM decides. ENV itself when
   there is nothing to record, which is the common case and allocates nothing."
  (let ((declared (body-declared-safety body)))
    (cond (declared (cons (cons '%mini-safety declared) env))
          ((cdr (assoc '%mini-safety env)) (cons (cons '%mini-safety nil) env))
          (t env))))

(defun %mini-bind-supp (supp supp-val ps state env rest-args specials k)
  "Bind a supplied-p variable, then continue the lambda-list walk. Split out of
   %MINI-BIND-WALK (rather than being a LABELS inside it) so neither function
   closes over anything: both are on the per-call path of every interpreted
   call."
  (if (member supp specials)
      (progv (list supp) (list supp-val)
        (%mini-bind-walk ps state env rest-args specials k))
      (%mini-bind-walk ps state (acons supp supp-val env) rest-args specials k)))

(defun %mini-bind-walk (ps state env rest-args specials k)
  "Walk the remaining lambda list PS, extending ENV, and finally call K with it.
   See %MINI-BIND-PARAMS-CALL for what SPECIALS and K are for.

   The walk is ITERATIVE and recurses only where it must; at a special binding,
   whose PROGV has to wrap the remainder."
  (loop
    (when (null ps) (return-from %mini-bind-walk (funcall k env)))
    (let ((p (car ps)))
      (cond
        ((eq p '&optional) (setq state :optional ps (cdr ps)))
        ((or (eq p '&rest) (eq p '&body)) (setq state :rest ps (cdr ps)))
        ((eq p '&key) (setq state :key ps (cdr ps)))
        ((eq p '&aux) (setq state :aux ps (cdr ps)))
        ((eq p '&allow-other-keys) (setq ps (cdr ps)))
        (t
         (let ((var nil) (val nil) (supp nil) (supp-val nil) (next rest-args))
           (cond
             ((eq state :required)
              (setq var p val (car rest-args) next (cdr rest-args)))
             ((eq state :optional)
              (let* ((v (if (consp p) (car p) p))
                     (default (when (consp p) (cadr p)))
                     ;; An explicit NIL supplied-p is a binding of the constant
                     ;; NIL; an ABSENT one is just a short clause. (caddr p)
                     ;; cannot tell them apart, so test for the cell.
                     (sp (when (and (consp p) (cddr p))
                           (check-binding-name (caddr p) "lambda list")))
                     (present rest-args))
                (setq var v
                      val (if present (car rest-args)
                              (when default (%mini-eval default env)))
                      supp sp
                      supp-val (if present t nil)
                      next (if present (cdr rest-args) rest-args))))
             ((eq state :rest)
              (setq var p val rest-args))
             ((eq state :key)
              ;; p may be VAR, (VAR DEFAULT), (VAR DEFAULT SUPPLIED-P), or
              ;; ((:KEYWORD VAR) DEFAULT [SUPPLIED-P]).
              (let* ((head (if (consp p) (car p) p))
                     (v (if (consp head) (cadr head) head))
                     (kw (if (consp head) (car head)
                             (intern (symbol-name v) :keyword)))
                     (default (when (consp p) (cadr p)))
                     (sp (when (and (consp p) (cddr p))
                           (check-binding-name (caddr p) "lambda list")))
                     (cell (do ((a rest-args (cddr a))) ((null a) nil)
                             (when (eq (car a) kw) (return a)))))
                (setq var v
                      val (if cell (cadr cell)
                              (when default (%mini-eval default env)))
                      supp sp
                      supp-val (if cell t nil))))
             ((eq state :aux)
              (let* ((v (if (consp p) (car p) p))
                     (init (when (consp p) (cadr p))))
                (setq var v val (when init (%mini-eval init env))))))
           (check-binding-name var "lambda list")
           (setq ps (cdr ps))
           (cond
             ((member var specials)
              (return-from %mini-bind-walk
                (progv (list var) (list val)
                  (if supp
                      (%mini-bind-supp supp supp-val ps state env next specials k)
                      (%mini-bind-walk ps state env next specials k)))))
             (supp
              (setq env (acons var val env))
              (return-from %mini-bind-walk
                (%mini-bind-supp supp supp-val ps state env next specials k)))
             (t (setq env (acons var val env)
                      rest-args next)))))))))

(defun %mini-bind-params-call (params args env specials k)
  "Bind lambda list PARAMS to ARGS extending ENV, then call K with the result.
   Handles required, &optional (default + supplied-p), &rest, &key (default,
   supplied-p, ((:keyword var) ...)) and &aux. Init forms are evaluated
   left-to-right in the partial environment.

   A parameter named in SPECIALS is bound DYNAMICALLY (PROGV) instead of being
   put in the alist, and that binding is in effect for every LATER init form as
   well as for K. **That is why this takes a continuation instead of returning
   the env**: a PROGV scope cannot be handed back as a value.

   The previous shape built the whole env first and left the dynamic binding to
   %MINI-EVAL-PROGN, so an &AUX init ran BEFORE the parameter it reads was bound:

     (let ((x :bad)) (declare (special x))
       (flet ((%f () x))
         ((lambda (x &aux (y (%f))) (declare (special x)) y) :good)))   ; => :BAD

   (ansi-test LAMBDA.64.) Leaving the special parameter OUT of the alist is also
   what makes %MINI-EVAL-PROGN do the right thing afterwards: it only re-binds a
   declared name it can still find in ENV, so it now sees nothing to do and body
   references fall through to the dynamic value.

   This is the hottest path in the evaluator, so nothing here allocates for the
   common case (SPECIALS NIL): the walk is a plain loop in two top-level
   functions that close over nothing, and K is built once per closure rather than
   once per call (see %MINI-MAKE-CLOSURE). Written as a straightforward CPS walk
   with LABELS it cost ~10% on every interpreted call."
  (%mini-check-args params args)
  (%mini-bind-walk params :required env args specials k))

(defun %mini-bind-params (params args env)
  "Bind PARAMS to ARGS and RETURN the extended ENV. No parameter is treated as
   special; callers that need that use %MINI-BIND-PARAMS-CALL, which can keep a
   PROGV scope open across the rest of the binding and the body."
  (%mini-bind-params-call params args env '() #'identity))

(defvar *%mini-symbol-macro-marker* (make-symbol "SYMBOL-MACRO")
  "Head cons marker for a SYMBOL-MACROLET binding in %MINI-EVAL's ENV alist.

   It must be UNINTERNED. It used to be the interned symbol
   DOTCL-INTERNAL::SYMBOL-MACRO, which the SIL loader materialises lazily; the
   first interpreted SYMBOL-MACROLET brings it into the package, and from then on
   DO-SYMBOLS / DO-ALL-SYMBOLS hand the interpreter's own sentinel back to user
   code. Any interpreted variable then holding a list that starts with it (the
   DOLIST variable walking a package's symbol list is the obvious one) was read as
   a symbol-macro binding, and its SECOND element was evaluated as a variable:

     (do-symbols (sym (find-package \"DOTCL-INTERNAL\")) (symbol-name sym))
     => #<UNBOUND-VARIABLE: %THREADP>   ; the symbol after the marker

   That is ansi-test DO-ALL-SYMBOLS.1-13 / FIND-ALL-SYMBOLS.1 / PRINT.SYMBOL.RANDOM.3-4.
   The lazy interning is what made it look state-dependent: the same form passes
   until something interprets a SYMBOL-MACROLET, and the reported variable name
   changes with the package's symbol order. MAKE-SYMBOL puts the marker out of
   every package, so no iteration can ever produce it.")

(defun %mini-macroexpand-env (&optional lex-macros env)
  "&ENVIRONMENT object to hand MACROEXPAND-1 from %MINI-EVAL: the MACROLET and
   SYMBOL-MACROLET bindings currently in scope, and nothing else. (NIL . NIL)
   when there are none: never NIL, as in compiled code. LEX-MACROS is the enclosing %MINI-MACROS alist (name . expander).

   %MINI-EVAL used to call (MACROEXPAND-1 FORM) with no environment at all, so an
   expander's &ENVIRONMENT was always empty. SYMBOL-MACROLET bindings live only in
   *SYMBOL-MACROS*, which the runtime MACROEXPAND-1 does not read: so
   (MACROEXPAND x env) inside an expander could not see them (ansi-test
   RESTART-CASE.30, where RESTART-CASE must decide whether its body is a signaling
   form and the body is a symbol macro).

   MACROLET used to reach an expander by a different route: the MACROLET case
   registered its expanders GLOBALLY in *MACROS* for the dynamic extent of the
   body. That made them visible with an empty environment; and visible to every
   other form interpreted during that extent, including the body of a separately
   defined function the body happened to call. Putting the bindings here instead
   is what makes them lexical: MACROEXPAND-1 consults an environment's macro table
   before the runtime one, which is the shadowing CL asks for, and nothing outside
   this scope is handed the table.

   The CAR still must not be *MACROS* itself; that would reorder macro lookup for
   every interpreted form. Only the lexically established bindings go in."
  ;; LEX-MACROS goes in as it is: MACROEXPAND-1 reads that alist (innermost
  ;; first, (NAME) for an FLET name hiding a macro) directly.
  (cons lex-macros
        (when (or *symbol-macros* lex-macros
                  (plusp (hash-table-count *global-symbol-macros*)))
          (%mini-symbol-macro-table env))))

(defun %mini-symbol-macro-table (env)
  "The symbol-macro table of the &ENVIRONMENT %MINI-MACROEXPAND-ENV builds, keyed
   by symbol, or NIL when it would be empty. ENV is walked first, innermost
   binding first: a SYMBOL-MACROLET entry gives its expansion, and a variable over
   a symbol macro of the same name (global, or bound further out) gives a shadow
   entry, so MACROEXPAND-1 of that variable does not expand it. Then
   *SYMBOL-MACROS*, for the bindings ENV does not hold (the compiler's, while it
   runs an expander)."
  (let ((ht nil) (sm-names '()))
    (dolist (b env)
      (when (and (consp b) (consp (cdr b))
                 (eq (cadr b) *%mini-symbol-macro-marker*))
        (push (car b) sm-names)))
    (dolist (b env)
      (when (and (consp b) (car b) (symbolp (car b)))
        (let ((name (car b)))
          (cond ((and (consp (cdr b)) (eq (cadr b) *%mini-symbol-macro-marker*))
                 (unless ht (setq ht (make-hash-table :test #'equal)))
                 (%add-env-symbol-macro ht name (caddr b)))
                ((and (symbol-package name)
                      (or (member name sm-names :test #'eq)
                          (%symbol-macro-name-p name)))
                 (setq ht (%add-symbol-macro-shadows ht (list name))))))))
    ;; innermost first, exactly as above: *SYMBOL-MACROS* is pushed onto by
    ;; SYMBOL-MACROLET, so an outer binding sits behind an inner one of the same
    ;; name and must not overwrite it. Letting it win made the reified
    ;; &ENVIRONMENT expand a shadowed name to the OUTERMOST expansion (CLHS
    ;; 5.1.2.1 asks for the innermost).
    (dolist (entry *symbol-macros* ht)
      (unless ht (setq ht (make-hash-table :test #'equal)))
      (%add-env-symbol-macro ht (car entry) (cdr entry)))))

(defun %mini-flet-hide-macros (fn-defs new-env env)
  "NEW-ENV, plus an entry (NAME) in its %MINI-MACROS for each FLET / LABELS
   definition in FN-DEFS whose name is a macro (or has a compiler macro) in
   ENV: inside the local function's scope that name is a function
   (CLHS 3.1.2.1.2.2), which the
   operator dispatch and the &ENVIRONMENT handed to an expander both have to
   show. Returns NEW-ENV itself when no name is a macro, the common case, so
   %MINI-MACROS stays empty and costs nothing."
  (let* ((lex-macros (cdr (assoc '%mini-macros env)))
         (hidden '()))
    (dolist (def fn-defs)
      (let ((name (car def)))
        (when (and name (symbolp name)
                   (let ((e (assoc name lex-macros)))
                     (if e
                         (cdr e)
                         ;; A local function also hides a global compiler
                         ;; macro from COMPILER-MACRO-FUNCTION.
                         (or (macro-function name)
                             (compiler-macro-function name)))))
          (push (list name) hidden))))
    (if hidden
        (cons (cons '%mini-macros (nreconc hidden lex-macros)) new-env)
        new-env)))

(defun %mini-macro-fn (name env)
  "The lexical MACROLET binding of NAME in ENV, or NIL.

   Macros get their own key in ENV for the reasons FLET bindings (%MINI-FDEFN)
   and GO tags (%MINI-GO-TAGS) do: they are looked up before MACROEXPAND-1, which
   cannot see ENV and therefore always answered with the GLOBAL macro of the same
   name. ASSOC's EQL test is right here; a macro name is always a symbol (and
   NIL / T reach this too, which is the point: they are not SYMBOL instances, so
   nothing else in this evaluator recognises them in operator position)."
  (%mini-macro-fn-in name (cdr (assoc '%mini-macros env))))

(defun %mini-macro-fn-in (name lex-macros)
  "%MINI-MACRO-FN against an already-extracted %MINI-MACROS alist. The operator
   dispatch needs that alist twice per compound form, once to look the operator
   up, once to build the &ENVIRONMENT it hands MACROEXPAND-1, and this runs on
   every interpreted form, so it is extracted once and passed in."
  (and (or (symbolp name) (null name))
       (cdr (assoc name lex-macros))))

(defun symbol-macrolet-violation-p (bindings body)
  "True when (symbol-macrolet BINDINGS . BODY) is a program error.
   CLHS SYMBOL-MACROLET: the consequences are undefined (and it is required to
   signal PROGRAM-ERROR) if a name is a constant or names a globally special
   variable, or if the body declares one of the names SPECIAL.
   Shared by COMPILE-SYMBOL-MACROLET and the %MINI-EVAL case so the two
   evaluators cannot drift; the interpreter used to accept all three."
  (dolist (b bindings)
    (let ((name (car b)))
      (when (or (constantp name) (global-special-p name))
        (return-from symbol-macrolet-violation-p t))))
  (let ((binding-names (mapcar #'car bindings)))
    (dolist (form body)
      (when (and (consp form) (eq (car form) 'declare))
        (dolist (decl (cdr form))
          (when (and (consp decl) (eq (car decl) 'special))
            (dolist (sname (cdr decl))
              (when (member sname binding-names)
                (return-from symbol-macrolet-violation-p t))))))))
  nil)

;;; Tail calls in the tree-walk evaluator.
;;;
;;; A call in tail position to an interpreted function is not made; it is
;;; RETURNED, as (marker fn . args), to the trampoline that %MINI-MAKE-CLOSURE
;;; wraps around every interpreted function body. The trampoline runs the
;;; callee's body in its own loop, so a chain of interpreted tail calls costs
;;; constant stack instead of one .NET frame per call;
;;; without which a tail-recursive loop, the ordinary CL idiom, could not be
;;; written at all on a build with no compiler (netstandard2.0 / WASM / AOT).
;;;
;;; The marker is a fresh cons held in a variable, so nothing a user program can
;;; construct is EQ to it. Only positions that are genuinely tail pass TAILP on:
;;; the last form of a body, both arms of IF, the body of BLOCK, FLET, LABELS,
;;; MACROLET and SYMBOL-MACROLET. A body running
;;; inside PROGV (a special binding, including a special parameter), UNWIND-PROTECT
;;; or a handler cluster is NOT tail, the binding has to outlive the call, and
;;; those simply never pass the flag.
(defvar *%mini-tail-marker* (list '%mini-tail-call))

(defun %mini-tail-p (x)
  (and (consp x) (eq (car x) *%mini-tail-marker*)))

;; %MACROEXPAND-1-OR-SELF is a runtime builtin. COMPILE-MACROLET runs
;; %MINI-EVAL on the cross-compile host as well, which has its own
;; (cil-host.lisp).

;; %MINI-MACROEXPAND-1-CACHED and %MINI-STORE-EXPANSION are the runtime
;; builtins that remember the expansion of a form evaluated with no MACROLET or
;; SYMBOL-MACROLET in scope. The cross-compile host has no such cache
;; (cil-host.lisp).
(defvar *%mini-expansion-miss* (%mini-expansion-miss)
  "What %MINI-MACROEXPAND-1-CACHED answers when the expansion has to be made in
   the caller's environment.")

(defun %mini-eval-args (forms env)
  "Evaluate FORMS left to right in ENV and return the list of their values.
   A loop, not (MAPCAR (LAMBDA (A) (%MINI-EVAL A ENV)) FORMS): that lambda
   captures ENV, and creating a capturing closure costs a flat ~512 B whatever
   it captures. Every interpreted function call evaluates its arguments here."
  (let ((head nil) (tail nil))
    (dolist (f forms head)
      (let ((cell (cons (%mini-eval f env) nil)))
        (if tail (setf (cdr tail) cell) (setq head cell))
        (setq tail cell)))))

(defvar *%mini-go-marker* (list '%mini-go)
  "Head cons of the value (GO tag) returns instead of throwing, when the caller
   asked for it. See %MINI-EVAL's GOP parameter.")

(defun %mini-go-p (x)
  (and (consp x) (eq (car x) *%mini-go-marker*)))

;;; A RETURN-FROM travels as the same kind of marker as a GO, so everything that
;;; hands a GO marker on hands it on too: (marker block-tag . values), where a
;;; GO's is (marker tagbody-id . index). A BLOCK's tag is (%MINI-BLOCK name).

(defun %mini-throw-marker (m)
  "Take the exit marker M stands for by throwing, for a form whose caller does
   not check for markers: to the TAGBODY's catch with the index, or to the
   BLOCK's catch with the values."
  (if (eq (car (cadr m)) '%mini-block)
      (throw (cadr m) (values-list (cddr m)))
      (throw (cadr m) (cddr m))))

(defun %mini-block-catch-tag (tag)
  (let ((r (cddr tag)))
    (if (consp r) (car r) tag)))

(defun %mini-block-exit (tag gop &optional (v nil vp) &rest more)
  "The values of a BLOCK whose body ran with GOP on (see the BLOCK case of
   %MINI-EVAL). A marker for this block's RETURN-FROM gives its values; any other
   marker goes on to the BLOCK's caller if that checks (GOP), else is thrown;
   anything else is the body's own values."
  (cond ((not vp) (values))
        ((and (null more) (%mini-go-p v))
         (cond ((eq (cadr v) tag) (values-list (cddr v)))
               (gop v)
               (t (%mini-throw-marker v))))
        ((and (null more) (%mini-tail-p v))
         ;; The call leaves this block's CATCH behind. A RETURN-FROM it from
         ;; inside the call goes to the trampoline that makes the call instead.
         (setf (cddr tag) v)
         v)
        ((null more) v)
        (t (apply #'values v more))))

(defun %mini-check-simple-decls (forms bound-vars env)
  "Signal a TYPE-ERROR for the first variable among BOUND-VARS that FORMS'
   leading declarations declare with a SIMPLE-* array type and that holds a
   non-simple array. The variables are SIMPLE-ARRAY-DECLARED-VARS, which is what
   the compiler checks. A special variable's value is its dynamic one."
  (loop for (v . spec) in (simple-array-declared-vars
                           forms (mapcar #'var-name bound-vars))
        for b = (assoc v env)
        for val = (cond (b (cdr b))
                        ((boundp v) (symbol-value v))
                        (t nil))
        when (and (arrayp val) (not (typep val 'simple-array)))
          do (error 'type-error :datum val :expected-type spec)))

(defun %mini-arg-decl-checks (params body)
  (when (let ((f (if (stringp (car body)) (cadr body) (car body))))
          (and (consp f) (eq (car f) 'declare)))
    (multiple-value-bind (required optional key rest-param) (parse-lambda-list params)
      (arg-decl-checks body (mapcar #'var-name
                                    (append required (mapcar #'car optional) (mapcar #'second key)
                                            (if rest-param (list rest-param) nil)))))))

(defun %mini-check-arg-decls (checks env specials)
  (loop for (v . spec) in checks
        for val = (if (member v specials)
                      (symbol-value v)
                      (cdr (assoc v env)))
        unless (handler-case (typep val spec) (error () t))
          do (error 'type-error :datum val :expected-type spec)))

(defun %check-arg-decl (value spec)
  (unless (typep value spec)
    (error 'type-error :datum value :expected-type spec))
  nil)

(defparameter +arg-check-calls+
  '((list . "Runtime.CheckArgList") (cons . "Runtime.CheckArgCons")
    (symbol . "Runtime.CheckArgSymbol") (function . "Runtime.CheckArgFunction")
    (character . "Runtime.CheckArgCharacter") (integer . "Runtime.CheckArgInteger")))

(defun %arg-check-instrs (var spec)
  (let* ((ty (%typexpand-for-range spec))
         (long-slot (native-slot-p var (cstate-long-locals)))
         (dbl-slot (native-slot-p var (cstate-native-double-locals)))
         (range (and (or long-slot (not (and (consp ty) (eq (car ty) 'and))))
                     (declared-integer-range ty))))
    (cond
      ((eq ty 'fixnum)
       (unless long-slot `(,@(compile-expr var) (:call "Runtime.CheckArgFixnum"))))
      ((and range (range-fits-int64-p range))
       (if long-slot
           `(,@(compile-as-long var) (:ldc-i8 ,(car range)) (:ldc-i8 ,(cdr range))
             (:call "Runtime.CheckLongRange") (:pop))
           `(,@(compile-expr var) (:ldc-i8 ,(car range)) (:ldc-i8 ,(cdr range))
             (:call "Runtime.CheckArgRange"))))
      ((and range (eql (car range) 0) (eql (cdr range) (1- (expt 2 64))) (not long-slot))
       `(,@(compile-expr var) (:call "Runtime.CheckArgU64")))
      ((and (member ty '(double-float long-float)) dbl-slot) nil)
      ((and (member ty '(double-float long-float)) (not long-slot))
       `(,@(compile-expr var) (:call "Runtime.CheckArgDouble")))
      ((and (member ty '(single-float short-float)) (not long-slot) (not dbl-slot))
       `(,@(compile-expr var) (:call "Runtime.CheckArgSingle")))
      ((and (%float-range-bounds ty) (not long-slot) (not dbl-slot)
            (not (float-native-local-kind var)))
       (destructuring-bind (kind lo hi) (%float-range-bounds ty)
         (if (eq kind :single)
             `(,@(compile-expr var) (:ldc-r4 ,lo) (:ldc-r4 ,hi)
               ,@(compile-quoted-cached spec) (:call "Runtime.CheckArgSingleRange"))
             `(,@(compile-expr var) (:ldc-r8 ,lo) (:ldc-r8 ,hi)
               ,@(compile-quoted-cached spec) (:call "Runtime.CheckArgDoubleRange")))))
      ((and (consp ty) (eq (car ty) 'or) (= (length ty) 3) (member 'null (cdr ty)))
       (let ((ok (gen-label "ACOK"))
             (other (if (eq (second ty) 'null) (third ty) (second ty))))
         `(,@(compile-expr var) ,@(emit-nil) (:beq ,ok)
           ,@(%arg-check-instrs var other)
           (:label ,ok))))
      ((and (symbolp ty) (assoc ty +arg-check-calls+))
       `(,@(compile-expr var) (:call ,(cdr (assoc ty +arg-check-calls+)))))
      ((member ty '(string simple-string))
       `(,@(compile-expr var) ,@(compile-quoted-cached spec) (:call "Runtime.CheckArgString")))
      ((%simple-vector-et-hint ty)
       `(,@(compile-expr var) ,@(compile-quoted-cached spec)
         (:ldstr ,(%simple-vector-et-hint ty)) (:call "Runtime.CheckArgSimpleVector")))
      ((and (symbolp ty) (not (eq (symbol-package ty) (find-package "COMMON-LISP"))))
       `(,@(compile-expr var) ,@(compile-quoted-cached spec) (:call "Runtime.CheckArgStructSym")))
      (t `(,@(compile-expr var) ,@(compile-quoted-cached spec) (:call "Runtime.CheckArgType"))))))

(defun %float-range-bounds (ty)
  "(:SINGLE LO HI) / (:DOUBLE LO HI) for (SINGLE-FLOAT LO HI) / (DOUBLE-FLOAT LO
   HI) with inclusive bounds of that format or *, * as the format's most negative
   or positive finite value. Else NIL."
  (when (and (consp ty) (member (car ty) '(single-float double-float))
             (consp (cdr ty)) (consp (cddr ty)) (null (cdddr ty)))
    (let* ((single (eq (car ty) 'single-float))
           (fmt (if single 'single-float 'double-float))
           (lo (cadr ty)) (hi (caddr ty)))
      (when (and (or (eq lo '*) (typep lo fmt))
                 (or (eq hi '*) (typep hi fmt)))
        (list (if single :single :double)
              (if (eq lo '*)
                  (if single most-negative-single-float most-negative-double-float)
                  lo)
              (if (eq hi '*)
                  (if single most-positive-single-float most-positive-double-float)
                  hi))))))

(defun %simple-vector-et-hint (ty)
  (when (and (consp ty) (eq (car ty) 'simple-array) (= (length ty) 3) (equal (third ty) '(*)))
    (let ((u (ignore-errors (upgraded-array-element-type (second ty)))))
      (cond ((null u) nil)
            ((symbolp u) (symbol-name u))
            ((and (consp u) (member (car u) '(unsigned-byte signed-byte)) (integerp (second u)))
             (format nil "~a-~d" (symbol-name (car u)) (second u)))))))

(defun %check-simple-decl (value spec)
  "The function behind the %CHECK-SIMPLE-DECL intrinsic, for an evaluator that
   meets the form as a call. The compiler only ever emits it in its own
   expansions, and the interpreter checks declarations itself
   (%MINI-CHECK-SIMPLE-DECLS), so this exists for the intrinsic table's
   every-intrinsic-is-callable rule; it makes the same test."
  (when (and (arrayp value) (not (typep value 'simple-array)))
    (error 'type-error :datum value :expected-type spec))
  nil)

(defun %mini-eval-progn (forms env &optional bound-vars tailp gop)
  ;; (declare (special V)) in a body makes references to V within that body
  ;; DYNAMIC (CLHS 3.3.4). Which binding that refers to depends on whether the
  ;; enclosing form bound V, so BOUND-VARS names what it just bound:
  ;;
  ;;   V is in BOUND-VARS: the enclosing form's own binding becomes dynamic.
  ;;     PROGV it with the value that form computed.
  ;;       (let ((x 5)) (declare (special x)) ...)      ; x is dynamically 5
  ;;
  ;;   V is free here: the declaration only says "read V dynamically". Do NOT
  ;;     rebind: the reference must reach the value an OUTER special binding
  ;;     established.
  ;;       (let ((x :good)) (declare (special x))
  ;;         (let ((x :bad)) (locally (declare (special x)) x)))   ; => :GOOD
  ;;
  ;; Either way the lexical entry must be dropped for the body, or reads keep
  ;; hitting the alist and never consult the dynamic value at all. Taking the
  ;; value out of ENV unconditionally, what this did before, got the free case
  ;; exactly backwards: it re-bound V to the *shadowing lexical* value, so the
  ;; example above answered :BAD (ansi-test DO.14/17/19 and the LOCALLY tests).
  (let ((dyn-vars '()) (dyn-vals '()) (shadow '()) (simple-decl nil))
    (dolist (f forms)
      (when (and (consp f) (eq (car f) 'declare))
        (dolist (d (cdr f))
          (when (and (consp d)
                     (or (%simple-array-decl-spec-p (car d))
                         (and (eq (car d) 'type) (consp (cdr d))
                              (%simple-array-decl-spec-p (cadr d)))))
            (setq simple-decl t))
          (when (and (consp d) (eq (car d) 'special))
            (dolist (v (cdr d))
              (push v shadow)
              (when (member v bound-vars)
                (let ((b (assoc v env)))
                  (when b
                    (push v dyn-vars)
                    (push (cdr b) dyn-vals)))))))))
    ;; A variable this form binds, declared with a SIMPLE-* array type, must not
    ;; hold a non-simple array above (safety 0): the compiled check is
    ;; %CHECK-SIMPLE-DECL, and this is the same test on the same variables.
    (when (and simple-decl bound-vars (not (%mini-safety-0-p env)))
      (%mini-check-simple-decls forms bound-vars env))
    (when shadow
      (setq env (remove-if (lambda (e) (member (car e) shadow)) env)))
    ;; Evaluate all-but-last for effect; return the LAST form's values via a
    ;; tail %mini-eval so multiple values propagate (CL progn semantics).
    ;; ENV travels as an argument rather than being closed over. A local
    ;; function that captures anything costs a flat ~512 B to create, paid on
    ;; every entry here: and this runs for the body of every PROGN, LET,
    ;; LAMBDA and DEFUN the interpreter evaluates. Taking it as a parameter
    ;; leaves RUN capturing nothing, which allocates nothing.
    ;; GOP is passed straight through to every form: this body's value goes to a
    ;; caller that checks for the marker, and a non-last form's value is dropped
    ;; here, so a GO in either position can travel as a return value. If a
    ;; non-last form produces one, the rest of the body must not run.
    (labels ((run (fs e tail g)
               (cond ((null fs) nil)
                     ((null (cdr fs)) (%mini-eval (car fs) e tail g))
                     (t (let ((v (%mini-eval (car fs) e nil g)))
                          (if (and g (%mini-go-p v))
                              v
                              (run (cdr fs) e tail g)))))))
      (if dyn-vars
          ;; The PROGV must outlive the body, so its last form is not tail.
          (progv dyn-vars dyn-vals (run forms env nil gop))
          (run forms env tailp gop)))))

(defun %mini-lambda-list-var-names (params)
  "The variable names PARAMS binds, ignoring lambda-list keywords, default forms
   and supplied-p vars' spelling. Used to tell a (declare (special p)) that
   rebinds a parameter from one that merely reads an outer special."
  (let ((names '()))
    (dolist (p params)
      (cond ((and (symbolp p) p (char= (char (symbol-name p) 0) #\&)) nil)
            ((symbolp p) (push p names))
            ((consp p)
             (let ((head (car p)))
               (push (if (consp head) (cadr head) head) names))
             (when (caddr p) (push (caddr p) names)))))
    (nreverse names)))

(defun %mini-fdefn (name env)
  "The lexical FLET / LABELS binding of NAME in ENV, or NIL.

   Function names get their own key, for the same two reasons go tags did
   (%MINI-GO-TAGS): they are a separate namespace from variables (CLHS 3.1.1),
   and, unlike tags, a name can be the CONS (SETF F), which ASSOC's default
   EQL test can never match. Storing them in the variable alist therefore made
   every (setf f) local function invisible no matter how it was referenced, and
   made a VARIABLE whose value happened to be a function answer in operator
   position: (let ((list #'car)) (list 1 2)) called CAR.

   EQUAL is only needed for the (SETF F) names; a symbol name is settled by
   ASSOC's default EQL, which never matches one of those conses. Taking that
   branch keeps the keyword argument (and the args vector it allocates) out of
   a lookup every interpreted compound form makes."
  (let ((fns (cdr (assoc '%mini-functions env))))
    (if (symbolp name)
        (cdr (assoc name fns))
        (cdr (assoc name fns :test #'equal)))))

(defun %mini-fn-lambda (params bname body)
  "(LAMBDA PARAMS decls... (BLOCK BNAME body...)) for a named local function.
   The declarations must stay OUTSIDE the BLOCK: %MINI-MAKE-CLOSURE is the only
   caller that knows which names the lambda list bound, and %MINI-EVAL-PROGN only
   scans the forms handed to it. Burying them in the BLOCK made a parameter's
   (declare (special p)) look like a FREE declaration, so the parameter was never
   bound dynamically."
  (let ((decls '()) (rest body))
    (when (and (stringp (car rest)) (cdr rest)) (push (pop rest) decls))
    (loop while (and (consp (car rest)) (eq (caar rest) 'declare))
          do (push (pop rest) decls))
    `(lambda ,params ,@(nreverse decls) (block ,bname ,@rest))))

(defun %mini-check-lambda-list (params)
  "Signal PROGRAM-ERROR if PARAMS binds a constant, and return PARAMS.
   Checked when the closure is MADE, not when it is called: SBCL rejects
   (lambda (t) t) at that point, and a lambda that is never called would
   otherwise slip through the per-binding check in %MINI-BIND-WALK.
   %MINI-LAMBDA-LIST-VAR-NAMES cannot serve here -- it drops a bare NIL
   parameter, which is exactly one of the cases to catch."
  (dolist (p params)
    (unless (and (symbolp p) p (char= (char (symbol-name p) 0) #\&))
      (cond ((consp p)
             (let ((head (car p)))
               (check-binding-name (if (consp head) (cadr head) head)
                                   "lambda list"))
             ;; An explicit NIL supplied-p is a binding; an absent one is not.
             (when (cddr p) (check-binding-name (caddr p) "lambda list")))
            (t (check-binding-name p "lambda list")))))
  params)

(defun %mini-make-closure (lambda-form env &optional name plumbing)
  "Return a Lisp function that interprets LAMBDA-FORM in captured ENV.

   NAME, when given, is the string the function reports on the debugger call
   stack. Only a globally named definition passes one: a compiled backtrace lists
   DEFUN and DEFMETHOD frames and nothing else, FLET, LABELS and plain LAMBDA
   stay anonymous there, so the interpreted path names exactly the same set.

   PLUMBING says LAMBDA-FORM is the interpreter's own thunk around a body the
   user wrote inline (a HANDLER-BIND or RESTART-CASE body), not a function the
   user wrote: it keeps the enclosing function's SAFETY instead of starting a
   new policy scope (see %MINI-SAFETY-0-P)."
  (let* ((params (%mini-check-lambda-list (cadr lambda-form)))
         (body   (cddr lambda-form))
         (env    (if plumbing env (%mini-policy-env body env)))
         (names  (%mini-lambda-list-var-names params))
         ;; The parameters are what THIS form binds, so a (declare (special p))
         ;; in the body rebinds the parameter rather than reading an outer one.
         ;; Those are bound during the lambda-list walk, not after it, so a later
         ;; &OPTIONAL / &KEY default or &AUX init sees the dynamic binding.
         ;; A globally special name (DEFVAR, or a standard one like *PACKAGE*)
         ;; binds dynamically too, as LET does here: put in the alist, only
         ;; interpreted code saw the new value, and a function it called (INTERN
         ;; reading *PACKAGE*) still saw the outer one.
         (specials (union (intersection (%mini-body-special-decls body) names)
                          (remove-if-not #'%runtime-special-p names)))
         ;; Built once per closure, not once per call: it depends only on the
         ;; lambda form, and this runs on every interpreted call.
         ;; A special parameter binds through PROGV around the body, which must
         ;; outlive any call the body ends in: so such a body is not tail.
         (body-tailp (null specials))
         ;; The SYMBOL-MACROLET bindings in scope where this closure is made.
         ;; The SYMBOL-MACROLET case binds *SYMBOL-MACROS* dynamically (the
         ;; reified &ENVIRONMENT and LOOKUP-SYMBOL-MACRO read it), so without
         ;; this a function called from inside a SYMBOL-MACROLET body saw the
         ;; CALLER's local symbol macros: a global symbol macro referenced in
         ;; its body expanded to the caller's local expansion instead. Rebinding
         ;; to the value captured here makes the binding lexical at every call
         ;; boundary. The EQ test skips the binding in the common case where
         ;; nothing differs (typically both NIL).
         (captured-sms *symbol-macros*)
         ;; Everything %MINI-BIND-PARAMS-CALL needs to run this body again, so the
         ;; trampoline can re-enter it without calling the closure. Its identity
         ;; also stands in for this closure's: it is a fresh list per closure, so
         ;; EQ on it answers "is this callee me?": which the closure cannot ask
         ;; about itself, having no name inside its own LET* init form. The
         ;; continuation and the closure are filled in below, once they exist.
         ;; The last element says whether a tail call to another function is
         ;; made as one: not under (debug 3), as compiled code.
         (info (list params env specials nil captured-sms nil
                     (not (tail-calls-off-p body))))
         ;; INFO under its own key in ENV is how a call in tail position tells
         ;; "this is me again" from "this is some other function": the same
         ;; namespace trick %MINI-BLOCKS and %MINI-FUNCTIONS use.
         (arg-checks (let ((checks (and (not plumbing) (%mini-arg-decl-checks params body))))
                       (and checks (not (%mini-safety-0-p env)) checks)))
         (k (lambda (new-env)
              (when arg-checks (%mini-check-arg-decls arg-checks new-env specials))
              (%mini-eval-progn body
                                (if body-tailp
                                    (cons (cons '%mini-self-info info) new-env)
                                    new-env)
                                names body-tailp)))
         ;; MULTIPLE-VALUE-CALL, not (let ((r ...))): binding the body's result
         ;; would truncate it to one value, and an interpreted function that ends
         ;; in (values a b) must still return both. A tail request is always
         ;; exactly one value, so only that shape is intercepted; everything else
         ;; is passed straight back out, including no values at all.
         ;; Built once per closure, like K. It closes over INFO, and a closure
         ;; that captures anything costs a flat ~450 B to create: written inline
         ;; as MULTIPLE-VALUE-CALL's function it was built again on every call,
         ;; which was most of what entering an interpreted function allocated.
         (tail-filter
          (lambda (&optional (v1 nil v1p) &rest more)
            (cond ((and v1p (null more) (%mini-tail-p v1)) (%mini-trampoline v1 info))
                  (v1p (apply #'values v1 more))
                  (t (values)))))
         (fn (lambda (&rest call-args)
               (if (eq *symbol-macros* captured-sms)
                   (multiple-value-call tail-filter
                     (%mini-bind-params-call params call-args env specials k))
                   (let ((*symbol-macros* captured-sms))
                     (multiple-value-call tail-filter
                       (%mini-bind-params-call params call-args env specials k)))))))
    (setf (fourth info) k
          (sixth info) fn)
    (when name (%set-function-name fn name))
    ;; Every interpreted closure is variadic, so it would otherwise report zero
    ;; required parameters whatever the user wrote. Anything that reads a
    ;; function's arity then sees the evaluator's shape instead of the user's;
    ;; selecting the .NET overload for a delegate parameter, for one, which is why
    ;; a Lisp lambda handed to Enumerable.Select matched no overload at all.
    ;; Compiled code records the same count when it emits the function.
    ;; FBOUNDP because the cross-compiling host runs this while loading the
    ;; compiler, and %SET-FUNCTION-ARITY is a dotcl runtime primitive.
    (when (fboundp '%set-function-arity)
      (%set-function-arity fn (%mini-required-param-count params)))
    ;; What the trampoline needs to continue a tail call INTO this function
    ;; without calling it: everything %MINI-BIND-PARAMS-CALL takes.
    (when (fboundp '%set-fn-interp-info)
      (%set-fn-interp-info fn info))
    fn))

(defun %mini-call (fn args tailp env)
  "Call FN on ARGS or, in tail position, hand the call to the trampoline of the
   function being interpreted when the callee is interpreted too.

   A call back into the same function is always handed over. A call to another
   interpreted function (directly, or through FUNCALL or APPLY) is handed over
   unless the caller was declared (debug 3), which keeps every caller in the
   backtrace, as compiled code does. A call to a compiled function is made here."
  (let ((self (and tailp
                   (fboundp '%fn-interp-info)
                   (cdr (assoc '%mini-self-info env)))))
    (if self
        (multiple-value-bind (callee cargs) (%mini-tail-callee fn args)
          (let ((info (and callee (%fn-interp-info callee))))
            (if (and info (or (eq info self) (seventh self)))
                (list* *%mini-tail-marker* callee cargs)
                (apply fn args))))
        (apply fn args))))

(defvar *%mini-funcall* #'funcall)
(defvar *%mini-apply* #'apply)

(defun %mini-tail-callee (fn args)
  "The function and arguments a call of FN on ARGS comes down to, seen through
   FUNCALL and APPLY, or NIL when that is not plain."
  (cond ((eq fn *%mini-funcall*)
         (if args (values (%mini-function-designator (car args)) (cdr args)) nil))
        ((eq fn *%mini-apply*)
         (when (cdr args)
           (let ((spread (if (cddr args) (apply #'list* (cdr args)) (cadr args)))
                 (f (%mini-function-designator (car args))))
             (when (and f (%mini-proper-list-p spread))
               (values f spread)))))
        (t (values fn args))))

(defun %mini-function-designator (f)
  (cond ((functionp f) f)
        ((and f (symbolp f) (fboundp f)
              (not (macro-function f)) (not (special-operator-p f)))
         (symbol-function f))
        (t nil)))

(defun %mini-proper-list-p (x)
  (loop for n from 0
        do (cond ((null x) (return t))
                 ((or (atom x) (> n call-arguments-limit)) (return nil)))
           (setq x (cdr x))))

(defun %mini-trampoline (r self-info)
  "Run out the tail calls R stands for, each callee's body in this frame, so a
   chain of interpreted tail calls costs constant stack.

   The debugger frame of the function that started the chain is handed on as in
   compiled code (%MINI-TAIL-FRAME): it is renamed for each named callee, so the
   backtrace shows the function running in place of the one that called it. A
   named callee with no frame to take over is called the ordinary way, in tail
   position of this loop, and runs its own chain from there."
  (let ((owner (sixth self-info))
        (exit (list '%mini-trampoline)))
    ;; A BLOCK the chain has left points at the request it left by
    ;; (%MINI-BLOCK-EXIT). Its head becomes this catch's tag once taken here, so
    ;; a RETURN-FROM that block returns from the chain.
    (catch exit
      (setf (car r) exit)
      (loop
        (let* ((fn (cadr r))
               (args (cddr r))
               (info (%fn-interp-info fn)))
          (unless (eq fn owner)
            (let ((o (%mini-tail-frame owner fn args)))
              (if o
                  (setq owner o)
                  (return (apply fn args)))))
          (let ((next (if (eq *symbol-macros* (fifth info))
                          (%mini-tail-step info args)
                          (let ((*symbol-macros* (fifth info)))
                            (%mini-tail-step info args)))))
            (if (%mini-tail-p next)
                (setf (car next) exit r next)
                (return (values-list (cdr next))))))))))

(defun %mini-tail-step (info args)
  (multiple-value-call #'%mini-tail-step-values
    (%mini-bind-params-call (first info) args (second info)
                            (third info) (fourth info))))

(defun %mini-tail-step-values (&optional (v1 nil v1p) &rest more)
  (if (and v1p (null more) (%mini-tail-p v1))
      v1
      (list* *%mini-tail-values* (if v1p (cons v1 more) '()))))

;;; Marks "the chain ended with these values" inside the trampoline. Distinct
;;; from the tail marker so the loop can tell a further tail call from a result
;;; that merely happens to be a list.
(defvar *%mini-tail-values* (list '%mini-tail-values))

(defun %mini-required-param-count (params)
  "Number of parameters before the first lambda-list keyword; what a compiled
   function records as its arity."
  (let ((n 0))
    (dolist (p params n)
      (if (and (symbolp p) p (char= (char (symbol-name p) 0) #\&))
          (return n)
          (incf n)))))

(defun %mini-expand-handler-case (form)
  "Rewrite (handler-case body (type (var) h...) ... [(:no-error ll forms...)])
   into block + %MINI-HANDLER-CASE-BIND (+ multiple-value-call for :no-error).
   %MINI-HANDLER-CASE-BIND is HANDLER-BIND's shape, evaluated by
   %MINI-EVAL-HANDLER-BIND through %CALL-WITH-HANDLER-CASE instead of
   %CALL-WITH-HANDLER-CLUSTER. The condition clauses become handlers that
   non-locally exit the block with their result; a :no-error clause receives
   the body's values when no condition is signalled.

   Same shape as the HANDLER-CASE macro: a handler only stores the condition
   and GOes to its clause's tag, so the clause body runs after the unwind (an
   UNWIND-PROTECT cleanup in the body runs first), with the clause variable
   bound by a LET so the clause's own declarations (IGNORE, SPECIAL) apply."
  (let* ((body (cadr form))
         (all-clauses (cddr form))
         (no-error (find :no-error all-clauses :key (lambda (c) (and (consp c) (car c)))))
         (clauses (remove no-error all-clauses))
         (blk (gensym "HC-BLK"))
         (cvar (gensym "HC-COND"))
         (tag-forms '())
         (handler-bindings
          (mapcar (lambda (clause)
                    (let* ((type (car clause))
                           (vars (cadr clause))
                           (hbody (cddr clause))
                           ;; (error (nil) ...) names the constant NIL, which
                           ;; cannot be bound; an EMPTY list is the "no variable"
                           ;; spelling.
                           (var (when vars
                                  (check-binding-name (car vars) "HANDLER-CASE")))
                           (tag (gensym "HC-TAG"))
                           (g (gensym "HC-C")))
                      (push tag tag-forms)
                      (push `(return-from ,blk
                               ,(if var
                                    `(let ((,var ,cvar)) ,@hbody)
                                    `(locally ,@hbody)))
                            tag-forms)
                      (list type `(lambda (,g) (setq ,cvar ,g) (go ,tag)))))
                  clauses))
         (bind `(%mini-handler-case-bind ,handler-bindings ,body)))
    `(block ,blk
       (let ((,cvar nil))
         (tagbody
            (return-from ,blk
              ,(if no-error
                   `(multiple-value-call
                        (lambda ,(cadr no-error) ,@(cddr no-error))
                      ,bind)
                   bind))
            ,@(nreverse tag-forms))))))

(defun %mini-eval-handler-bind (form env)
  "Interpret (handler-bind ((type handler-form)...) body...), or the
   (%mini-handler-case-bind ...) form %MINI-EXPAND-HANDLER-CASE builds.
   Establishes the cluster through the runtime primitive, which runs the body
   under a .NET try/catch: a raw .NET exception is converted to a condition and
   signalled through the cluster, so an interpreted handler sees the same
   conditions a compiled one does. For HANDLER-CASE the primitive also takes an
   error that unwinds without being signalled to it, matched by type, which is
   what a compiled HANDLER-CASE's catch does."
  (let ((handlers (mapcar (lambda (b) (cons (car b) (%mini-eval (cadr b) env)))
                          (cadr form)))
        (thunk (%mini-make-closure (list* 'lambda '() (cddr form)) env nil t)))
    (if (eq (car form) 'handler-bind)
        (%call-with-handler-cluster handlers thunk)
        (%call-with-handler-case handlers thunk))))

(defun %mini-eval (form env &optional tailp gop)
  "Interpret FORM in ENV (alist of (sym . val)). No Reflection.Emit needed.

   TAILP says FORM's value is returned straight to the enclosing function body,
   so a call it ends in may be handed to the trampoline instead of made here
   (see *%MINI-TAIL-MARKER*). Callers that do anything with the value, bind it,
   test it, unwind past it, leave it NIL, which is the default.

   GOP says the caller inspects this form's value for a GO marker and will act
   on it, so a (GO tag) reached from here may return one instead of throwing.
   THROW out of the interpreter is expensive far beyond the throw itself: the
   .NET unwind walks every interpreter frame between the GO and its TAGBODY, and
   the interpreter stacks a dozen frames per level of Lisp nesting. The same
   throw costs about 1.0 us from compiled code and 4.8 us from here, which made
   one GO 3.1 us; 45% of an interpreted DOTIMES iteration.

   Only positions whose caller actually checks pass GOP on: the forms of a
   TAGBODY body, the forms of a PROGN/LET/LAMBDA body under one, and the arms of
   an IF. Everywhere else it stays NIL and GO throws exactly as before, so a GO
   from a place the marker could not travel through, an argument, an init form,
   a test, is unaffected."
  (cond
    ;; Self-evaluating
    ((null form) nil)
    ((or (numberp form) (stringp form) (characterp form)) form)
    ((keywordp form) form)
    ;; Variable lookup (also check symbol-macrolet bindings via lookup-symbol-macro)
    ((symbolp form)
     (let ((b (assoc form env)))
       (if b
           ;; Could be (SYMBOL-MACRO expansion) from symbol-macrolet
           (let ((v (cdr b)))
             (if (and (consp v) (eq (car v) *%mini-symbol-macro-marker*))
                 (%mini-eval (cadr v) env)
                 v))
           ;; With no symbol macro defined anywhere there is nothing for
           ;; LOOKUP-SYMBOL-MACRO to find. It would still read the global table
           ;; under its lock, and this runs for every free variable read.
           (multiple-value-bind (sm found)
               (if (or *symbol-macros*
                       (plusp (hash-table-count *global-symbol-macros*)))
                   (lookup-symbol-macro form)
                   (values nil nil))
             (if found
                 (%mini-eval sm env)
                 ;; Not lexically bound: a global symbol macro
                 ;; (define-symbol-macro) expands via macroexpand-1; otherwise
                 ;; treat as a dynamic/global variable reference.
                 (multiple-value-bind (exp expandedp) (macroexpand-1 form)
                   (if expandedp
                       (%mini-eval exp env)
                       (symbol-value form))))))))
    ((consp form)
     ;; A lexical FLET/LABELS binding shadows a GLOBAL MACRO of the same name
     ;; (CLHS 3.1.2.1.2.4). MACROEXPAND-1 cannot see ENV, so expanding first
     ;; turned (flet ((f () :good)) (f)) into the global macro's expansion.
     ;; Unless a MACROLET nested inside that FLET rebinds the name as a macro;
     ;; an FLET nested the other way round has left an entry (NAME) in front of
     ;; the MACROLET's (%MINI-FLET-HIDE-MACROS), so the innermost one wins.
     (let ((lex-fn (let ((f (%mini-fdefn (car form) env)))
                     (and f
                          (not (%mini-macro-fn (car form) env))
                          f))))
     (if lex-fn
         (%mini-call lex-fn
                     (%mini-eval-args (cdr form) env)
                     tailp env)
     ;; A MACROLET binding shadows a global macro for the same reason, and needs
     ;; its own lookup for the same reason: MACROEXPAND-1 consults the runtime
     ;; macro table before it ever reaches the *MACROS* entry the MACROLET case
     ;; writes, so (macrolet ((m () :good)) (m)) took the GLOBAL M's expansion
     ;; whenever one existed (ansi-test MACROLET.50, and the entries of
     ;; MACROLET.16 that name a CL macro).
     ;;
     ;; It also settles the operator NIL and T. Those are not SYMBOL instances
     ;; here, so MACROEXPAND-1 does not treat them as macro calls and the
     ;; fall-through called (SYMBOL-FUNCTION NIL), which type-errors;
     ;; (macrolet ((nil () ''a)) (nil)) died there (ansi-test MACROLET.15).
     ;; Looking the operator up in ENV first never reaches that path.
     (let* ((lex-macros (cdr (assoc '%mini-macros env)))
            (lex-macro (%mini-macro-fn-in (car form) lex-macros)))
     (if lex-macro
         ;; The expander is handed the same environment MACROEXPAND-1 would get,
         ;; so an &ENVIRONMENT parameter can expand the MACROLET bindings in scope
         ;; (they are lexical here, so an environment built from the globals would
         ;; not show them).
         (%mini-eval (funcall lex-macro form (%mini-macroexpand-env lex-macros env)) env tailp gop)
     (if (eq (car form) 'multiple-value-bind)
         ;; Handled before macroexpansion, like HANDLER-BIND just below. The
         ;; compiled expansion takes the values out of a per-thread snapshot
         ;; immediately after producing them -- sound in compiled code, where
         ;; nothing runs in between, but here the "in between" IS the
         ;; interpreter, whose own MULTIPLE-VALUE-BINDs overwrite that snapshot
         ;; (binding (floor 17 5) produced FIND-SYMBOL's two values). Bind from
         ;; a list instead, which is what this form did before the snapshot.
         (let* ((mvb-vars (mapcar (lambda (v)
                                    (check-binding-name v "MULTIPLE-VALUE-BIND"))
                                  (cadr form)))
                (mvb-vals (multiple-value-list (%mini-eval (caddr form) env))))
           (labels ((bind-seq (vs vls benv tail)
                      (if (null vs)
                          (%mini-eval-progn (cdddr form) benv mvb-vars tail gop)
                          (let ((var (car vs)) (val (car vls)))
                            (if (%runtime-special-p var)
                                (progv (list var) (list val)
                                  (bind-seq (cdr vs) (cdr vls) benv nil))
                                (bind-seq (cdr vs) (cdr vls)
                                          (cons (cons var val) benv) tail))))))
             (bind-seq mvb-vars mvb-vals env tailp)))
     (if (or (eq (car form) 'handler-bind)
             (eq (car form) 'handler-case)
             (eq (car form) '%mini-handler-case-bind))
         ;; HANDLER-BIND is implemented here rather than through its macro, the
         ;; way COMPILE-FORM consults its handler table before macroexpanding.
         ;; The macro exists so a code walker can see the body inline, and its
         ;; expansion (%PUSH-HANDLER-CLUSTER + UNWIND-PROTECT) establishes the
         ;; cluster without wrapping the body in a .NET try/catch: which is
         ;; correct for a walker and wrong for an evaluator, because a raw .NET
         ;; throw does not go through SIGNAL and so flew straight past the
         ;; handlers. %CALL-WITH-HANDLER-CLUSTER runs the body under a catch that
         ;; converts and signals.
         ;;
         ;; HANDLER-CASE is taken here too, before its macro turns it into a
         ;; HANDLER-BIND: a compiled HANDLER-CASE also catches an error that
         ;; unwinds without being signalled to it, matched by type, and a
         ;; HANDLER-BIND cannot. %MINI-EXPAND-HANDLER-CASE rewrites it into
         ;; %MINI-HANDLER-CASE-BIND, which runs under %CALL-WITH-HANDLER-CASE.
         (if (eq (car form) 'handler-case)
             ;; The rewrite is made once per form and kept, like a macro's
             ;; expansion (%MINI-MACROEXPAND-1-CACHED): rebuilt on every
             ;; evaluation, every form in it was new, so nothing about it could
             ;; be remembered either.
             (%mini-eval (or (%mini-memo form)
                             (%set-mini-memo form (%mini-expand-handler-case form)))
                         env)
             (%mini-eval-handler-bind form env))
     ;; First try macroexpand-1: handles destructuring-bind, when, cond, etc.
     ;; The environment carries the enclosing SYMBOL-MACROLET bindings (and only
     ;; those) so an expander's (MACROEXPAND x env) can see them. The *SYMBOL-MACROS*
     ;; test is inline rather than inside %MINI-MACROEXPAND-ENV: this runs for every
     ;; interpreted compound form, and the overwhelmingly common case is "no symbol
     ;; macros in scope", which must not cost a call.
     ;; %MACROEXPAND-1-OR-SELF, not MACROEXPAND-1: the two-value version decides
     ;; "expanded" by comparing the result against the input anyway, so the same
     ;; test here loses nothing: and returning two values allocates ~96 B on
     ;; every compound form the evaluator looks at, expanding or not.
     ;; With a global symbol macro defined, a SETF-family expander asks
     ;; LOOKUP-SYMBOL-MACRO whether a place names one, and only ENV knows
     ;; whether a LET here shadows it: hand ENV over for the expansion.
     ;; The MACROLET bindings also go to FIND-MACRO-EXPANDER for the extent of
     ;; the expansion: the SETF family's expanders take no environment, and look
     ;; a place's operator up there ((setf (m x) v) with M a local macro).
     (let ((expanded
            (if (or *symbol-macros* lex-macros)
                (let* ((*sm-interp-env* env)
                       (menv (%mini-macroexpand-env lex-macros env))
                       (*interp-expansion-macros* lex-macros)
                       (*interp-expansion-env* menv))
                  (%macroexpand-1-or-self form menv))
                ;; No MACROLET or SYMBOL-MACROLET in scope: the expansion is made
                ;; once per form and kept (%MINI-MACROEXPAND-1-CACHED). Only
                ;; when a global symbol macro exists and the kept one cannot
                ;; serve this scope is it made here, with ENV, as before.
                (let ((kept (%mini-macroexpand-1-cached form)))
                  (if (eq kept *%mini-expansion-miss*)
                      (%mini-store-expansion
                       form
                       (let ((*sm-interp-env* env))
                         (%macroexpand-1-or-self form (%mini-macroexpand-env nil env))))
                      kept)))))
       (if (not (eq expanded form))
           (%mini-eval expanded env tailp gop)
           ;; Dispatch on special form operators
           (let ((op (car form)))
             (case op
               (quote   (cadr form))
               ;; The test's value is used here, so it never carries a marker;
               ;; the arms' values are this form's value, so they can.
               (if      (if (%mini-eval (cadr form) env)
                            (%mini-eval (caddr form) env tailp gop)
                            (when (cdddr form) (%mini-eval (cadddr form) env tailp gop))))
               (progn   (%mini-eval-progn (cdr form) env nil tailp gop))
               (let
                ;; Special (dynamic) vars bind via progv; lexical vars via alist.
                ;; All init forms are evaluated in the outer env (parallel).
                (let ((spec-vars '()) (spec-vals '()) (lex '()))
                  (dolist (b (cadr form))
                    (let ((var (check-binding-name
                                (if (consp b) (car b) b) "LET"))
                          (val (when (consp b) (%mini-eval (cadr b) env))))
                      (if (%runtime-special-p var)
                          (progn (push var spec-vars) (push val spec-vals))
                          (push (cons var val) lex))))
                  (let ((new-env (append lex env))
                        (bound (mapcar (lambda (b) (if (consp b) (car b) b))
                                       (cadr form))))
                    (if spec-vars
                        ;; PROGV must outlive the body: not a tail position.
                        ;; A GO marker still travels: returning it unwinds the
                        ;; PROGV, which is what leaving the LET has to do.
                        (progv (nreverse spec-vars) (nreverse spec-vals)
                          (%mini-eval-progn (cddr form) new-env bound nil gop))
                        (%mini-eval-progn (cddr form) new-env bound tailp gop)))))
               (let*
                ;; Sequential (CLHS 3.1.2.1.1.2): each binding is in effect while
                ;; the REMAINING init forms are evaluated. Collecting the specials
                ;; and PROGV-ing them all at the end got that backwards: a later
                ;; init form ran with the special still at its outer value, and
                ;; then PROGV installed the saved value over whatever that form
                ;; had done to it. So
                ;;   (let* ((*ctr* 0) (s (make-s2))) ... *ctr* ...)
                ;; where the struct's slot default is (incf *ctr*) saw *ctr* = 0
                ;; in the body although the counter had been incremented: the
                ;; increment was applied to the outer binding and then masked.
                ;; (ansi-test structures: 370 of that category's failures.)
                ;; Bind one at a time instead, so each PROGV is established before
                ;; the next init form is evaluated and the body runs inside all of
                ;; them. Lexicals keep accumulating in ENV as before.
                (labels ((bind-seq (bindings benv tail)
                           (if (null bindings)
                               (%mini-eval-progn
                                (cddr form) benv
                                (mapcar (lambda (b) (if (consp b) (car b) b))
                                        (cadr form))
                                tail gop)
                               (let* ((b (car bindings))
                                      (var (check-binding-name
                                            (if (consp b) (car b) b) "LET*"))
                                      (val (when (consp b)
                                             (%mini-eval (cadr b) benv))))
                                 (if (%runtime-special-p var)
                                     ;; PROGV must outlive the body: from here in,
                                     ;; nothing under it is a tail position.
                                     (progv (list var) (list val)
                                       (bind-seq (cdr bindings) benv nil))
                                     (bind-seq (cdr bindings)
                                               (cons (cons var val) benv) tail))))))
                  (bind-seq (cadr form) env tailp)))
               (setq
                ;; CLHS 5.1.2.4 / setq: assigning a name that is a symbol macro
                ;; is SETF of its expansion, not a variable assignment. The env
                ;; alist holds a symbol-macrolet binding as
                ;; (name SYMBOL-MACRO expansion), the same shape the variable
                ;; read branch above already unwraps, so writing to (cdr b)
                ;; here did not just assign the wrong place, it OVERWROTE the
                ;; binding with the value and destroyed the symbol macro for the
                ;; rest of the body. (ansi-test SETF-SYMBOL-MACRO.1-3, and
                ;; PSETQ/PSETF/ROTATEF, which macroexpand into SETQ.)
                (let (result)
                  (let ((pairs (cdr form)))
                    (loop while pairs do
                      (let* ((var (car pairs))
                             (vform (cadr pairs))
                             (b   (assoc var env)))
                        (multiple-value-bind (exp smp)
                            (cond ((and b (consp (cdr b))
                                        (eq (cadr b) *%mini-symbol-macro-marker*))
                                   (values (caddr b) t))
                                  (b (values nil nil))
                                  ;; Not lexically bound: a global symbol macro
                                  ;; (DEFINE-SYMBOL-MACRO) is found the way the
                                  ;; variable read branch above finds it.
                                  (t (multiple-value-bind (sm found) (lookup-symbol-macro var)
                                       (if found
                                           (values sm t)
                                           (macroexpand-1 var)))))
                          (if smp
                              ;; Hand the whole assignment back to the evaluator as
                              ;; a SETF of the expansion, so the value form is
                              ;; evaluated exactly once and any place form works.
                              (setq result
                                    (%mini-eval (list 'setf exp vform) env))
                              (let ((val (%mini-eval vform env)))
                                (if b (setf (cdr b) val) (set var val))
                                (setq result val))))
                        (setq pairs (cddr pairs)))))
                  result))
               (function
                ;; A lexical FLET/LABELS binding shadows the global definition, so
                ;; the function namespace is consulted first (ansi-test BLOCK.5 /
                ;; BLOCK.10, where #'%f is handed to MAPCAR). This used to look in
                ;; the variable alist for an entry whose value happened to be a
                ;; function, because that was where FLET put its bindings; with a
                ;; real namespace the guess is unnecessary, and a (SETF F) name,
                ;; a CONS, which ASSOC/EQL never matched, now resolves too.
                ;; %COERCE-TO-FUNCTION for the same reason the operator branch
                ;; uses it: SYMBOL-FUNCTION has no cross-package bare-name bridge,
                ;; so #'class-precedence-list failed under the interpreter while
                ;; the compiled #' and (funcall 'name ...) both worked.
                (let ((fn (cadr form)))
                  (cond ((%mini-fdefn fn env))
                        ((symbolp fn) (%coerce-to-function fn))
                        ((and (consp fn) (eq (car fn) 'setf)) (fdefinition fn))
                        (t (%mini-make-closure fn env)))))
               (lambda
                (%mini-make-closure form env))
               (flet
                ;; Named local functions get an implicit block named after the
                ;; function ((setf f) -> block f), so (return-from f ...) works.
                ;; Bindings go in the FUNCTION namespace (see %MINI-FDEFN).
                (let ((fns '()) (outer (cdr (assoc '%mini-functions env))))
                  (dolist (def (cadr form))
                    (let ((bname (if (consp (car def)) (cadr (car def)) (car def))))
                      (push (cons (car def)
                                  (%mini-make-closure
                                   (%mini-fn-lambda (cadr def) bname (cddr def)) env))
                            fns)))
                  (%mini-eval-progn
                   (cddr form)
                   (%mini-flet-hide-macros
                    (cadr form)
                    (cons (cons '%mini-functions (append (nreverse fns) outer)) env)
                    env)
                   nil tailp)))
               (labels
                ;; Pre-allocate cells so closures can mutually reference each other,
                ;; then fill them in against an env that already has every name.
                (let* ((cells (mapcar (lambda (def) (cons (car def) nil)) (cadr form)))
                       (outer (cdr (assoc '%mini-functions env)))
                       (new-env (%mini-flet-hide-macros
                                 (cadr form)
                                 (cons (cons '%mini-functions (append cells outer)) env)
                                 env)))
                  (dolist (def (cadr form))
                    (let ((cell (assoc (car def) cells :test #'equal))
                          (bname (if (consp (car def)) (cadr (car def)) (car def))))
                      (setf (cdr cell)
                            (%mini-make-closure
                             (%mini-fn-lambda (cadr def) bname (cddr def)) new-env))))
                  (%mini-eval-progn (cddr form) new-env nil tailp)))
               (block
                ;; BLOCK names are LEXICAL (CLHS 3.1.2.1.2.4), so each entry gets a
                ;; fresh tag object recorded in its own namespace in ENV: the same
                ;; shape %MINI-GO-TAGS uses, and for the same reason.
                ;;
                ;; The block NAME itself used to be the CATCH tag, which made the
                ;; scope DYNAMIC: a same-named inner block hid the outer one for
                ;; everything running inside it, including a closure whose text is
                ;; lexically outside. In
                ;;   (block done
                ;;     (flet ((%f (x) (return-from done x)))
                ;;       (block done (mapcar #'%f '(good bad bad))))
                ;;     'bad)
                ;; %f's RETURN-FROM refers to the OUTER DONE, but the throw landed
                ;; on the inner one, so MAPCAR simply took its next element and the
                ;; form returned BAD (ansi-test BLOCK.10). A closure carries the ENV
                ;; it was made in, so looking the tag up there is exactly the
                ;; lexical rule.
                ;;
                ;; The body runs with GOP on, so a RETURN-FROM this BLOCK in a
                ;; position whose value reaches the BLOCK comes back as a marker
                ;; (see RETURN-FROM) instead of being thrown. The CATCH stays for
                ;; the ones that cannot: from an argument, an init form, a closure.
                (let* ((name (cadr form))
                       (tag (list '%mini-block name))
                       (new-env (cons (cons '%mini-blocks
                                            (cons (cons name tag)
                                                  (cdr (assoc '%mini-blocks env))))
                                      env)))
                  (multiple-value-call #'%mini-block-exit tag gop
                    (catch tag (%mini-eval-progn (cddr form) new-env nil tailp t)))))
               (return-from
                ;; No lexically visible block of that name: fall back to throwing on
                ;; the name, which is what every established block used as its tag
                ;; before. Keeps a RETURN-FROM that crosses an evaluator boundary
                ;; behaving as it did rather than turning into a new failure mode.
                ;;
                ;; When the caller checks for a marker (GOP), a RETURN-FROM a
                ;; lexically visible block returns one, carrying the block's tag
                ;; and the values, and every form on the way up hands it on to the
                ;; BLOCK (%MINI-BLOCK-EXIT), as a GO marker travels to its
                ;; TAGBODY. A throw unwinds through the .NET exception machinery,
                ;; which costs microseconds, and an interpreted DOTIMES, LOOP or
                ;; HANDLER-CASE leaves its block by RETURN-FROM every time.
                (let ((b (assoc (cadr form) (cdr (assoc '%mini-blocks env)))))
                  (if (and b gop)
                      (list* *%mini-go-marker* (cdr b)
                             (multiple-value-list
                              (when (cddr form) (%mini-eval (caddr form) env))))
                      (throw (if b (%mini-block-catch-tag (cdr b)) (cadr form))
                             (when (cddr form) (%mini-eval (caddr form) env))))))
               (the
                ;; (THE FIXNUM ...) is a range license on the compiled path: the
                ;; arithmetic under it is lowered to native int64 and, above
                ;; SAFETY 0, computed with an overflow check that signals a
                ;; TYPE-ERROR. Check it here too, or the same source means one
                ;; thing compiled and another interpreted -- the emit-free build
                ;; runs everything through this evaluator.
                ;;
                ;; Only the bare FIXNUM type is checked -- the one the license is
                ;; about. A wider declaration is deliberately left alone: a
                ;; (THE (SIGNED-BYTE 100) X) may legitimately hold a bignum, and
                ;; native-int-arith.lisp asserts exactly that such a value comes
                ;; back uncorrupted rather than being rejected.
                ;;
                ;; SAFETY is the enclosing function's, as the compiler sees it
                ;; (%MINI-SAFETY-0-P).
                ;; DOUBLE-FLOAT is checked for the same reason and on the same
                ;; terms. It is what licenses the native square root, whose one
                ;; input with a different answer -- a negative, whose root is a
                ;; COMPLEX -- signals there above SAFETY 0.
                ;;
                ;; THE returns all the values of its form (CLHS): the check
                ;; reads the primary value and the rest pass through.
                (if (member (cadr form) '(fixnum double-float))
                    (multiple-value-call
                        (lambda (&optional v &rest more)
                          (when (and (not (typep v (cadr form)))
                                     (not (%mini-safety-0-p env)))
                            (error 'type-error :datum v :expected-type (cadr form)))
                          (if more (apply #'values v more) v))
                      (%mini-eval (caddr form) env))
                    (%mini-eval (caddr form) env)))
               (%check-slot-type
                ;; A structure slot's declared :TYPE, checked at the store (see
                ;; %SLOT-CHECK-STMT). The compiler drops the check under
                ;; (safety 0); drop it under the same condition here. The
                ;; arguments are a variable and three constants, so not
                ;; evaluating them has no effect to lose.
                (if (%mini-safety-0-p env)
                    nil
                    (%check-slot-type (%mini-eval (second form) env)
                                      (%mini-eval (third form) env)
                                      (%mini-eval (fourth form) env)
                                      (%mini-eval (fifth form) env))))
               (locally (%mini-eval-progn (cdr form) env nil tailp))
               (eval-when
                ;; In an evaluator (not compile-file), eval the body iff :execute
                ;; is among the situations (CLHS 3.2.3.1).
                (let ((situations (cadr form)))
                  (when (or (member :execute situations)
                            (member 'cl:eval situations)) ; legacy EVAL keyword
                    (%mini-eval-progn (cddr form) env))))
               (symbol-macrolet
                ;; Extend env with symbol macro bindings, AND bind *SYMBOL-MACROS*.
                ;; That dynamically-scoped alist is what %MACROLET-EXPANDER-FORM
                ;; turns into the &ENVIRONMENT object it hands a macrolet expander:
                ;;   (cons *macros* <hash of *symbol-macros*>)
                ;; so an expander's (macroexpand x env) can only see an enclosing
                ;; SYMBOL-MACROLET if that special was bound. The interpreter pushed
                ;; onto its own alist only, leaving *SYMBOL-MACROS* empty, so the
                ;; expander was handed an environment with no symbol macros in it
                ;; and MACROEXPAND returned the symbol unchanged (ansi-test
                ;; MACROLET.13/14/15). COMPILE-SYMBOL-MACROLET binds the same
                ;; special, which is exactly why the compiled path was right: the
                ;; expander builder is deliberately shared between the two paths,
                ;; so the divergence was in who populates what it reads.
                ;;
                ;; The same split applied to the CLHS program-error cases (a
                ;; constant / globally special name, or a body that declares one
                ;; of the names special): COMPILE-SYMBOL-MACROLET rejected them
                ;; and the interpreter silently accepted, so a symbol-macro could
                ;; shadow a special variable under EVAL only (ansi-test
                ;; SYMBOL-MACROLET.ERROR.1/2/3). The predicate is shared.
                (when (symbol-macrolet-violation-p (cadr form) (cddr form))
                  (error 'program-error))
                (let ((new-env env) (new-sm *symbol-macros*))
                  (dolist (binding (cadr form))
                    (push (cons (car binding)
                                (list *%mini-symbol-macro-marker* (cadr binding)))
                          new-env)
                    (push (cons (car binding) (cadr binding)) new-sm))
                  (let ((*symbol-macros* new-sm))
                    (%mini-eval-progn (cddr form) new-env nil tailp))))
               (macrolet
                ;; The bindings go into a lexical %MINI-MACROS namespace in ENV. The
                ;; operator dispatch consults it (%MINI-MACRO-FN), and it is what the
                ;; &ENVIRONMENT handed to MACROEXPAND-1 is built from
                ;; (%MINI-MACROEXPAND-ENV): an environment's macro table is consulted
                ;; before the runtime one, so a MACROLET over a name that already had
                ;; a global macro shadows it, which is what CL asks for.
                ;;
                ;; The expanders used to ALSO be registered in the global *MACROS*
                ;; table for the dynamic extent of the body. That is how they used to
                ;; become visible to MACROEXPAND-1, which cannot see ENV: and it made
                ;; them visible far too widely: any form interpreted during that
                ;; extent saw them, including the body of a separately defined
                ;; function that the body called, so
                ;;   (defun h () :global) (defun c () (h))
                ;;   (macrolet ((h () :macro)) (list (h) (c)))
                ;; answered (:MACRO :MACRO) interpreted and (:MACRO :GLOBAL) compiled.
                (let ((lex-macros '()))
                  (dolist (def (cadr form))
                    (push (cons (car def)
                                (%mini-eval (%macrolet-expander-form (cadr def) (cddr def))
                                            env))
                          lex-macros))
                  (%mini-eval-progn
                   (cddr form)
                   (cons (cons '%mini-macros
                               (append (nreverse lex-macros)
                                       (cdr (assoc '%mini-macros env))))
                         env)
                   nil tailp)))
               (tagbody
                (let* ((tb-id (list 'tagbody))
                       ;; Parse body into segments: list of (tag . forms)
                       (segs
                        (let ((cur-tag nil) (cur-forms '()) (result '()))
                          (dolist (item (cdr form))
                            (if (or (symbolp item) (integerp item))
                                (progn
                                  (push (cons cur-tag (nreverse cur-forms)) result)
                                  (setq cur-tag item cur-forms '()))
                                (push item cur-forms)))
                          (push (cons cur-tag (nreverse cur-forms)) result)
                          (nreverse result)))
                       ;; Go tags live in their OWN namespace (CLHS 3.1.1), so they
                       ;; are collected under one private key instead of being
                       ;; pushed onto ENV under the tag symbol itself. Sharing the
                       ;; symbol key with variables was actively destructive: in
                       ;;   (let ((even nil)) (dotimes (i 8) ... (go even) ...
                       ;;                       even (push i even) ...))
                       ;; the tag EVEN and the variable EVEN landed on the same
                       ;; alist entry, so (push i even), a SETQ, overwrote the
                       ;; GO-TARGET with a list and the NEXT (go even) reported
                       ;; "tag not found" (ansi-test DOTIMES.12 and the DO / DO* /
                       ;; DOLIST / TAGBODY tests of the same shape). Inner tagbodies
                       ;; shadow outer ones by consing in front.
                       (outer-tags (cdr (assoc '%mini-go-tags env)))
                       (tagged-env
                        (let ((tl '()) (idx 0))
                          (dolist (seg segs)
                            (when (car seg)
                              (push (cons (car seg) (cons tb-id idx)) tl))
                            (incf idx))
                          (cons (cons '%mini-go-tags (append (nreverse tl) outer-tags))
                                env)))
                       (done-marker (list 'done))
                       (start-idx 0))
                  ;; Body forms are evaluated with GOP on, so a GO that can reach
                  ;; here as a return value does that instead of throwing. The
                  ;; CATCH stays for the ones that cannot: a GO inside an
                  ;; argument, an init form, a handler, an interpreted function
                  ;; called from here: which still throw.
                  (loop
                    (let ((result
                           (catch tb-id
                             (let ((idx 0) (jump nil))
                               (dolist (seg segs)
                                 (when (and (null jump) (>= idx start-idx))
                                   (dolist (f (cdr seg))
                                     (let ((v (%mini-eval f tagged-env nil t)))
                                       (when (%mini-go-p v)
                                         (setq jump v)
                                         (return)))))
                                 (incf idx))
                               (cond ((null jump) done-marker)
                                     ;; Ours: loop again from the tag's segment.
                                     ((eq (cadr jump) tb-id) (cddr jump))
                                     ;; An enclosing TAGBODY's tag. Pass it on as
                                     ;; a marker if our own caller checks, else
                                     ;; put it back on the throw path.
                                     (gop (return-from %mini-eval jump))
                                     (t (%mini-throw-marker jump)))))))
                      (if (eq result done-marker)
                          (return nil)
                          (setq start-idx result))))))
               (go
                ;; Tags are looked up in the tag namespace only (see TAGBODY), so a
                ;; variable of the same name can neither hide a tag nor be clobbered
                ;; by one. Entry is (tag tb-id . index).
                (let* ((tag (cadr form))
                       (b (assoc tag (cdr (assoc '%mini-go-tags env)))))
                  (cond ((null b) (error "%mini-eval: go tag ~S not found" tag))
                        ;; (cdr b) is (tb-id . index), which is what the marker
                        ;; carries and what the CATCH would have thrown.
                        (gop (cons *%mini-go-marker* (cdr b)))
                        (t (throw (cadr b) (cddr b))))))
               (declare nil)
               (catch
                (catch (%mini-eval (cadr form) env)
                  (%mini-eval-progn (cddr form) env)))
               (throw
                (throw (%mini-eval (cadr form) env)
                       (%mini-eval (caddr form) env)))
               (unwind-protect
                (unwind-protect (%mini-eval (cadr form) env)
                  (%mini-eval-progn (cddr form) env)))
               (multiple-value-call
                ;; (m-v-call fn form*): splice all values of each form as args.
                (apply (%mini-eval (cadr form) env)
                       (mapcan (lambda (a)
                                 (multiple-value-list (%mini-eval a env)))
                               (cddr form))))
               (multiple-value-prog1
                (multiple-value-prog1 (%mini-eval (cadr form) env)
                  (%mini-eval-progn (cddr form) env)))
               (progv
                (progv (%mini-eval (cadr form) env)
                       (%mini-eval (caddr form) env)
                  (%mini-eval-progn (cdddr form) env)))
               (defun
                ;; Interpreted defun: install an interpreted closure (with the
                ;; implicit block) as the function. No compilation: works on
                ;; emit-free targets.
                ;;
                ;; %MINI-FN-LAMBDA, not a hand-built (lambda params (block ...)):
                ;; the declarations have to stay OUTSIDE the block. Buried inside
                ;; it, a parameter's (declare (special p)) looks like a FREE
                ;; declaration to %MINI-EVAL-PROGN: which then drops P's lexical
                ;; entry without establishing a dynamic binding, so the body reads
                ;; an unbound variable:
                ;;   (defun f (x &aux (y 10)) (declare (special x)) (+ x y))
                ;;   (f 5)   =>  Unbound variable: X
                ;; FLET / LABELS already went through %MINI-FN-LAMBDA for exactly
                ;; this reason; DEFUN was the one that did not. It only shows when
                ;; the DEFUN ITSELF is interpreted: LOAD compiles top-level forms
                ;; on an emit build, so it took running the suite on an emit-free
                ;; build to surface it.
                ;; The docstring is registered the same way COMPILE-DEFUN does
                ;; it, with (setf documentation). %MINI-FN-LAMBDA hoists it out of
                ;; the implicit block, but hoisting only moves it: as a form in
                ;; the lambda body it is evaluated and discarded, so an
                ;; interpreted DEFUN recorded nothing and (documentation 'f
                ;; 'function) answered NIL. Same shape as the DEFVAR family.
                ;; A docstring needs a body after it; "foo" alone IS the body.
                (let* ((name (progn (%check-defun-name (cadr form)) (cadr form)))
                       (params (caddr form))
                       (fbody (cdddr form))
                       (bname (if (consp name) (cadr name) name))
                       (doc (when (and (stringp (car fbody)) (cdr fbody))
                              (car fbody)))
                       (fn (%mini-make-closure
                            (%mini-fn-lambda params bname
                                             (%ftype-arg-declarations name params fbody))
                            env (mangle-name name))))
                  (setf (fdefinition name) fn)
                  ;; As COMPILE-DEFUN-TOPLEVEL does: a new definition of a name
                  ;; DEFSTRUCT registered as a keyword constructor decides what
                  ;; a call means, so the call-site construction is dropped.
                  (when (symbolp name)
                    (remhash name *struct-keyword-ctors*)
                    (remhash name *typed-vector-struct-accessors*))
                  (when doc (funcall #'(setf documentation) doc name 'function))
                  name))
               ;; The docstring is the 4th element and is stored INDEPENDENTLY of the
               ;; value: DEFVAR skips the init form when the variable is already
               ;; bound, but its documentation is still updated (CLHS defvar;
               ;; ansi-test DEFVAR.5 turns exactly on that). COMPILE-DEFVAR emitted
               ;; the SetVariableDocumentation call; these cases dropped the string
               ;; on the floor, so (documentation name 'variable) was NIL under EVAL
               ;; (DEFVAR.4/5, DEFPARAMETER.4/5).
               (defvar
                (let ((name (cadr form)) (doc (cadddr form)))
                  (proclaim (list 'special name))
                  (when (and (cddr form) (not (boundp name)))
                    (set name (%mini-eval (caddr form) env)))
                  (when (stringp doc) (setf (documentation name 'variable) doc))
                  name))
               (defparameter
                (let ((name (cadr form)) (doc (cadddr form)))
                  (proclaim (list 'special name))
                  (set name (%mini-eval (caddr form) env))
                  (when (stringp doc) (setf (documentation name 'variable) doc))
                  name))
               (defconstant
                ;; SET-SYMBOL-CONSTANT is what makes CONSTANTP answer T; without it
                ;; a constant defined through EVAL was an ordinary special. That
                ;; matters beyond introspection: SYMBOL-MACROLET's program-error
                ;; check consults CONSTANTP, and nothing could refuse to rebind
                ;; such a name. COMPILE-DEFVAR emitted the call for defconstant;
                ;; this case only did the SET and the PROCLAIM.
                (let ((name (cadr form)) (doc (cadddr form)))
                  (proclaim (list 'special name))
                  ;; %DEFINE-CONSTANT, not SET + SET-SYMBOL-CONSTANT: SET now
                  ;; refuses to assign a symbol that is already constant, and the
                  ;; same primitive is what refuses a redefinition to a non-EQL
                  ;; value. The compiled path emits a call to it too.
                  (%define-constant name (%mini-eval (caddr form) env))
                  (when (stringp doc) (setf (documentation name 'variable) doc))
                  name))
               (define-symbol-macro
                ;; Register into the global symbol-macro registry that
                ;; macroexpand-1 (and the symbol-read branch above) consults.
                (let ((name (cadr form)) (expansion (caddr form)))
                  (%register-symbol-macro-rt name expansion)
                  name))
               (defmacro
                ;; Interpreted defmacro: build a 2-arg (form env) interpreted
                ;; expander and register it via (setf (macro-function name) ...),
                ;; which feeds the runtime macro registry that macroexpand-1 reads.
                ;; Handles &whole / &environment / dotted tail; &body via
                ;; destructuring-bind. No compilation.
                (let* ((name (cadr form))
                       (ll0 (caddr form))
                       (mbody (cdddr form))
                       (whole-var (when (and (consp ll0) (eq (car ll0) '&whole))
                                    (cadr ll0)))
                       (rest-ll (if whole-var (cddr ll0) ll0))
                       (env-var nil)
                       (clean-ll
                        (let ((res '()) (p rest-ll))
                          (loop while (consp p) do
                            (cond ((eq (car p) '&environment)
                                   (setq env-var (cadr p) p (cddr p)))
                                  (t (push (car p) res) (setq p (cdr p)))))
                          (when p (push '&rest res) (push p res)) ; dotted tail
                          (nreverse res)))
                       (wv (or whole-var (gensym "MWHOLE")))
                       (ev (or env-var (gensym "MENV")))
                       (expander
                        (%mini-make-closure
                         `(lambda (,wv ,ev)
                            (declare (ignorable ,wv ,ev))
                            (destructuring-bind ,clean-ll (cdr ,wv)
                              (block ,name ,@mbody)))
                         env)))
                  (setf (macro-function name) expander)
                  ;; What a tool should show for a macro is the lambda list the
                  ;; user wrote, not the (whole env) pair this expander takes --
                  ;; %MINI-MAKE-CLOSURE has just recorded the latter, and on an
                  ;; emit-free build this is the only DEFMACRO path there is, so
                  ;; DOTCL:FUNCTION-LAMBDA-LIST answered (#:MWHOLE #:MENV).
                  (when (fboundp '%set-function-lambda-list)
                    (%set-function-lambda-list expander ll0))
                  ;; The docstring, as the interpreted DEFUN above records it.
                  ;; Left in the body it is only evaluated and discarded.
                  (when (and (stringp (car mbody)) (cdr mbody))
                    (funcall #'(setf documentation) (car mbody) name 'function))
                  name))
               (handler-case
                (%mini-eval (%mini-expand-handler-case form) env))
               (restart-case
                ;; (restart-case body (name (params) [:report/:interactive/:test x]* h...) ...)
                ;; Each spec is (name handler report interactive test). A
                ;; :report string stays a string; any other option value is a
                ;; function name or lambda expression, taken as (function x).
                (%call-with-restart-cluster
                 (mapcar (lambda (clause)
                           (let ((name (car clause))
                                 (params (cadr clause))
                                 (rest (cddr clause))
                                 (report nil) (interactive nil) (test nil))
                             (loop while (and rest (member (car rest)
                                                           '(:report :interactive :test)))
                                   do (let ((v (cadr rest)))
                                        (case (car rest)
                                          (:report (setq report v))
                                          (:interactive (setq interactive v))
                                          (:test (setq test v))))
                                      (setq rest (cddr rest)))
                             (flet ((fn (x)
                                      (if (or (null x) (stringp x))
                                          x
                                          (%mini-eval (list 'function x) env))))
                               (list name
                                     (%mini-make-closure `(lambda ,params ,@rest) env)
                                     (fn report) (fn interactive) (fn test)))))
                         (cddr form))
                 (%mini-make-closure (list 'lambda '() (cadr form)) env nil t)))
               (restart-bind
                ;; (restart-bind ((name function [:report/:interactive/:test x]*) ...) body...)
                (%call-with-restart-bind
                 (mapcar (lambda (binding)
                           (cons (car binding) (%mini-eval (cadr binding) env)))
                         (cadr form))
                 (%mini-make-closure (list* 'lambda '() (cddr form)) env nil t)))
               (multiple-value-list
                (multiple-value-list (%mini-eval (cadr form) env)))
               (%make-instance-with-initargs
                ;; Compiler intrinsic for (make-instance 'class ...): delegate to GF.
                (apply (symbol-function 'make-instance)
                       (%mini-eval-args (cdr form) env)))
               (t
                ;; %DOTIMES-1+ is a compiler intrinsic emitted by dotcl's DOTIMES
                ;; for a fixnum counter (an increment asserted to fit int64).
                ;; compile-expr lowers it to a raw add, but the interpreter never
                ;; sees a handler for it: so a macro body that uses DOTIMES and is
                ;; expanded through %MINI-EVAL (e.g. the DO-FPRS assembler macro)
                ;; would call %DOTIMES-1+ as an undefined function. The interpreter
                ;; has no int64 assertion, so it is simply 1+. Match by name: the
                ;; symbol may be DOTCL-INTERNAL:: or DOTCL.CIL-COMPILER:: depending
                ;; on how it was interned at emit time.
                (if (and (symbolp op) (string= (symbol-name op) "%DOTIMES-1+"))
                    (return-from %mini-eval (1+ (%mini-eval (cadr form) env)))
                    nil)
                ;; %MV-CAPTURE is MULTIPLE-VALUE-BIND's capture intrinsic. The
                ;; compiled form reads the thread values right after the call;
                ;; the interpreter cannot, because its own evaluation steps (a
                ;; symbol lookup is itself a two-value FIND-SYMBOL) sit in
                ;; between and overwrite them. So take the values as a list here.
                ;; Matched by name for the same reason as %DOTIMES-1+.
                (if (and (symbolp op) (string= (symbol-name op) "%MV-CAPTURE"))
                    (return-from %mini-eval
                      (%mv-capture-list
                       (multiple-value-list (%mini-eval (cadr form) env))))
                    nil)
                ;; %FIXNUM-GE-OBJECT is the other DOTIMES intrinsic: the loop test
                ;; for a counter in an Int64 slot against a boxed (possibly bignum)
                ;; limit. The interpreter has no unboxed counter, so it is >=.
                ;; Matched by name for the same reason as %DOTIMES-1+.
                ;; %DOTIMES-LIMIT and %LOOP-LIMIT-FLOOR clamp a loop limit into
                ;; int64 for the raw loop test. The interpreter compares with a
                ;; generic comparison, which takes the limit as it is.
                (if (and (symbolp op) (or (string= (symbol-name op) "%DOTIMES-LIMIT")
                                          (string= (symbol-name op) "%LOOP-LIMIT-FLOOR")))
                    (return-from %mini-eval (%mini-eval (cadr form) env))
                    nil)
                (if (and (symbolp op) (string= (symbol-name op) "%FIXNUM-GE-OBJECT"))
                    (return-from %mini-eval
                      (>= (%mini-eval (cadr form) env) (%mini-eval (caddr form) env)))
                    nil)
                ;; %DOTNET-CALL-DIRECT is the one compiler intrinsic on this path
                ;; that cannot be given a function binding: its third argument is
                ;; a literal list of parameter type names, which ordinary argument
                ;; evaluation would try to call. It is a special form, so the
                ;; interpreter needs its own case.
                ;;   (%dotnet-call-direct "Type" "Method" (param-types...) recv arg...)
                ;; The type and parameter-type strings only select an overload for
                ;; the direct callvirt; dropping them and going through the dynamic
                ;; DOTNET:INVOKE path is the same call, which is exactly what the
                ;; assembler itself falls back to when it cannot resolve the
                ;; overload. Matched by name for the same reason as %DOTIMES-1+.
                (if (and (symbolp op) (string= (symbol-name op) "%DOTNET-CALL-DIRECT"))
                    (return-from %mini-eval
                      (apply (symbol-function (find-symbol "INVOKE" "DOTNET"))
                             (%mini-eval (nth 4 form) env)
                             (caddr form)
                             (%mini-eval-args (nthcdr 5 form) env)))
                    nil)
                ;; A keyword structure constructor called at (safety 0). The
                ;; compiler builds such a call at the call site
                ;; (%STRUCT-CTOR-DIRECT-FORM) and its slot :TYPE checks then
                ;; follow the CALLER's safety, so under (safety 0) the value is
                ;; stored unchecked. Calling the constructor instead would check
                ;; with the constructor's own safety and refuse what the compiled
                ;; call accepts. Evaluate the same rewritten form here; its
                ;; checks are %CHECK-SLOT-TYPE forms, which the case above drops
                ;; at (safety 0). Above safety 0 the rewrite and the call check
                ;; the same thing. A call whose safety is plainly above 0 is let
                ;; through before the table is read: the table is synchronized,
                ;; and looking it up on every call made every thread evaluating
                ;; a call wait on every other.
                (when (and (symbolp op)
                           (not *cross-compiling*)
                           (not (%mini-safety-plainly-above-0-p env)))
                  (let ((entry (gethash op *struct-keyword-ctors*)))
                    (when (and entry
                               (not (%global-notinline-p op))
                               (%mini-safety-0-p env))
                      (let ((direct (%struct-ctor-direct-form entry (cdr form))))
                        (when direct
                          (return-from %mini-eval (%mini-eval direct env tailp)))))))
                ;; Function call: a local binding, a symbol's function, a
                ;; (setf name) function designator in operator position (e.g.
                ;; the ((setf foo) v place) form setf expands to), or a
                ;; ((lambda ...) ...) / computed-function operator.
                (let* ((fn (cond
                             ;; A lexical FLET/LABELS binding of OP was already
                             ;; taken at the top of this branch (LEX-FN), where it
                             ;; has to be looked up before macroexpansion to shadow
                             ;; a global macro of the same name (CLHS 3.1.2.1.2.4).
                             ;; Reaching here means there is none.
                             ;;
                             ;; %COERCE-TO-FUNCTION, not SYMBOL-FUNCTION: the
                             ;; compiled named-call path resolves through
                             ;; CilAssembler, which bridges a bare name across
                             ;; dotcl's own packages. SYMBOL-FUNCTION does not, so
                             ;; (class-precedence-list c), registered on a
                             ;; DOTCL-MOP symbol, worked compiled and through
                             ;; FUNCALL but not as a plain form under the
                             ;; interpreter.
                             ((symbolp op) (%coerce-to-function op))
                             ((and (consp op) (eq (car op) 'setf))
                              (fdefinition op))
                             (t (%mini-eval op env))))
                       (args (%mini-eval-args (cdr form) env)))
                  (%mini-call fn args tailp env))))))))))))))
    (t form)))
