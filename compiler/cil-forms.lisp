;;; cil-forms.lisp — Special forms, functions, control flow
;;; Part of the CIL compiler (A2 instruction list architecture)

(in-package :dotcl.cil-compiler)

;;; TCO state — must be declared before any function that uses them
;;; (SBCL needs defvar evaluated before let* can dynamically bind them)
(defvar *tco-self-name* nil
  "Mangled name of function currently being compiled for self-TCO. NIL = disabled.")
(defvar *tco-self-symbol* nil
  "Original symbol of function currently being compiled for self-TCO. NIL = disabled.")
(defvar *tco-loop-label* nil
  "Label name to branch back to for TCO self-call.")
(defvar *tco-param-entries* nil
  "List of (key . boxed-p) in param order, for rewriting TCO self-call args.
   key = gen-local string; boxed-p = T if the local is a LispObject[] box.")
(defvar *tco-local-fn-key* nil
  "Box key (gen-local string) of the labels function currently being compiled
   for self-TCO, or NIL if compiling a defun. Allows compile-named-call to
   permit self-TCO despite the function name appearing in *local-functions*.")
(defvar *tco-leave-instrs* nil
  "List of CIL instructions to emit BEFORE the TCO branch (br or leave).
   Used when the TCO site is inside a try block that requires explicit cleanup
   before branching back (e.g. handler-case must call HandlerClusterStack.PopCluster
   before leaving the catch-protected region). NIL for ordinary TCO.")
(defvar *tco-in-try-catch* nil
  "T when the current TCO site is inside a handler-case try/catch body (not
   a try/finally). In this case, TCO uses `leave` to exit the protected region
   instead of `br`. Distinct from *in-try-block* which signals try/finally
   (special-variable LET / unwind-protect) and always suppresses TCO (#126).")
(defvar *in-tail-position* nil
  "T when the currently-being-compiled expression is in tail position
   within the TCO scope. Reset to NIL at function entry; compile-progn
   and compile-if set it appropriately.")

(defvar *labels-mutual-tco* nil
  "Dispatch table for labels mutual TCO (#124/D919). Each entry:
   (name-str fn-index which-fn-key tcoloop-label shared-param-keys).
   NIL outside a mutual-TCO labels group. Reset to NIL in every
   compile-function-body-*/compile-closure-body so closures compiled
   within the group never emit br-to-outer-TCOLOOP.")

(defvar *in-try-block* nil
  "Non-NIL when the currently-being-compiled expression is inside a try/
   finally region whose finally must run on exit (e.g. special-variable
   LET, UNWIND-PROTECT). TCO branches (`br` to the loop label) cannot
   legally cross such a region — IL requires `leave`, which this compiler
   does not yet emit for tail-recursion. Suppressing TCO via this flag
   avoids invalid IL while keeping MV propagation intact.")

(defvar *self-fn-local* nil
  "When set, name of the local variable holding the current function's own
   LispFunction object. compile-named-call uses this instead of doing a full
   load-sym-pkg / GetFunctionBySymbol sequence for non-tail self-calls.")

(defvar *fixnum-locals* '()
  "List of symbol-name strings for lexical locals declared (fixnum X) /
   (type fixnum X) / (type (integer LO HI) X) with bounded fixnum range.
   compile-as-long treats references to these as int64 unbox sites, and
   fixnum-typed-p reports them as fixnum. Values are boxed LispObject at
   the slot; unboxing happens inline in compile-as-long (castclass Fixnum
   + get_Value). Caller-side guarantee: the user's declaration contract.")

(defvar *double-float-locals* '()
  "Like *fixnum-locals* but for double-float declarations. Enables native
   r8 arithmetic on (declare (double-float x)) locals and references.")

(defvar *single-float-locals* '()
  "Like *double-float-locals* but for single-float declarations. Enables native
   r4 arithmetic on (declare (single-float x)) locals and references.")

(defvar *long-locals* '()
  "List of symbol-name strings whose local slots hold Int64 directly (not boxed
   LispObject). Set in native function bodies where params are long-typed.
   compile-as-long skips :unbox-fixnum for these; fixnum-typed-p returns T (#130).")

(defvar *native-self-name* nil
  "Mangled name of the current function if it is native-eligible (all fixnum params
   + fixnum return, no captures, no specials). Enables native self-call path
   in compile-as-long and native TCO arg evaluation (#130).")

(defvar *compile-time-flet-defs* nil
  "List of flet function source definitions active during compilation.
   Each entry is (name lambda-list . body). Used by compile-defmacro
   to make flet-local functions available during compile-time eval.")

(defun tail-prefix-instrs ()
  "Return ((:tail-prefix)) when the current call site is in tail position and
  outside any try/finally region, else NIL. Used by compile-named-call etc.
  to prepend the CIL `tail.` prefix to the final call instruction. CLR JIT
  may honor or ignore; for us it's a safe hint that helps some patterns
  (e.g. mutual recursion, funcall of external function in tail pos) (D683)."
  (when (and *in-tail-position* (not *in-try-block*))
    '((:tail-prefix))))

(defun maybe-tail-callvirt (instrs)
  "Post-pass for compile-function-body-direct: if INSTRS ends with (:callvirt ...),
  insert (:tail-prefix) immediately before it. Only called when there is no
  try/finally wrapping the body (special-param-syms is nil), so the sequence
  (:tail-prefix) (:callvirt ...) (:ret) is valid CIL."
  (let ((last (and (consp instrs) (car (last instrs)))))
    (if (and (consp last) (eq (car last) :callvirt))
        (append (butlast instrs 1) '((:tail-prefix)) (list last))
        instrs)))

(defun compile-and-pop (form)
  "Compile FORM for effect. Uses void call variants when available to avoid allocation.
   Returns CIL instructions without (:pop) if a void variant was used."
  (if (and (consp form)
           (eq (car form) 'vector-push-extend)
           (= (length (cdr form)) 2))
      ;; Direct vector-push-extend in for-effect position: use void variant (no Fixnum.Make)
      (compile-binary-call (cdr form) "Runtime.VectorPushExtendVoid2")
      `(,@(compile-expr form) (:pop))))

;;; ============================================================
;;; Named function call (user-defined)
;;; ============================================================

(defun compile-direct-call-args (args)
  "Pre-evaluate args to individual temp locals (not array).
   Returns (temps . eval-instrs) where temps is list of local names.
   Args are never in tail position and receive a single value each."
  (let ((temps (loop for a in args collect (gen-local "DA"))))
    (cons temps
          (loop for arg in args
                for tmp in temps
                append `((:declare-local ,tmp "LispObject")
                         ,@(let ((*in-tail-position* nil)
                                 (*in-mv-context* nil))
                             (compile-expr arg)) (:stloc ,tmp))))))

(defun compile-direct-call-args-long (args)
  "Like compile-direct-call-args but evaluates each arg via compile-as-long → Int64.
   Used in native function bodies for TCO self-call argument evaluation (#130)."
  (let ((temps (loop for a in args collect (gen-local "DA"))))
    (cons temps
          (loop for arg in args
                for tmp in temps
                append `((:declare-local ,tmp "Int64")
                         ,@(let ((*in-tail-position* nil)
                                 (*in-mv-context* nil))
                             (compile-as-long arg))
                         (:stloc ,tmp))))))

(defun compile-named-call (name args)
  (block compile-named-call
    ;; Self-TCO: if in tail position and calling current function, emit loop
    ;; Use symbol identity (eq) not just name string to avoid cross-package false matches
    ;; (e.g. uiop/os:getenv calling dotcl:getenv must not be treated as self-recursion)
    ;; Skip TCO when a local function (flet/labels) shadows the defun name
    (let ((name-str (mangle-name name))
          (n-args (length args)))
      (when (and *in-tail-position*
                 ;; try/finally (special-var LET) suppresses TCO, but handler-case's
                 ;; try/catch allows it via *tco-in-try-catch* (uses `leave`, not `br`)
                 (or *tco-in-try-catch* (not *in-try-block*))
                 *tco-self-name*
                 ;; Skip TCO when a different local function shadows the name;
                 ;; allow when the shadow IS the labels fn being compiled (#125).
                 (let ((lf (assoc name-str *local-functions* :test #'string=)))
                   (or (null lf)
                       (and *tco-local-fn-key* (string= (second lf) *tco-local-fn-key*))))
                 (if *tco-self-symbol*
                     (eq name *tco-self-symbol*)
                     (string= name-str *tco-self-name*))
                 (= n-args (length *tco-param-entries*)))
        (let* ((use-native-tco (and *native-self-name* (every #'fixnum-typed-p args)))
               (da (if use-native-tco
                       (compile-direct-call-args-long args)
                       (compile-direct-call-args args)))
               (temps (car da))
               (eval-instrs (cdr da))
               (store-instrs
                 (if use-native-tco
                     ;; Native body: all params are Int64, temps are Int64 → direct store
                     (loop for tmp in temps
                           for (key . boxed-p) in *tco-param-entries*
                           append `((:ldloc ,tmp) (:stloc ,key)))
                     ;; Normal body: LispObject temps → LispObject or boxed params
                     (loop for tmp in temps
                           for (key . boxed-p) in *tco-param-entries*
                           if boxed-p
                             append `((:ldloc ,key) (:ldc-i4 0) (:ldloc ,tmp) (:stelem-ref))
                           else
                             append `((:ldloc ,tmp) (:stloc ,key))))))
          (return-from compile-named-call
            `(,@eval-instrs
              ,@store-instrs
              ,@*tco-leave-instrs*
              ;; handler-case try/catch: use `leave` to exit cleanly (#126).
              ;; try/finally (special-var LET) already suppressed above via *in-try-block*.
              ,(if *tco-in-try-catch*
                   `(:leave ,*tco-loop-label*)
                   `(:br ,*tco-loop-label*))))))
      ;; Mutual-TCO: tail call to a labels sibling → update shared params + br TCOLOOP (#124/D919)
      (when (and *in-tail-position*
                 (or *tco-in-try-catch* (not *in-try-block*))
                 *labels-mutual-tco*)
        (let ((mtco (assoc name-str *labels-mutual-tco* :test #'string=)))
          (when mtco
            (let* ((fn-index (second mtco))
                   (which-fn-key (third mtco))
                   (tcoloop-label (fourth mtco))
                   (shared-param-keys (fifth mtco)))
              (when (= n-args (length shared-param-keys))
                (let* ((da (compile-direct-call-args args))
                       (temps (car da))
                       (eval-instrs (cdr da)))
                  (return-from compile-named-call
                    `(,@eval-instrs
                      ,@(loop for tmp in temps
                              for key in shared-param-keys
                              append `((:ldloc ,tmp) (:stloc ,key)))
                      (:ldc-i4 ,fn-index)
                      (:stloc ,which-fn-key)
                      ,@*tco-leave-instrs*
                      ,(if *tco-in-try-catch*
                           `(:leave ,tcoloop-label)
                           `(:br ,tcoloop-label))))))))))
      ;; Non-tail self-call fast path: reuse LispFunction cached at body entry.
      (when (and *self-fn-local*
                 (not (assoc name-str *local-functions* :test #'string=))
                 (if *tco-self-symbol*
                     (eq name *tco-self-symbol*)
                     (string= name-str *tco-self-name*))
                 (<= n-args 8))
        (let* ((skip-reset (single-value-form-p (cons name args)))
               (da (compile-direct-call-args args))
               (temps (car da))
               (eval-instrs (cdr da)))
          (return-from compile-named-call
            `(,@eval-instrs
              (:ldloc ,*self-fn-local*)
              ,@(unless skip-reset '((:call "MultipleValues.Reset")))
              ,@(loop for tmp in temps append `((:ldloc ,tmp)))
              (:callvirt ,(format nil "LispFunction.Invoke~D" n-args)))))))
    ;; Inline struct accessor: (accessor-name obj) → StructRefI with raw int index
    ;; Only when not shadowed by a local function (flet/labels)
    (when (and (symbolp name)
               (= (length args) 1)
               (not (assoc (mangle-name name) *local-functions* :test #'string=)))
      (let ((slot-idx (gethash (symbol-name name) *struct-accessors*)))
        (when slot-idx
          (return-from compile-named-call
            `(,@(let ((*in-tail-position* nil)) (compile-expr (car args)))
              (:ldc-i4 ,slot-idx)
              (:call "Runtime.StructRefI"))))))
    ;; --- original compile-named-call body (unchanged) ---
    ;; Compile args first (into temp), then load function and invoke.
    ;; This ensures the stack is empty during arg evaluation, which is
    ;; required by CIL when args contain try blocks (e.g. loop with block).
    (let ((args-tmp (gen-local "NCARGS"))
          (name-str (mangle-name name))
          (n-args (length args))
          (local-fn (assoc (mangle-name name) *local-functions* :test #'string=))
          (skip-reset (single-value-form-p (cons name args))))
    (if local-fn
        ;; Local function (flet/labels): load from local or box, cast, invoke
        (let ((key (second local-fn))
              (boxed-p (third local-fn)))
          (if (<= n-args 8)
              ;; Direct invoke for small arg count (0-8 args, no array allocation)
              (let* ((da (compile-direct-call-args args))
                     (temps (car da))
                     (eval-instrs (cdr da)))
                `(,@eval-instrs
                  ,@(if boxed-p
                        `((:ldloc ,key) (:ldc-i4 0) (:ldelem-ref))
                        `((:ldloc ,key)))
                  (:castclass "LispFunction")
                  ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                  ,@(loop for tmp in temps append `((:ldloc ,tmp)))
                  (:callvirt ,(format nil "LispFunction.Invoke~D" n-args))))
              `((:declare-local ,args-tmp "LispObject[]")
                ,@(compile-args-array args) (:stloc ,args-tmp)
                ,@(if boxed-p
                      `((:ldloc ,key) (:ldc-i4 0) (:ldelem-ref))
                      `((:ldloc ,key)))
                (:castclass "LispFunction")
                ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                (:ldloc ,args-tmp)
                (:callvirt "LispFunction.Invoke"))))
        ;; Check if it's a captured boxed local (e.g. labels functions captured in closure)
        ;; Skip for (setf foo) list names — they are never local variables
        (let ((local-entry (and (symbolp name) (local-bound-p name))))
          (if (and local-entry (boxed-var-p name))
              ;; It's a boxed var — load box[0], cast to LispFunction, invoke
              (let ((key (lookup-local name)))
                (if (<= n-args 8)
                    (let* ((da (compile-direct-call-args args))
                           (temps (car da))
                           (eval-instrs (cdr da)))
                      `(,@eval-instrs
                        (:ldloc ,key) (:ldc-i4 0) (:ldelem-ref)
                        (:castclass "LispFunction")
                        ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                        ,@(loop for tmp in temps append `((:ldloc ,tmp)))
                        (:callvirt ,(format nil "LispFunction.Invoke~D" n-args))))
                    `((:declare-local ,args-tmp "LispObject[]")
                      ,@(compile-args-array args) (:stloc ,args-tmp)
                      (:ldloc ,key) (:ldc-i4 0) (:ldelem-ref)
                      (:castclass "LispFunction")
                      ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                      (:ldloc ,args-tmp)
                      (:callvirt "LispFunction.Invoke"))))
              ;; Global function — use symbol-based lookup (D115: fixes flat namespace collision)
              (if (symbolp name)
                  (if (<= n-args 8)
                      (if (every #'simple-expr-p args)
                          ;; Fast path: simple args → skip temps, push directly
                          `(,@(compile-sym-lookup name)
                            (:castclass "Symbol")
                            (:call "CilAssembler.GetFunctionBySymbol")
                            ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                            ,@(loop for arg in args
                                    append (let ((*in-tail-position* nil)) (compile-expr arg)))
                            (:callvirt ,(format nil "LispFunction.Invoke~D" n-args)))
                          (let* ((da (compile-direct-call-args args))
                                 (temps (car da))
                                 (eval-instrs (cdr da)))
                            `(,@eval-instrs
                              ,@(compile-sym-lookup name)
                              (:castclass "Symbol")
                              (:call "CilAssembler.GetFunctionBySymbol")
                              ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                              ,@(loop for tmp in temps append `((:ldloc ,tmp)))
                              (:callvirt ,(format nil "LispFunction.Invoke~D" n-args)))))
                      `((:declare-local ,args-tmp "LispObject[]")
                        ,@(compile-args-array args) (:stloc ,args-tmp)
                        ,@(compile-sym-lookup name)
                        (:castclass "Symbol")
                        (:call "CilAssembler.GetFunctionBySymbol")
                        ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                        (:ldloc ,args-tmp)
                        (:callvirt "LispFunction.Invoke")))
                  (if (<= n-args 8)
                      (let* ((da (compile-direct-call-args args))
                             (temps (car da))
                             (eval-instrs (cdr da)))
                        `(,@eval-instrs
                          (:ldstr ,(mangle-name name))
                          (:call "CilAssembler.GetFunction")
                          ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                          ,@(loop for tmp in temps append `((:ldloc ,tmp)))
                          (:callvirt ,(format nil "LispFunction.Invoke~D" n-args))))
                      `((:declare-local ,args-tmp "LispObject[]")
                        ,@(compile-args-array args) (:stloc ,args-tmp)
                        (:ldstr ,(mangle-name name))
                        (:call "CilAssembler.GetFunction")
                        ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                        (:ldloc ,args-tmp)
                        (:callvirt "LispFunction.Invoke"))))))))))


;;; ============================================================
;;; if
;;; ============================================================

(defun compile-if-fused-comparison-p (cond-expr)
  "If cond-expr is a binary comparison suitable for fused comparison+branch,
   return (:binary method-name) or (:unary method-name).
   Returns nil otherwise."
  (when (and (consp cond-expr)
             (symbolp (car cond-expr))
             ;; Don't fuse if the comparison is a local function (flet/labels shadowing)
             (not (assoc (mangle-name (car cond-expr)) *local-functions* :test #'string=)))
    (let ((op (car cond-expr))
          (nargs (length (cdr cond-expr)))
          (args (cdr cond-expr)))
      (cond
        ;; Special case: (= x 0) or (= 0 x) → zerop optimization
        ((and (eq op '=) (= nargs 2)
              (or (eql (second cond-expr) 0) (eql (third cond-expr) 0)))
         (let ((non-zero-arg (if (eql (second cond-expr) 0) (third cond-expr) (second cond-expr))))
           (list :unary "Runtime.IsTrueZerop" (list non-zero-arg))))
        ;; Double-float-typed fast path: both args statically double-float →
        ;; native r8 compare. Emitted as compile-double-cmp (D672).
        ((and (= nargs 2)
              (member op '(< > <= >= = /=))
              (double-float-typed-p (first args))
              (double-float-typed-p (second args)))
         (list :double-cmp
               (ecase op (< :lt) (> :gt) (<= :le) (>= :ge) (= :eq) (/= :ne))
               args))
        ;; Fixnum-typed fast path: both args statically fixnum → native i8 compare.
        ;; Emitted as compile-fixnum-cmp which leaves an i4 (0/1) on stack.
        ((and (= nargs 2)
              (member op '(< > <= >= = /=))
              (fixnum-typed-p (first args))
              (fixnum-typed-p (second args)))
         (list :fixnum-cmp
               (ecase op (< :lt) (> :gt) (<= :le) (>= :ge) (= :eq) (/= :ne))
               args))
        ;; Binary numeric comparisons (generic path)
        ((and (= nargs 2)
              (cdr (assoc op '((> . "Runtime.IsTrueGt")
                               (< . "Runtime.IsTrueLt")
                               (>= . "Runtime.IsTrueGe")
                               (<= . "Runtime.IsTrueLe")
                               (= . "Runtime.IsTrueNumEq")))))
         (list :binary (cdr (assoc op '((> . "Runtime.IsTrueGt")
                                        (< . "Runtime.IsTrueLt")
                                        (>= . "Runtime.IsTrueGe")
                                        (<= . "Runtime.IsTrueLe")
                                        (= . "Runtime.IsTrueNumEq")))) args))
        ;; Unary predicates: zerop, minusp, plusp
        ((and (= nargs 1)
              (member op '(zerop minusp plusp)))
         (list :unary (cdr (assoc op '((zerop . "Runtime.IsTrueZerop")
                                       (minusp . "Runtime.IsTrueMinusp")
                                       (plusp . "Runtime.IsTruePlusp")))) args))
        ;; Binary equality: eq, eql, equal
        ((and (= nargs 2)
              (member op '(eq eql equal)))
         (list :binary (cdr (assoc op '((eq . "Runtime.IsTrueEq")
                                        (eql . "Runtime.IsTrueEql")
                                        (equal . "Runtime.IsTrueEqual")))) args))
        ;; Unary type predicates: consp, atom, null (as predicate, not as not)
        ((and (= nargs 1)
              (member op '(consp atom listp numberp integerp symbolp stringp
                           characterp functionp)))
         (list :unary (cdr (assoc op '((consp . "Runtime.IsTrueConsp")
                                       (atom . "Runtime.IsTrueAtom")
                                       (listp . "Runtime.IsTrueListp")
                                       (numberp . "Runtime.IsTrueNumberp")
                                       (integerp . "Runtime.IsTrueIntegerp")
                                       (symbolp . "Runtime.IsTrueSymbolp")
                                       (stringp . "Runtime.IsTrueStringp")
                                       (characterp . "Runtime.IsTrueCharacterp")
                                       (functionp . "Runtime.IsTrueFunctionp")))) args))
        ;; (typep x 'known-type) → IsTrueXxx fused predicate
        ((and (eq op 'typep) (= nargs 2)
              (let ((type-arg (second args)))
                (and (consp type-arg) (eq (car type-arg) 'quote)
                     (symbolp (cadr type-arg)))))
         (let* ((type-name (cadr (second args)))
                (predicate (cdr (assoc type-name
                                  '((cons . "Runtime.IsTrueConsp")
                                    (list . "Runtime.IsTrueListp")
                                    (number . "Runtime.IsTrueNumberp")
                                    (integer . "Runtime.IsTrueIntegerp")
                                    (symbol . "Runtime.IsTrueSymbolp")
                                    (string . "Runtime.IsTrueStringp")
                                    (character . "Runtime.IsTrueCharacterp")
                                    (function . "Runtime.IsTrueFunctionp")
                                    (atom . "Runtime.IsTrueAtom"))))))
           (if predicate
               (list :unary predicate (list (first args)))
               ;; Non-standard type: use IsTrueTypep for fused branch
               (list :binary "Runtime.IsTrueTypep" args))))
        (t nil)))))

(defun compile-boolean-branch (expr label branch-on-true)
  "Compile expr as a boolean condition and emit a branch to label.
   If branch-on-true, branch when condition is true (brtrue).
   Otherwise, branch when condition is false (brfalse).
   Handles fused comparisons, (not x), nested (and ...) / (or ...) recursively."
  (let ((fused (compile-if-fused-comparison-p expr)))
    (cond
      ;; Fused comparison: IsTrueXxx → bool → branch
      (fused
       `(,@(let ((*in-tail-position* nil))
             (ecase (first fused)
               (:binary (compile-binary-call (third fused) (second fused)))
               (:unary (compile-unary-call (third fused) (second fused)))
               (:fixnum-cmp (compile-fixnum-cmp (third fused) (second fused)))
               (:double-cmp (compile-double-cmp (third fused) (second fused)))))
         (,(if branch-on-true :brtrue :brfalse) ,label)))
      ;; (not x) / (null x): negate direction and recurse
      ((and (consp expr)
            (symbolp (car expr))
            (member (car expr) '(not null))
            (= (length (cdr expr)) 1)
            (not (assoc (mangle-name (car expr)) *local-functions* :test #'string=)))
       (compile-boolean-branch (cadr expr) label (not branch-on-true)))
      ;; (and ...) in boolean context
      ((and (consp expr) (eq (car expr) 'and) (cdr expr))
       (if branch-on-true
           ;; branch-on-true: all must be true → chain brfalse to fail, then br to label
           (let ((fail-label (gen-label "ANDFAIL")))
             `(,@(loop for sub in (cdr expr)
                       append (compile-boolean-branch sub fail-label nil))
               (:br ,label)
               (:label ,fail-label)))
           ;; branch-on-false: any false → branch to label
           `(,@(loop for sub in (cdr expr)
                     append (compile-boolean-branch sub label nil)))))
      ;; (or ...) in boolean context
      ((and (consp expr) (eq (car expr) 'or) (cdr expr))
       (if branch-on-true
           ;; branch-on-true: any true → branch to label
           `(,@(loop for sub in (cdr expr)
                     append (compile-boolean-branch sub label t)))
           ;; branch-on-false: all must be false → chain brtrue to pass, then br to label
           (let ((pass-label (gen-label "ORPASS")))
             `(,@(loop for sub in (cdr expr)
                       append (compile-boolean-branch sub pass-label t))
               (:br ,label)
               (:label ,pass-label)))))
      ;; Default: IsTruthy
      (t
       `(,@(let ((*in-tail-position* nil)) (compile-expr expr))
         (:call "Runtime.IsTruthy")
         (:call "MultipleValues.Reset")
         (,(if branch-on-true :brtrue :brfalse) ,label))))))

(defun compile-if (args)
  (let ((else-label (gen-label "ELSE"))
        (end-label (gen-label "END"))
        (cond-expr (first args))
        (fused-method nil))
    ;; Check for fused comparison+branch optimization
    (cond
      ;; Fused comparison: skip IsTruthy (still reset MV)
      ((setq fused-method (compile-if-fused-comparison-p cond-expr))
       `(,@(let ((*in-tail-position* nil))
             (ecase (first fused-method)
               (:binary (compile-binary-call (third fused-method) (second fused-method)))
               (:unary (compile-unary-call (third fused-method) (second fused-method)))
               (:fixnum-cmp (compile-fixnum-cmp (third fused-method) (second fused-method)))
               (:double-cmp (compile-double-cmp (third fused-method) (second fused-method)))))
         (:brfalse ,else-label)
         ,@(compile-expr (second args))
         (:br ,end-label)
         (:label ,else-label)
         ,@(if (third args)
               (compile-expr (third args))
               (let ((*in-tail-position* nil)) (emit-nil)))
         (:label ,end-label)))
      ;; (if (and ...) then else): chain brfalse to else for each condition
      ((and (consp cond-expr) (eq (car cond-expr) 'and) (cddr cond-expr))
       `(,@(loop for sub in (cdr cond-expr)
                 append (compile-boolean-branch sub else-label nil))
         ,@(compile-expr (second args))
         (:br ,end-label)
         (:label ,else-label)
         ,@(if (third args)
               (compile-expr (third args))
               (let ((*in-tail-position* nil)) (emit-nil)))
         (:label ,end-label)))
      ;; (if (or ...) then else): chain brtrue to then for each, fall through to else
      ((and (consp cond-expr) (eq (car cond-expr) 'or) (cddr cond-expr))
       (let ((or-true (gen-label "ORTRUE")))
         `(,@(loop for (sub . rest) on (cdr cond-expr)
                   if rest
                     append (compile-boolean-branch sub or-true t)
                   else
                     append (compile-boolean-branch sub else-label nil))
           (:label ,or-true)
           ,@(compile-expr (second args))
           (:br ,end-label)
           (:label ,else-label)
           ,@(if (third args)
                 (compile-expr (third args))
                 (let ((*in-tail-position* nil)) (emit-nil)))
           (:label ,end-label))))
      ;; Fused (not x) / (null x): negate the branch
      ((and (consp cond-expr)
            (symbolp (car cond-expr))
            (member (car cond-expr) '(not null))
            (= (length (cdr cond-expr)) 1))
       (let ((inner-fused (compile-if-fused-comparison-p (cadr cond-expr))))
         (if inner-fused
             ;; Inner expression is fusable: use fused comparison with negated branch
             `(,@(let ((*in-tail-position* nil))
                   (ecase (first inner-fused)
                     (:binary (compile-binary-call (third inner-fused) (second inner-fused)))
                     (:unary (compile-unary-call (third inner-fused) (second inner-fused)))
                     (:fixnum-cmp (compile-fixnum-cmp (third inner-fused) (second inner-fused)))
                     (:double-cmp (compile-double-cmp (third inner-fused) (second inner-fused)))))
               (:brtrue ,else-label)  ;; Negate: branch to else when TRUE
               ,@(compile-expr (second args))
               (:br ,end-label)
               (:label ,else-label)
               ,@(if (third args)
                     (compile-expr (third args))
                     (let ((*in-tail-position* nil)) (emit-nil)))
               (:label ,end-label))
             ;; Default: IsTruthy + negate
             `(,@(let ((*in-tail-position* nil)) (compile-expr (cadr cond-expr)))
               (:call "Runtime.IsTruthy")
               (:call "MultipleValues.Reset")
               (:brtrue ,else-label)  ;; Negate: branch to else when TRUE (meaning (not x) is false)
               ,@(compile-expr (second args))
               (:br ,end-label)
               (:label ,else-label)
               ,@(if (third args)
                     (compile-expr (third args))
                     (let ((*in-tail-position* nil)) (emit-nil)))
               (:label ,end-label)))))
      ;; Default: general condition
      (t
       `(,@(let ((*in-tail-position* nil)) (compile-expr cond-expr))
         (:call "Runtime.IsTruthy")
         (:call "MultipleValues.Reset")
         (:brfalse ,else-label)
         ,@(compile-expr (second args))
         (:br ,end-label)
         (:label ,else-label)
         ,@(if (third args)
               (compile-expr (third args))
               (let ((*in-tail-position* nil)) (emit-nil)))
         (:label ,end-label))))))

;;; ============================================================
;;; progn
;;; ============================================================

(defun compile-progn (forms)
  (cond
    ((null forms) (emit-nil))
    ;; Single form — inherits *in-tail-position* from caller
    ((null (cdr forms)) (compile-expr (car forms)))
    (t (append
        ;; Non-last forms: never in tail position
        (let ((*in-tail-position* nil))
          (loop for form in (butlast forms)
                append `(,@(compile-and-pop form)
                          (:call "MultipleValues.Reset"))))
        ;; Last form: inherits outer *in-tail-position*
        (compile-expr (car (last forms)))))))

;;; ============================================================
;;; defvar / defparameter
;;; ============================================================

(defun compile-defvar (name init-form has-init-p &optional (is-defvar t) (is-defconstant nil) docstring)
  "Compile (defvar name value [doc]) or (defparameter name value [doc]) or (defconstant name value [doc]).
   Returns the symbol name. Stores docstring if provided."
  ;; Register as globally special at compile time
  (pushnew name *specials*)
  (pushnew name *global-specials*)
  (let ((val-local (gen-local "DEFVAR-VAL"))
        (sym-local (gen-local "DEFVAR-SYM")))
    `((:declare-local ,val-local "LispObject")
      (:declare-local ,sym-local "LispObject")
      ,@(compile-sym-lookup name)
      (:castclass "Symbol")
      (:stloc ,sym-local)
      ,@(if has-init-p
            (let ((skip-label (gen-label "DEFVAR-SKIP")))
              `(,@(if is-defvar
                      ;; defvar: only evaluate and set init-form if unbound
                      `((:ldloc ,sym-local)
                        (:call "Runtime.Boundp")
                        (:call "Runtime.IsTruthy")
                        (:brtrue ,skip-label)
                        ,@(compile-expr init-form)
                        (:stloc ,val-local)
                        (:ldloc ,sym-local)
                        (:ldloc ,val-local)
                        (:call "DynamicBindings.Set")
                        (:pop)
                        (:label ,skip-label))
                      ;; defparameter / defconstant: always set
                      `(,@(compile-expr init-form)
                        (:stloc ,val-local)
                        (:ldloc ,sym-local)
                        (:ldloc ,val-local)
                        (:call ,(if is-defconstant "DynamicBindings.Set" "DynamicBindings.Set"))
                        (:pop)
                        ,@(if is-defconstant
                              `((:ldloc ,sym-local)
                                (:call "Runtime.SetSymbolConstant")
                                (:pop))
                              '())))))
            '())
      ;; Mark the symbol as special at runtime (needed for self-hosting)
      (:ldloc ,sym-local)
      (:call "Runtime.MarkSpecial")
      (:pop)
      ,@(if (and docstring (stringp docstring))
            `((:ldloc ,sym-local)
              (:ldstr ,docstring)
              (:newobj "LispString")
              (:call "Runtime.SetVariableDocumentation")
              (:pop))
            '())
      (:ldloc ,sym-local))))

;;; ============================================================
;;; defun
;;; ============================================================

(defun form-has-return-from-p (name form)
  "Check if FORM contains (return-from NAME ...) anywhere in the tree.
   Stops descending into nested blocks/defuns that shadow NAME."
  (cond
    ((atom form) nil)
    ;; (return-from X ...) — match if X eq NAME
    ((and (eq (car form) 'return-from)
          (consp (cdr form))
          (eq (cadr form) name))
     t)
    ;; (block NAME ...) shadows our block name — don't descend
    ((and (eq (car form) 'block)
          (consp (cdr form))
          (eq (cadr form) name))
     nil)
    ;; (defun NAME ...) also creates an implicit block — don't descend
    ((and (eq (car form) 'defun)
          (consp (cdr form))
          (eq (cadr form) name))
     nil)
    ;; Recurse into subforms (handle dotted pairs safely)
    (t (do ((cur form (cdr cur)))
           ((atom cur) (when cur (form-has-return-from-p name cur)))
         (when (form-has-return-from-p name (car cur))
           (return t))))))

(defun defun-pkg-spec (name)
  "Return (:pkg \"PKG\") if name refers to a symbol with a home package, nil otherwise.
   For (setf NAME) forms, uses the home package of NAME (the setf-target symbol).
   Used to pass package info to :defmethod so the assembler registers the function
   on the correct symbol (not just *package*'s inherited symbol, which at FASL load
   time is often CL-USER rather than the compile-time *package*)."
  (let ((target (cond ((symbolp name) name)
                      ((and (consp name) (eq (car name) 'setf) (symbolp (cadr name)))
                       (cadr name)))))
    (when (and target (symbol-package target))
      `(:pkg ,(package-name (symbol-package target))))))

(defun compile-defun (name params body)
  "Compile (defun name (params) body...) → :defmethod directive + return symbol."
  ;; Pre-pass: infer return type before body compilation so self-calls inside the
  ;; body benefit from the D685 single-value elision path (#129).
  (when (and (symbolp name) (not (gethash name *function-return-types*)))
    (let ((inferred (infer-body-return-type body (mangle-name name))))
      (when inferred
        (setf (gethash name *function-return-types*) inferred))))
  (multiple-value-bind (required optional key rest-param) (parse-lambda-list params)
    (declare (ignore optional key rest-param))
    (let* ((param-names (mapcar #'symbol-name required))
           (block-name (cond ((and (consp name) (eq (car name) 'setf)) (cadr name))
                             ((consp name) (cadr name)) ; (cas foo) → block named foo
                             (t name)))
           (has-literal-return-from
            (or (some (lambda (f) (form-has-return-from-p block-name f)) body)
                ;; Also check macro-expanded forms: a local macro call like (def 10)
                ;; might expand to contain (return-from block-name ...).
                (some (lambda (f)
                        (and (consp f)
                             (symbolp (car f))
                             (gethash (car f) *macros*)
                             (handler-case
                                 (let ((expanded (cached-macroexpand f (gethash (car f) *macros*))))
                                   (and (not (equal expanded f))
                                        (form-has-return-from-p block-name expanded)))
                               (error () t))))  ; on expansion error, be conservative
                      body)))
           ;; Check for free variables from original body (block wrapper doesn't add free vars)
           (free-vars (find-free-vars-with-defaults params body))
           ;; Use direct params for simple required-only functions with no return-from
           (use-direct (and (null free-vars)
                            (simple-required-only-p params)
                            (not has-literal-return-from)))
           ;; For use-direct path: skip block wrapper (preserves TCO, literal body has no return-from).
           ;; For standard path: always wrap to handle return-from inside macro expansions.
           (wrapped-body (if use-direct
                             body
                             `((block ,block-name ,@body))))
           (pkg-spec (defun-pkg-spec name)))
      ;; For uninterned symbols (gensyms), emit extra code to set .Function
      ;; on the actual symbol object after defmethod registers the function.
      ;; Also handle (setf gensym): set SetfFunction on the uninterned gensym (D700).
      (let ((uninterned-fixup
              (cond
                ;; Regular gensym: set .Function on the original symbol
                ((and (symbolp name) (null (symbol-package name)))
                 `((:load-const ,name)
                   (:castclass "Symbol")
                   (:ldstr ,(mangle-name name))
                   (:call "CilAssembler.GetFunction")
                   (:castclass "LispFunction")
                   (:call "CilAssembler.RegisterFunctionOnSymbol")))
                ;; (setf gensym): set .SetfFunction on the uninterned gensym target
                ((and (consp name) (eq (car name) 'setf)
                      (symbolp (cadr name)) (null (symbol-package (cadr name))))
                 `((:load-const ,(cadr name))
                   (:castclass "Symbol")
                   (:ldstr ,(mangle-name name))
                   (:call "CilAssembler.GetFunction")
                   (:castclass "LispFunction")
                   (:call "CilAssembler.RegisterSetfFunctionOnSymbol"))))))
        (let ((*tco-self-symbol* (if (symbolp name) name nil)))
          (cond
            ;; Closure defun: free vars captured from enclosing lexical environment.
            ;; Register on the package-qualified symbol (compile-sym-lookup) so that
            ;; (funcall sym) finds the function without a cross-package search.
            ;; Non-symbol names (setf) fall back to the string-based path.
            (free-vars
             (if (symbolp name)
                 `(,@(compile-sym-lookup name)
                   ,@(compile-lambda params wrapped-body)
                   (:castclass "LispFunction")
                   (:call "CilAssembler.RegisterFunctionOnSymbol")
                   ,@uninterned-fixup
                   ,@(compile-sym-lookup name))
                 `((:ldstr ,(mangle-name name))
                   ,@(compile-lambda params wrapped-body)
                   (:castclass "LispFunction")
                   (:call "CilAssembler.RegisterFunction")
                   ,@uninterned-fixup
                   (:ldstr ,(mangle-name name)) (:call "Startup.Sym"))))
            ;; Direct params: simple required-only functions
            (use-direct
             (let* ((mangled (mangle-name name))
                    (pkg-name (cadr pkg-spec))
                    ;; Native eligibility: all fixnum params, fixnum return, ≤4 params,
                    ;; no special-declared params (#130)
                    (native-eligible
                      (and (symbolp name)
                           (<= (length required) 4)
                           (all-params-fixnum-p params wrapped-body)
                           (eq 'fixnum (gethash name *function-return-types*))
                           (null (fn-body-special-params wrapped-body
                                                         (mapcar #'symbol-name required)))
                           (null (remove-if-not #'global-special-p required)))))
               `(,(if native-eligible
                      `(:defmethod-native ,mangled
                         ,@pkg-spec
                         :params ,param-names
                         :body ,(compile-function-body-direct
                                 params wrapped-body mangled pkg-name name))
                      `(:defmethod-direct ,mangled
                         ,@pkg-spec
                         :params ,param-names
                         :body ,(compile-function-body-direct
                                 params wrapped-body mangled pkg-name name)))
                 ,@uninterned-fixup
                 ,@(if (symbolp name)
                       (compile-sym-lookup name)
                       `((:ldstr ,mangled) (:call "Startup.Sym"))))))
            ;; Standard defmethod
            (t
              `((:defmethod ,(mangle-name name)
                 ,@pkg-spec
                 :params ,param-names
                 :body ,(compile-function-body params wrapped-body (mangle-name name)))
                ,@uninterned-fixup
                ,@(if (symbolp name)
                      (compile-sym-lookup name)
                      `((:ldstr ,(mangle-name name)) (:call "Startup.Sym")))))))))))

(defun compile-defmacro (name lambda-list body)
  "Compile (defmacro name lambda-list body...).
   Uses SBCL's eval to create macro function at compile time.
   At runtime, just returns the macro name as a symbol."
  ;; Package lock check (#93): only fire when the macro isn't already defined.
  ;; If *macros* already has this key (e.g. bootstrap CL macros like DEFMETHOD),
  ;; the registration below is a no-op — checking the lock would cause false errors
  ;; for guard patterns like (unless (fboundp 'defmethod) (defmacro defmethod ...)).
  (when (and (not *cross-compiling*) (symbolp name)
             (not (gethash (macro-key-for-symbol name) *macros*)))
    (%check-package-lock name "DEFMACRO"))
  (let* ((whole-sym (gensym "WHOLE"))
         ;; If lambda-list starts with &whole var, bind var to whole form
         (whole-var (when (and (consp lambda-list) (eq (car lambda-list) '&whole))
                      (cadr lambda-list)))
         (rest-ll (if whole-var (cddr lambda-list) lambda-list))
         ;; Strip &environment var from lambda-list (not valid in destructuring-bind)
         (env-var nil)
         (clean-ll (let ((result nil) (ll rest-ll))
                     (loop while ll do
                       (cond ((atom ll)
                              ;; Dotted tail (a b . rest) = (a b &rest rest)
                              (push '&rest result)
                              (push ll result)
                              (setq ll nil))
                             ((eq (car ll) '&environment)
                              (setq env-var (cadr ll))
                              (setq ll (cddr ll)))
                             (t (push (car ll) result)
                                (setq ll (cdr ll)))))
                     (nreverse result)))
         ;; 1-arg (compile-time eval): env-var bound to nil (no env available)
         (wrapped-body-1arg (if env-var
                                `((let ((,env-var nil)) ,@body))
                                body))
         ;; defmacro creates an implicit block named after the macro (1-arg form)
         (block-wrapped-body
           (if (some (lambda (f) (form-has-return-from-p name f)) wrapped-body-1arg)
               `((block ,name ,@wrapped-body-1arg))
               wrapped-body-1arg))
         ;; expander-form for compile-time eval (1-arg: form only)
         (expander-form-1arg
           (if whole-var
               `(lambda (,whole-sym)
                  (let ((,whole-var ,whole-sym))
                    (destructuring-bind ,clean-ll (cdr ,whole-sym)
                      ,@block-wrapped-body)))
               `(lambda (,whole-sym)
                  (destructuring-bind ,clean-ll (cdr ,whole-sym)
                    ,@block-wrapped-body))))
         ;; 2-arg (runtime): env-var bound to the actual env argument (CLHS 3.4.4)
         (env-sym (gensym "ENV"))
         (wrapped-body-2arg (if env-var
                                `((let ((,env-var ,env-sym)) ,@body))
                                body))
         (block-wrapped-body-2arg
           (if (some (lambda (f) (form-has-return-from-p name f)) wrapped-body-2arg)
               `((block ,name ,@wrapped-body-2arg))
               wrapped-body-2arg))
         (expander-form-2arg
           (if whole-var
               `(lambda (,whole-sym ,env-sym)
                  ,@(unless env-var `((declare (ignore ,env-sym))))
                  (let ((,whole-var ,whole-sym))
                    (destructuring-bind ,clean-ll (cdr ,whole-sym)
                      ,@block-wrapped-body-2arg)))
               `(lambda (,whole-sym ,env-sym)
                  ,@(unless env-var `((declare (ignore ,env-sym))))
                  (destructuring-bind ,clean-ll (cdr ,whole-sym)
                    ,@block-wrapped-body-2arg))))
         ;; When inside a flet scope, wrap the eval form with the flet definitions
         ;; so that flet-local functions (e.g. PROGNIFY in SBCL's macros.lisp) are
         ;; available during compile-time macro registration.
         (eval-form (if *compile-time-flet-defs*
                        `(flet ,*compile-time-flet-defs*
                           ,expander-form-1arg)
                        expander-form-1arg))
         (expander-fn (handler-case (eval eval-form)
                        (error () nil))))
    ;; Only register if eval succeeded and no existing entry
    (when expander-fn
      (let ((mkey (macro-key-for-symbol name)))
        (unless (gethash mkey *macros*)
          (setf (gethash mkey *macros*) expander-fn)))
      ;; In cross-compilation context (dotcl as SBCL XC host), also update
      ;; SBCL's info db so ir1-convert-global-functoid recognizes this as a macro.
      ;; Without this, SBCL XC treats macros defined by dotcl's defmacro as functions.
      ;; Gated by #+sbcl so dotcl's reader doesn't try to intern SB-C symbols.
      #+sbcl
      (when (and *cross-compiling* (symbolp name))
        (ignore-errors
          (let ((fn2 (let ((fn expander-fn))
                       (lambda (form env) (declare (ignore env)) (funcall fn form)))))
            (setf (sb-c::info :function :kind name) :macro)
            (setf (sb-c::info :function :macro-function name) fn2)))))
    ;; Runtime: compile the expander lambda and register as macro function,
    ;; then return macro name symbol.
    ;; This ensures FASL loads also register the macro.
    (compile-expr `(progn (%register-macro-function-rt ',name ,expander-form-2arg)
                          ',name))))

(defun try-eval (form)
  "Eval FORM, ignoring errors. Used during compile-file to establish
   compile-time values for defvar/defparameter/defconstant.
   Note: callers usually hit Runtime.TryEval instead (cil-compiler.lisp
   has a (string= op \"TRY-EVAL\") shortcut that emits a direct call to
   the C# entry); the C# side is responsible for binding
   *compile-file-mode* to NIL during the inner eval. This Lisp
   definition is the fallback for non-shortcut callers."
  (handler-case (eval form)
    (error (c)
      (warn "compile-time eval failed: ~A" c)
      nil)))

(defun compile-eval-when (situations body)
  "Compile (eval-when (situations...) body...).
   During cross-compilation: :compile-toplevel/:execute → eval in SBCL.
   During compile-file: :compile-toplevel → eval at compile time (CLHS 3.2.3.1),
     :load-toplevel → emit CIL for load.
   During load/eval: only :execute → emit CIL.
   When :compile-toplevel at runtime, eval each form individually first
   (so defvar values are available for subsequent macro expansion)."
  (let ((ct-p (or (member :compile-toplevel situations)
                  (member 'compile-toplevel situations)
                  (member 'compile situations)))
        (lt-p (or (member :load-toplevel situations)
                  (member 'load-toplevel situations)
                  (member 'load situations)))
        (ex-p (or (member :execute situations)
                  (member 'execute situations)
                  (member 'eval situations))))
    ;; Compile-time evaluation at top level.
    ;; Cross-compile: SBCL eval (all forms).
    ;; Compile-file: eval all :compile-toplevel forms per CLHS 3.2.3.1
    ;;   (defmacro, defvar, etc. must take effect before subsequent forms).
    ;; Load/eval: best-effort try-eval of defvar/defparameter only.
    (when *at-toplevel*
      (cond
        (*cross-compiling*
         (when (or ct-p ex-p)
           (dolist (form body)
             (eval form))))
        (*compile-file-mode*
         (when ct-p
           (dolist (form body)
             (eval form))))
        (t
         (when (or ct-p ex-p)
           (dolist (form body)
             (when (and (consp form)
                        (symbolp (car form))
                        (member (symbol-name (car form))
                                '("DEFVAR" "DEFPARAMETER" "DEFCONSTANT"
                                  "DEFTYPE" "DEFSTRUCT" "DEFCLASS"
                                  "DEFINE-CONDITION" "DEFMACRO")
                                :test #'string=))
               (try-eval form)))))))
    ;; Emit CIL for runtime/load.
    ;; Per CLHS 3.2.3.1:
    ;; - Cross-compiling or compile-file at top level: :load-toplevel/:execute → emit CIL
    ;; - Load/eval (not cross-compiling, not compile-file): only :execute → emit CIL
    ;; - Not at top level: only :execute → emit body as implicit progn
    (if (if (or *cross-compiling* *compile-file-mode*)
            (if *at-toplevel*
                (or lt-p ex-p)
                ex-p)
            ex-p)
        (compile-progn body)
        (emit-nil))))

(defun fn-body-special-params (body all-param-names)
  "Find function parameters declared special in the function body.
   Handles body possibly wrapped in (block name ...).
   ALL-PARAM-NAMES is a list of symbol-name strings."
  ;; Unwrap optional (block name ...) or (let* bindings ...) wrapper to find leading declares
  (let ((inner-forms
          (cond ((and (= (length body) 1)
                      (consp (car body))
                      (eq (caar body) 'block))
                 (cddar body))
                ((and (= (length body) 1)
                      (consp (car body))
                      (eq (caar body) 'let*))
                 (cddar body))  ;; skip let* and bindings: (cdr (cdr (car body)))
                (t body))))
    (multiple-value-bind (specials _rest) (extract-specials inner-forms)
      (declare (ignore _rest))
      (remove-if-not (lambda (s)
                       (member (symbol-name s) all-param-names :test #'string=))
                     specials))))

(defun compile-function-body (params body &optional (fn-name ""))
  "Compile a function body. Params are bound from args array (arg 0).
   Handles &rest/&optional/&key parameters."
  (merge-disjoint-locals (compile-function-body-inner params body 0 fn-name)))

(defun simple-required-only-p (params)
  "Return T if params is a simple required-only lambda list with <= 8 params."
  (multiple-value-bind (required optional key rest-param aux allow-other-keys-p has-key-p) (parse-lambda-list params)
    (declare (ignore allow-other-keys-p))
    (and (<= (length required) 8)
         (null optional) (null key) (not has-key-p) (null rest-param) (null aux))))

(defun all-params-fixnum-p (params body)
  "Return T if PARAMS is all required fixnum-declared args with no optional/key/rest.
   Used to determine native-eligibility for #130 native self-call path."
  (multiple-value-bind (required optional key rest-param) (parse-lambda-list params)
    (and (null optional) (null key) (null rest-param)
         (not (null required))
         (let ((fxlocals (extract-fixnum-locals body)))
           (every (lambda (p) (member (symbol-name p) fxlocals :test #'string=))
                  required)))))

(defun compile-function-body-direct (params body &optional (fn-name "") fn-pkg fn-symbol)
  "Compile function body with direct parameter passing (no args array).
   Only for functions with exactly required params, no optional/key/rest.
   Params are accessed via (:ldarg 0), (:ldarg 1), ... directly.
   FN-PKG, if given, is the defining package name — used by the self-call
   symbol-lookup cache.
   FN-SYMBOL, if given, is the defun symbol — used for native eligibility check (#130)."
  (multiple-value-bind (required optional key rest-param aux) (parse-lambda-list params)
    (declare (ignore optional key rest-param aux))
    (let* ((all-params required)
           (*locals* '())
           (*boxed-vars* '())
           (*block-tags* '())
           (*go-tags* '())
           (*local-functions* '())
           (local-keys (mapcar (lambda (p) (cons p (gen-local (symbol-name p)))) all-params))
           (mutated (find-mutated-vars body))
           (captured (find-captured-vars body (mapcar #'symbol-name all-params)))
           (needs-boxing (intersection mutated captured :test #'string=))
           ;; Pre-check native eligibility: all fixnum params, fixnum return, no captures
           ;; Full check (including no specials) happens after special-param-syms is computed,
           ;; but we need this early for param-instrs type selection (#130).
           (pre-special-syms
             (when (and fn-symbol (null needs-boxing) (all-params-fixnum-p params body))
               (union (fn-body-special-params body (mapcar #'symbol-name all-params))
                      (remove-if-not #'global-special-p all-params))))
           (pre-native-eligible
             (and fn-symbol
                  (null needs-boxing)
                  (all-params-fixnum-p params body)
                  (null pre-special-syms)
                  (eq 'fixnum (gethash fn-symbol *function-return-types*)))))
      (let ((*locals* local-keys)
            (*boxed-vars* (mapcar (lambda (name) (find name all-params :key #'symbol-name :test #'string=))
                                  needs-boxing)))
        (let ((param-instrs
                (if pre-native-eligible
                    ;; Native body: params come in as long, store into Int64 locals (#130)
                    (loop for p in required
                          for key = (cdr (assoc p local-keys))
                          for i from 0
                          append `((:declare-local ,key "Int64")
                                   (:ldarg ,i) (:stloc ,key)))
                    ;; Normal body: params as LispObject (with boxed-var support)
                    (loop for p in required
                          for key = (cdr (assoc p local-keys))
                          for i from 0
                          if (boxed-var-p p)
                            append `((:declare-local ,key "LispObject[]")
                                     (:ldc-i4 1) (:newarr "LispObject") (:dup)
                                     (:ldc-i4 0) (:ldarg ,i)
                                     (:stelem-ref) (:stloc ,key))
                          else
                            append `((:declare-local ,key "LispObject")
                                     (:ldarg ,i) (:stloc ,key))))))
          (let* ((special-param-syms
                   (union (fn-body-special-params body (mapcar #'symbol-name all-params))
                          (remove-if-not #'global-special-p all-params)))
                 (sp-names (mapcar #'symbol-name special-param-syms))
                 (special-push-instrs
                   (loop for p in special-param-syms
                         for pkey = (cdr (assoc p local-keys))
                         append `(,@(compile-sym-lookup p)
                                  (:castclass "Symbol")
                                  (:ldloc ,pkey)
                                  (:call "DynamicBindings.Push"))))
                 (*locals* (remove-if (lambda (entry)
                                        (member (let ((k (car entry)))
                                                  (if (symbolp k) (symbol-name k) ""))
                                                sp-names :test #'string=))
                                      *locals*)))
            ;; TCO scope: enabled for named functions without special params.
            ;; Special params require a try/finally block (DynamicBindings.Pop) and
            ;; CIL's plain Br can't escape it (needs Leave). Boxed params are fine —
            ;; compile-named-call's TCO branch writes through the box via stelem-ref
            ;; (D682 relaxed the prior (null needs-boxing) restriction; *tco-param-entries*
            ;; now carries per-param boxed-p so the self-call code chooses the right path).
            ;; Always reset TCO state so inner lambdas don't inherit outer TCO context.
            (let* ((use-tco (and (string/= fn-name "")
                                 (null special-param-syms)))
                   (tco-loop-label (when use-tco (gen-label "TCOLOOP")))
                   (*tco-self-name* (if use-tco fn-name nil))
                   (*tco-loop-label* (if use-tco tco-loop-label nil))
                   ;; Reset mutual-TCO: closures compiled within labels group must not
                   ;; emit br-to-outer-TCOLOOP (#124/D919).
                   (*labels-mutual-tco* nil)
                   ;; Labels functions are stored in boxes, not symbols — skip self-fn caching (#125)
                   (*self-fn-local* (when (and use-tco (null *tco-local-fn-key*)) (gen-local "SELF-FN")))
                   ;; Self-fn caching: for (SETF NAME) functions, look up SetfFunction
                   ;; on the target NAME symbol rather than Function on "(SETF NAME)"
                   ;; (D698: fix broken GetFunctionBySymbol call for setf functions).
                   (setf-fn-p (and *self-fn-local*
                                   (> (length fn-name) 7)
                                   (string= fn-name "(SETF " :end1 6)))
                   (setf-target-name (when setf-fn-p
                                       (subseq fn-name 6 (1- (length fn-name)))))
                   (self-fn-prelude
                     (when *self-fn-local*
                       (if setf-fn-p
                           ;; (SETF NAME): look up SetfFunction on the target symbol
                           `((:declare-local ,*self-fn-local* "LispFunction")
                             ,@(if fn-pkg
                                   `((:load-sym-pkg ,setf-target-name ,fn-pkg))
                                   `((:load-sym ,setf-target-name)))
                             (:castclass "Symbol")
                             (:call "CilAssembler.GetSetfFunctionBySymbol")
                             (:stloc ,*self-fn-local*))
                           ;; Normal function: look up Function on the symbol
                           `((:declare-local ,*self-fn-local* "LispFunction")
                             ,@(if fn-pkg
                                   `((:load-sym-pkg ,fn-name ,fn-pkg))
                                   `((:load-sym ,fn-name)))
                             (:castclass "Symbol")
                             (:call "CilAssembler.GetFunctionBySymbol")
                             (:stloc ,*self-fn-local*)))))
                   (*tco-param-entries*
                     (if use-tco
                         (loop for p in required
                               for key = (cdr (assoc p local-keys))
                               collect (cons key (boxed-var-p p)))
                         nil))
                   ;; Function body last form is in tail position:
                   ;; - TCO rewrite applies only when *tco-self-name* is set
                   ;; - MV return propagation (D638): tail form doesn't unwrap MvReturn
                   (*in-tail-position* t)
                   ;; Fixnum type declarations on params — consulted by fixnum-typed-p
                   ;; and compile-as-long for native int64 paths (D669).
                   (*fixnum-locals* (append (extract-fixnum-locals body) *fixnum-locals*))
                   ;; Double-float type declarations on params (D672).
                   (*double-float-locals* (append (extract-double-float-locals body)
                                                  *double-float-locals*))
                   (*single-float-locals* (append (extract-single-float-locals body)
                                                  *single-float-locals*))
                   ;; Native body: params are Int64, enabling compile-as-long without
                   ;; unbox and native self-calls via InvokeNativeN (#130).
                   (*long-locals* (if (and pre-native-eligible use-tco)
                                      (mapcar #'symbol-name all-params)
                                      *long-locals*))
                   (*native-self-name* (if (and pre-native-eligible use-tco)
                                           fn-name
                                           *native-self-name*))
                   ;; If we'll wrap in try/finally for DynamicBindings.Pop, body
                   ;; compilation must know — so tail-position calls don't emit
                   ;; the `.tail` prefix (illegal in CIL inside try) and TCO
                   ;; branches don't try to cross the try boundary (D683).
                   (*in-try-block* (or *in-try-block* (not (null special-param-syms))))
                   (body-instrs (compile-progn body)))
              (merge-disjoint-locals
               (if special-param-syms
                   `(,@param-instrs
                     ,@self-fn-prelude
                     ,@(when use-tco `((:label ,tco-loop-label)))
                     ,@(compile-let-with-specials '() special-push-instrs body-instrs special-param-syms)
                     (:ret))
                   `(,@param-instrs
                     ,@self-fn-prelude
                     ,@(when use-tco `((:label ,tco-loop-label)))
                     ,@(maybe-tail-callvirt body-instrs)
                     (:ret)))))))))))

(defun compile-function-body-inner (params body args-arg-idx &optional (fn-name ""))
  "Compile function body, loading args from (:ldarg ARGS-ARG-IDX).
   For normal functions args-arg-idx=0, for closures args-arg-idx=1."
  (multiple-value-bind (required optional key rest-param aux allow-other-keys-p has-key-p) (parse-lambda-list params)
    (let ((body (if aux `((let* ,aux ,@body)) body)))
    (let* ((key-supplied-p-vars (remove nil (mapcar #'fourth key)))
           (opt-supplied-p-vars (remove nil (mapcar #'third optional)))
           (all-params (append required
                               (mapcar #'car optional)
                               opt-supplied-p-vars
                               (mapcar #'second key)
                               key-supplied-p-vars
                               (if rest-param (list rest-param) nil)))
           (*locals* '())
           (*boxed-vars* '())
           (*block-tags* '())
           (*go-tags* '())
           (*local-functions* '())
           ;; Reset TCO state: inner function bodies must not inherit outer TCO context
           (*tco-self-name* nil)
           (*tco-loop-label* nil)
           (*tco-param-entries* nil)
           (*tco-leave-instrs* nil)
           (*tco-in-try-catch* nil)
           (*self-fn-local* nil)
           (*in-tail-position* nil)
           ;; Reset mutual-TCO: closures within labels group must not emit br-to-outer-TCOLOOP
           (*labels-mutual-tco* nil)
           ;; Reset native state: inner lambdas don't inherit outer native context (#130)
           (*long-locals* nil)
           (*native-self-name* nil)
           (local-keys (mapcar (lambda (p) (cons p (gen-local (symbol-name p)))) all-params))
           ;; Pre-scan for mutable captures
           (mutated (find-mutated-vars body))
           (captured (find-captured-vars body (mapcar #'symbol-name all-params)))
           (needs-boxing (intersection mutated captured :test #'string=))
           (n-required (length required))
           (key-start (+ n-required (length optional))))
      (let ((*locals* local-keys)
            (*boxed-vars* (mapcar (lambda (name) (find name all-params :key #'symbol-name :test #'string=))
                                  needs-boxing)))
        ;; Generate parameter binding instructions
        (let ((param-instrs
                (append
                 ;; Required params: load from args[i]
                 (loop for p in required
                       for key = (cdr (assoc p local-keys))
                       for i from 0
                       if (boxed-var-p p)
                         append `((:declare-local ,key "LispObject[]")
                                  (:ldc-i4 1) (:newarr "LispObject") (:dup)
                                  (:ldc-i4 0) (:ldarg ,args-arg-idx) (:ldc-i4 ,i) (:ldelem-ref)
                                  (:stelem-ref) (:stloc ,key))
                       else
                         append `((:declare-local ,key "LispObject")
                                  (:ldarg ,args-arg-idx) (:ldc-i4 ,i) (:ldelem-ref)
                                  (:stloc ,key)))
                 ;; Optional params: check args.Length (with boxing & supplied-p support)
                 (let ((opt-instrs nil)
                       (remaining-opt-names (mapcar #'car optional)))
                   (loop for (opt-name opt-default sp-var) in optional
                         for i from n-required
                         for key = (cdr (assoc opt-name local-keys))
                         do ;; Remove current param from remaining list (it's being initialized now)
                            ;; so default-form compilation doesn't see it as a local
                            (let* ((*locals* (remove-if (lambda (entry)
                                                          (member (symbol-name (car entry))
                                                                  (mapcar #'symbol-name remaining-opt-names)
                                                                  :test #'string=))
                                                        *locals*))
                                   (default-label (gen-label "OPTDEF"))
                                   (done-label (gen-label "OPTDONE")))
                              (setq opt-instrs
                                    (append opt-instrs
                                            (if (boxed-var-p opt-name)
                                                (let ((tmp (gen-local "OPTTMP")))
                                                  `((:declare-local ,tmp "LispObject")
                                                    (:ldarg ,args-arg-idx) (:ldlen) (:conv-i4)
                                                    (:ldc-i4 ,(1+ i))
                                                    (:blt ,default-label)
                                                    (:ldarg ,args-arg-idx) (:ldc-i4 ,i) (:ldelem-ref)
                                                    (:br ,done-label)
                                                    (:label ,default-label)
                                                    ,@(if opt-default
                                                          (compile-expr opt-default)
                                                          (emit-nil))
                                                    (:label ,done-label)
                                                    (:stloc ,tmp)
                                                    (:declare-local ,key "LispObject[]")
                                                    (:ldc-i4 1) (:newarr "LispObject") (:dup)
                                                    (:ldc-i4 0) (:ldloc ,tmp) (:stelem-ref)
                                                    (:stloc ,key)))
                                                `((:declare-local ,key "LispObject")
                                                  (:ldarg ,args-arg-idx) (:ldlen) (:conv-i4)
                                                  (:ldc-i4 ,(1+ i))
                                                  (:blt ,default-label)
                                                  (:ldarg ,args-arg-idx) (:ldc-i4 ,i) (:ldelem-ref)
                                                  (:br ,done-label)
                                                  (:label ,default-label)
                                                  ,@(if opt-default
                                                        (compile-expr opt-default)
                                                        (emit-nil))
                                                  (:label ,done-label)
                                                  (:stloc ,key)))))
                            ;; After initializing, this param is now visible to subsequent defaults
                            (pop remaining-opt-names)
                            ;; supplied-p variable right after its optional param
                            (when sp-var
                                (let ((sp-key (cdr (assoc sp-var local-keys)))
                                      (sp-found-label (gen-label "OPTSPF"))
                                      (sp-done-label (gen-label "OPTSPD")))
                                  (setq opt-instrs
                                        (append opt-instrs
                                                `((:declare-local ,sp-key "LispObject")
                                                  (:ldarg ,args-arg-idx) (:ldlen) (:conv-i4)
                                                  (:ldc-i4 ,(1+ i))
                                                  (:blt ,sp-found-label)
                                                  ,@(emit-t)
                                                  (:br ,sp-done-label)
                                                  (:label ,sp-found-label)
                                                  ,@(emit-nil)
                                                  (:label ,sp-done-label)
                                                  (:stloc ,sp-key))))))))
                   opt-instrs)
                 ;; &key params: search from key-start (with boxing support)
                 ;; supplied-p variable is emitted right after its key param
                 ;; (CL requires left-to-right init, so later defaults can reference earlier supplied-p)
                 (let ((key-instrs nil)
                       (remaining-key-vars (mapcar #'second key)))
                   (dolist (key-spec key)
                     (let* ((key-name (first key-spec))
                            (var-name (second key-spec))
                            (key-default (third key-spec))
                            (sp-var (fourth key-spec))
                            (explicit-key-pkg (fifth key-spec))
                            (find-key-fn (if explicit-key-pkg "Runtime.FindKeyArgByName" "Runtime.FindKeyArg"))
                            (key (cdr (assoc var-name local-keys)))
                            (found-label (gen-label "KEYFOUND"))
                            (done-label (gen-label "KEYDONE")))
                       ;; Key param binding
                       ;; Per CL spec, the init-form is evaluated in an environment where
                       ;; the current parameter and all not-yet-initialized key params
                       ;; are not yet bound. Remove them from *locals* so that if the
                       ;; default references a later key param's name as a special variable,
                       ;; it reads the dynamic binding rather than an undeclared local.
                       (let ((default-instrs
                               (if key-default
                                   (let ((*locals* (remove-if (lambda (entry)
                                                                (member (symbol-name (car entry))
                                                                        (mapcar #'symbol-name remaining-key-vars)
                                                                        :test #'string=))
                                                              *locals*)))
                                     (compile-expr key-default))
                                   (emit-nil))))
                         (setq key-instrs
                               (append key-instrs
                                       (if (boxed-var-p var-name)
                                           (let ((tmp (gen-local "KEYTMP")))
                                             `((:declare-local ,tmp "LispObject")
                                               (:ldarg ,args-arg-idx) (:ldc-i4 ,key-start)
                                               (:ldstr ,key-name)
                                               ,@(when explicit-key-pkg `((:ldstr ,explicit-key-pkg)))
                                               (:call ,find-key-fn)
                                               (:dup) (:brtrue ,found-label)
                                               (:pop)
                                               ,@default-instrs
                                               (:br ,done-label)
                                               (:label ,found-label)
                                               (:label ,done-label)
                                               (:stloc ,tmp)
                                               (:declare-local ,key "LispObject[]")
                                               (:ldc-i4 1) (:newarr "LispObject") (:dup)
                                               (:ldc-i4 0) (:ldloc ,tmp) (:stelem-ref)
                                               (:stloc ,key)))
                                           `((:declare-local ,key "LispObject")
                                             (:ldarg ,args-arg-idx) (:ldc-i4 ,key-start)
                                             (:ldstr ,key-name)
                                             ,@(when explicit-key-pkg `((:ldstr ,explicit-key-pkg)))
                                             (:call ,find-key-fn)
                                             (:dup) (:brtrue ,found-label)
                                             (:pop)
                                             ,@default-instrs
                                             (:br ,done-label)
                                             (:label ,found-label)
                                             (:label ,done-label)
                                             (:stloc ,key))))))
                       ;; supplied-p variable right after its key param
                       (when sp-var
                         (let ((sp-key (cdr (assoc sp-var local-keys)))
                               (sp-found-label (gen-label "SPFOUND"))
                               (sp-done-label (gen-label "SPDONE")))
                           (setq key-instrs
                                 (append key-instrs
                                         `((:declare-local ,sp-key "LispObject")
                                           (:ldarg ,args-arg-idx) (:ldc-i4 ,key-start)
                                           (:ldstr ,key-name)
                                           ,@(when explicit-key-pkg `((:ldstr ,explicit-key-pkg)))
                                           (:call ,find-key-fn)
                                           (:brtrue ,sp-found-label)
                                           ,@(emit-nil)
                                           (:br ,sp-done-label)
                                           (:label ,sp-found-label)
                                           ,@(emit-t)
                                           (:label ,sp-done-label)
                                           (:stloc ,sp-key)))))))
                     ;; After initializing, this key param is now visible to subsequent defaults
                     (pop remaining-key-vars))
                   key-instrs)
                 ;; Rest param: collect args[N..] (after required+optional, before &key)
                 (when rest-param
                   (let ((key (cdr (assoc rest-param local-keys)))
                         (n key-start))
                     (if (boxed-var-p rest-param)
                         `((:declare-local ,key "LispObject[]")
                           (:ldc-i4 1) (:newarr "LispObject") (:dup)
                           (:ldc-i4 0) (:ldarg ,args-arg-idx) (:ldc-i4 ,n) (:call "Runtime.CollectRestArgs")
                           (:stelem-ref) (:stloc ,key))
                         `((:declare-local ,key "LispObject")
                           (:ldarg ,args-arg-idx) (:ldc-i4 ,n) (:call "Runtime.CollectRestArgs")
                           (:stloc ,key))))))))
          (let* ((arity-instrs
                  (cond
                    ((and (null optional) (null key) (null rest-param) (not has-key-p))
                     ;; Required-only: exact arity check
                     `((:ldstr ,fn-name) (:ldarg ,args-arg-idx) (:ldc-i4 ,n-required)
                       (:call "Runtime.CheckArityExact")))
                    ((and optional (null key) (null rest-param) (not has-key-p))
                     ;; Optional-only: min + max check
                     (let ((n-max (+ n-required (length optional))))
                       `((:ldstr ,fn-name) (:ldarg ,args-arg-idx) (:ldc-i4 ,n-required)
                         (:call "Runtime.CheckArityMin")
                         (:ldstr ,fn-name) (:ldarg ,args-arg-idx) (:ldc-i4 ,n-max)
                         (:call "Runtime.CheckArityMax"))))
                    (t
                     ;; Has key/rest: minimum arity check only
                     `((:ldstr ,fn-name) (:ldarg ,args-arg-idx) (:ldc-i4 ,n-required)
                       (:call "Runtime.CheckArityMin")))))
                (key-check-instrs
                  ;; If function has &key params and no &allow-other-keys, check for unknown keys
                  (when (and has-key-p (not allow-other-keys-p))
                    (let* ((n-keys (length key))
                           (has-explicit (some #'fifth key)))
                      `((:ldstr ,fn-name)
                        (:ldarg ,args-arg-idx) (:ldc-i4 ,key-start)
                        (:ldc-i4 ,n-keys) (:newarr "String")
                        ,@(loop for i from 0
                                for key-spec in key
                                append `((:dup) (:ldc-i4 ,i) (:ldstr ,(first key-spec)) (:stelem-ref)))
                        ,@(if has-explicit
                              ;; Pass package array for explicit key matching
                              `((:ldc-i4 ,n-keys) (:newarr "String")
                                ,@(loop for i from 0
                                        for key-spec in key
                                        append (let ((pkg (fifth key-spec)))
                                                 (if pkg
                                                     `((:dup) (:ldc-i4 ,i) (:ldstr ,pkg) (:stelem-ref))
                                                     `())))
                                (:call "Runtime.CheckNoUnknownKeys2"))
                              `((:call "Runtime.CheckNoUnknownKeys"))))))))
           ;; Handle special params: both (declare (special param)) and
           ;; globally special params (defvar/*name* convention) bind dynamically
           (let* ((special-param-syms
                    (union (fn-body-special-params body (mapcar #'symbol-name all-params))
                           (remove-if-not #'global-special-p all-params)))
                  (sp-names (mapcar #'symbol-name special-param-syms))
                  (special-push-instrs
                    (loop for p in special-param-syms
                          for pkey = (cdr (assoc p local-keys))
                          append `(,@(compile-sym-lookup p)
                                   (:castclass "Symbol")
                                   (:ldloc ,pkey)
                                   (:call "DynamicBindings.Push"))))
                  (*locals* (remove-if (lambda (entry)
                                         (member (let ((k (car entry)))
                                                   (if (symbolp k) (symbol-name k) ""))
                                                 sp-names :test #'string=))
                                       *locals*))
                  ;; D638: tail position preserves MvReturn for multi-value callers
                  (*in-tail-position* t)
                  (body-instrs (compile-progn body)))
             (merge-disjoint-locals
              (if special-param-syms
                  `(,@arity-instrs
                    ,@key-check-instrs
                    ,@param-instrs
                    ,@(compile-let-with-specials '() special-push-instrs body-instrs special-param-syms)
                    (:ret))
                  `(,@arity-instrs
                    ,@key-check-instrs
                    ,@param-instrs
                    ,@body-instrs
                    (:ret))))))))))))

;;; ============================================================
;;; let / let*
;;; ============================================================

(defun extract-specials (body)
  "Extract (declare (special ...)) from the beginning of body.
   Strips ALL declarations (ignore, type, etc.) from remaining-body.
   Returns (values specials-list remaining-body)."
  (let ((specials '())
        (rest-body body))
    (loop while (and rest-body
                     (consp (car rest-body))
                     (eq (caar rest-body) 'declare))
          do (dolist (decl (cdar rest-body))
               (when (and (consp decl) (eq (car decl) 'special))
                 (dolist (s (cdr decl))
                   (push s specials))))
             (pop rest-body))
    (values specials rest-body)))

(defun extract-fixnum-locals (body)
  "Scan (declare ...) forms at the head of BODY for fixnum type hints
   on locals. Recognizes:
     (declare (fixnum x y z))
     (declare (type fixnum x y))
   NOTE: `(type (integer LO HI) x)` is NOT treated as fixnum even when
   bounds fit — multiplication on such a var could produce a result outside
   fixnum range that we'd silently wrap (CL mandates bignum promotion).
   Users who want the optimization must declare `fixnum` explicitly.
   Returns a list of symbol-names (strings) declared fixnum."
  (let ((result '()))
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare))
        (return))
      (dolist (decl (cdr form))
        (when (consp decl)
          (cond
            ;; (fixnum x y ...)
            ((eq (car decl) 'fixnum)
             (dolist (v (cdr decl))
               (when (symbolp v) (pushnew (symbol-name v) result :test #'string=))))
            ;; (type fixnum x y ...)
            ((and (eq (car decl) 'type) (eq (cadr decl) 'fixnum))
             (dolist (v (cddr decl))
               (when (symbolp v) (pushnew (symbol-name v) result :test #'string=))))))))
    result))

(defun extract-double-float-locals (body)
  "Parallel to extract-fixnum-locals for double-float."
  (let ((result '()))
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare))
        (return))
      (dolist (decl (cdr form))
        (when (consp decl)
          (cond
            ((eq (car decl) 'double-float)
             (dolist (v (cdr decl))
               (when (symbolp v) (pushnew (symbol-name v) result :test #'string=))))
            ((and (eq (car decl) 'type) (eq (cadr decl) 'double-float))
             (dolist (v (cddr decl))
               (when (symbolp v) (pushnew (symbol-name v) result :test #'string=))))))))
    result))

(defun extract-single-float-locals (body)
  "Parallel to extract-double-float-locals for single-float."
  (let ((result '()))
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare))
        (return))
      (dolist (decl (cdr form))
        (when (consp decl)
          (cond
            ((eq (car decl) 'single-float)
             (dolist (v (cdr decl))
               (when (symbolp v) (pushnew (symbol-name v) result :test #'string=))))
            ((and (eq (car decl) 'type) (eq (cadr decl) 'single-float))
             (dolist (v (cddr decl))
               (when (symbolp v) (pushnew (symbol-name v) result :test #'string=))))))))
    result))

;;; ============================================================
;;; Return type inference (#129)
;;; ============================================================

(defun extract-declared-var-types (body)
  "Return alist of (var-symbol . type) from declare forms at head of BODY.
  Recognizes (fixnum x y), (double-float x), (type fixnum x y)."
  (let ((types '()))
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare)) (return))
      (dolist (decl (cdr form))
        (when (consp decl)
          (let ((dtype
                  (cond
                    ((member (car decl) '(fixnum double-float single-float
                                                 character boolean bit))
                     (car decl))
                    ((and (eq (car decl) 'type) (symbolp (cadr decl)))
                     (cadr decl))
                    (t nil))))
            (when dtype
              (dolist (v (if (eq (car decl) 'type) (cddr decl) (cdr decl)))
                (when (symbolp v) (push (cons v dtype) types))))))))
    types))

(defun infer-expr-return-type (expr var-types self-name)
  "Conservatively infer the return type of EXPR.
  VAR-TYPES is an alist (sym . type). SELF-NAME is the mangled fn name for
  self-recursion detection. Returns 'fixnum, 'double-float, :self-recursive,
  or NIL (unknown). :self-recursive means 'same as the enclosing function',
  handled by meet-inferred-types."
  (cond
    ((integerp expr)
     (when (<= most-negative-fixnum expr most-positive-fixnum) 'fixnum))
    ((symbolp expr)
     (cdr (assoc expr var-types)))
    ((and (consp expr) (eq (car expr) 'the) (symbolp (cadr expr)))
     (cadr expr))
    ((and (consp expr) (eq (car expr) 'progn))
     (when (cdr expr)
       (infer-expr-return-type (car (last expr)) var-types self-name)))
    ((and (consp expr) (eq (car expr) 'if))
     (let* ((then-t (infer-expr-return-type (caddr expr) var-types self-name))
            (else-t (if (cdddr expr)
                        (infer-expr-return-type (cadddr expr) var-types self-name)
                        nil)))
       (meet-inferred-types then-t else-t)))
    ;; Fixnum arithmetic on fixnum-typed operands
    ((and (consp expr) (symbolp (car expr))
          (member (car expr) '(+ - * 1+ 1- ash logand logior logxor min max abs))
          (every (lambda (a)
                   (eq 'fixnum (infer-expr-return-type a var-types self-name)))
                 (cdr expr)))
     'fixnum)
    ;; Self-recursive call
    ((and (consp expr) (symbolp (car expr)) self-name
          (string= (mangle-name (car expr)) self-name))
     :self-recursive)
    (t nil)))

(defun meet-inferred-types (t1 t2)
  "Merge two inferred types. :self-recursive is compatible with any concrete type."
  (cond
    ((equal t1 t2) t1)
    ((eq t1 :self-recursive) t2)
    ((eq t2 :self-recursive) t1)
    (t nil)))

(defun infer-body-return-type (body self-name)
  "Infer the return type of a function with BODY and mangled name SELF-NAME.
  Returns a concrete type symbol (e.g. 'fixnum) or NIL if unknown."
  (let* ((var-types (extract-declared-var-types body))
         (last-form (and body (car (last body)))))
    (when last-form
      (let ((ty (infer-expr-return-type last-form var-types self-name)))
        (when (and ty (not (eq ty :self-recursive)))
          ty)))))

(defun compile-let (bindings body sequential-p)
  "Compile (let bindings body...) or (let* bindings body...)."
  (multiple-value-bind (declared-specials real-body) (extract-specials body)
    (let* ((all-specials (append declared-specials *specials*))
           ;; Parse bindings
           (parsed (mapcar (lambda (b)
                             (if (consp b)
                                 (list (car b) (cadr b))
                                 (list b nil)))
                           bindings))
           ;; Classify into lexical and special
           ;; Only globally special vars (defvar/proclaim) or vars in this let's own declares
           ;; force dynamic binding. Outer locally-declared specials do NOT propagate to bindings.
           (binding-info
             (mapcar (lambda (p)
                       (let* ((var (first p))
                              (init (second p))
                              (is-special (or (member var declared-specials)
                                              (global-special-p var))))
                         (list var init is-special (gen-local (symbol-name var)))))
                     parsed))
           ;; Pre-scan for mutable captures in body (and init forms for let*)
           (var-names (mapcar #'first parsed))
           ;; For let*, lambdas in init forms can capture+mutate earlier vars
           (scan-forms (if sequential-p
                           (append (remove nil (mapcar #'second parsed)) real-body)
                           real-body))
           (mutated (find-mutated-vars scan-forms))
           (captured (find-captured-vars scan-forms (mapcar #'symbol-name var-names)))
           (needs-boxing (intersection mutated captured :test #'string=))
           ;; Separate
           (special-bindings (remove-if-not #'third binding-info))
           (lexical-bindings (remove-if #'third binding-info)))
      ;; Build new locals alist
      (let* ((new-local-entries
               (mapcar (lambda (b)
                         (cons (first b) (fourth b)))
                       lexical-bindings))
             (init-instrs '())
             (bind-instrs '())
             (special-syms '()))
        ;; For let (not let*): evaluate all inits in old scope first
        (if (not sequential-p)
            ;; Let: evaluate all inits, store to temp locals (only when the
            ;; binding routes through a special-var Push or a boxed array).
            ;; Plain lexical, non-boxed bindings store the init result directly
            ;; into the final binding key — no temp needed (D856, #114).
            (let ((temp-keys
                    (mapcar (lambda (b)
                              (let ((var (first b))
                                    (is-special (third b)))
                                (if (or is-special
                                        (member (symbol-name var) needs-boxing
                                                :test #'string=))
                                    (gen-local "INIT")
                                    nil)))
                            binding-info)))
              ;; Evaluate inits in old scope — inits bind a single value (D577):
              ;; never in tail position and never in MV context.
              (loop for b in binding-info
                    for tk in temp-keys
                    do (let ((init-form (second b))
                             (key (fourth b)))
                         (let ((init-code
                                 (let ((*in-tail-position* nil)
                                       (*in-mv-context* nil))
                                   (if init-form
                                       (compile-expr init-form)
                                       (emit-nil)))))
                           (if tk
                               ;; Boxed / special: route via temp; bind-instrs
                               ;; will consume it in the new scope.
                               (setf init-instrs
                                     (append init-instrs
                                             `((:declare-local ,tk "LispObject")
                                               ,@init-code
                                               (:stloc ,tk))))
                               ;; Plain lexical: declare the final key here and
                               ;; stloc directly. The new scope's *locals*
                               ;; entry already points at this key, so no
                               ;; further bind-instr is needed for this var.
                               (setf init-instrs
                                     (append init-instrs
                                             `((:declare-local ,key "LispObject")
                                               ,@init-code
                                               (:stloc ,key))))))))
              ;; Now bind in new scope
              ;; Filter *boxed-vars* to remove names being rebound as non-boxed
              (let* ((non-boxed-names (set-difference
                                       (mapcar #'symbol-name var-names)
                                       needs-boxing :test #'string=))
                     (filtered-boxed (remove-if
                                      (lambda (x)
                                        (member (if (symbolp x) (symbol-name x) x)
                                                non-boxed-names :test #'string=))
                                      *boxed-vars*))
                     ;; Remove declared-specials from *locals* so references use dynamic binding
                     (ds-names (mapcar #'symbol-name declared-specials))
                     (*locals* (remove-if
                                (lambda (entry)
                                  (member (let ((k (car entry)))
                                            (if (symbolp k) (symbol-name k) ""))
                                          ds-names :test #'string=))
                                (append new-local-entries *locals*)))
                     (*specials* all-specials)
                     ;; Shadow symbol-macros for variables being bound by this let
                     (*symbol-macros* (remove-if
                                       (lambda (entry)
                                         (member (symbol-name (car entry))
                                                 (mapcar #'symbol-name var-names)
                                                 :test #'string=))
                                       *symbol-macros*))
                     (*boxed-vars* (append
                                    (mapcar (lambda (name)
                                              (find name var-names
                                                    :key #'symbol-name :test #'string=))
                                            needs-boxing)
                                    filtered-boxed)))
                ;; Declare and store each binding. Plain lexical bindings (tk
                ;; is nil) were already finalized in init-instrs and need no
                ;; bind-instr here.
                (loop for b in binding-info
                      for tk in temp-keys
                      do (let ((var (first b))
                               (is-special (third b))
                               (key (fourth b)))
                           (cond
                             (is-special
                              (push var special-syms)
                              (setf bind-instrs
                                    (append bind-instrs
                                            `(,@(compile-sym-lookup var)
                                              (:castclass "Symbol")
                                              (:ldloc ,tk)
                                              (:call "DynamicBindings.Push")))))
                             ((member (symbol-name var) needs-boxing :test #'string=)
                              (setf bind-instrs
                                    (append bind-instrs
                                            `((:declare-local ,key "LispObject[]")
                                              (:ldc-i4 1) (:newarr "LispObject")
                                              (:dup) (:ldc-i4 0)
                                              (:ldloc ,tk) (:stelem-ref)
                                              (:stloc ,key)))))
                             (t
                              ;; Plain lexical: already initialized in
                              ;; init-instrs via direct stloc to key.
                              nil))))
                ;; Compile body — inherits outer *in-tail-position*.
                ;; But when we have special bindings, the body is wrapped in a
                ;; try/finally (to Pop on exit), so self-TCO's raw `br` to the
                ;; loop label outside the try would produce invalid IL. Flag
                ;; *in-try-block* suppresses TCO branch generation in that case
                ;; while still allowing MV propagation through tail position.
                (let ((body-instrs (let ((*in-try-block*
                                          (or *in-try-block* special-syms))
                                         (*fixnum-locals*
                                          (append (extract-fixnum-locals body)
                                                  *fixnum-locals*))
                                         (*double-float-locals*
                                          (append (extract-double-float-locals body)
                                                  *double-float-locals*))
                                         (*single-float-locals*
                                          (append (extract-single-float-locals body)
                                                  *single-float-locals*)))
                                     (compile-progn real-body))))
                  (compile-let-with-specials
                   init-instrs bind-instrs body-instrs
                   (reverse special-syms)))))
            ;; Let*: sequential binding — incrementally extend *locals*
            (let ((*locals* *locals*)
                  (*specials* all-specials)
                  (*symbol-macros* *symbol-macros*)
                  (*boxed-vars* (append
                                  (mapcar (lambda (name)
                                            (find name var-names
                                                  :key #'symbol-name :test #'string=))
                                          needs-boxing)
                                  *boxed-vars*)))
              ;; Bind one at a time; compile init THEN extend scope
              (loop for b in binding-info
                    do (let ((var (first b))
                             (init-form (second b))
                             (is-special (third b))
                             (key (fourth b)))
                           (if is-special
                               (let ((tmp (gen-local "SPLTMP")))
                                 (push var special-syms)
                                 ;; Shadow any symbol-macro with this name
                                 (setf *symbol-macros*
                                       (remove var *symbol-macros*
                                               :key #'car :test #'eq))
                                 (setf bind-instrs
                                       (append bind-instrs
                                               `((:declare-local ,tmp "LispObject")
                                                 ;; Init forms bind a single value (D577)
                                                 ,@(let ((*in-tail-position* nil)
                                                         (*in-mv-context* nil))
                                                     (if init-form
                                                         (compile-expr init-form)
                                                         (emit-nil)))
                                                 (:stloc ,tmp)
                                                 ,@(compile-sym-lookup var)
                                                 (:castclass "Symbol")
                                                 (:ldloc ,tmp)
                                                 (:call "DynamicBindings.Push")))))
                               ;; Lexical: compile init in current scope (not tail pos), then extend
                               (let ((init-code (let ((*in-tail-position* nil)
                                                      (*in-mv-context* nil))
                                                  (if init-form
                                                      (compile-expr init-form)
                                                      (emit-nil)))))
                                 ;; Extend scope for subsequent bindings and body
                                 (push (cons var key) *locals*)
                                 ;; Shadow any symbol-macro with this name
                                 (setf *symbol-macros*
                                       (remove var *symbol-macros*
                                               :key #'car :test #'eq))
                                 (if (member (symbol-name var) needs-boxing :test #'string=)
                                     (setf bind-instrs
                                           (append bind-instrs
                                                   `((:declare-local ,key "LispObject[]")
                                                     (:ldc-i4 1) (:newarr "LispObject")
                                                     (:dup) (:ldc-i4 0)
                                                     ,@init-code
                                                     (:stelem-ref) (:stloc ,key))))
                                     (progn
                                       ;; Remove from *boxed-vars* to shadow outer boxed binding (e.g. labels)
                                       (when (boxed-var-p var)
                                         (setf *boxed-vars*
                                               (remove (symbol-name var) *boxed-vars*
                                                       :key (lambda (x) (if (symbolp x) (symbol-name x) x))
                                                       :test #'string=)))
                                       (setf bind-instrs
                                             (append bind-instrs
                                                     `((:declare-local ,key "LispObject")
                                                       ,@init-code
                                                       (:stloc ,key))))))))))
              ;; Remove declared-specials from *locals* so body references use dynamic binding
              (let* ((ds-names (mapcar #'symbol-name declared-specials))
                     ;; Body inherits outer *in-tail-position*; TCO branches
                     ;; are suppressed via *in-try-block* when special-syms
                     ;; introduce a try/finally (see parallel-let branch).
                     (body-instrs (let ((*locals* (remove-if
                                                   (lambda (entry)
                                                     (member (let ((k (car entry)))
                                                               (if (symbolp k) (symbol-name k) ""))
                                                             ds-names :test #'string=))
                                                   *locals*))
                                        (*in-try-block*
                                         (or *in-try-block* special-syms))
                                        (*fixnum-locals*
                                         (append (extract-fixnum-locals body)
                                                 *fixnum-locals*))
                                        (*double-float-locals*
                                         (append (extract-double-float-locals body)
                                                 *double-float-locals*))
                                        (*single-float-locals*
                                         (append (extract-single-float-locals body)
                                                 *single-float-locals*)))
                                    (compile-progn real-body))))
                (compile-let-with-specials
                 '() bind-instrs body-instrs
                 (reverse special-syms)))))))))

(defun compile-let-with-specials (init-instrs bind-instrs body-instrs special-syms)
  "Wrap body with try/finally for special variable cleanup if needed."
  (if (null special-syms)
      (eliminate-single-ref-locals `(,@init-instrs ,@bind-instrs ,@body-instrs))
      (let ((result-key (gen-local "RESULT")))
        `(,@init-instrs
          ,@bind-instrs
          (:declare-local ,result-key "LispObject")
          (:begin-exception-block)
          ,@body-instrs
          (:stloc ,result-key)
          (:begin-finally-block)
          ,@(loop for sym in special-syms
                  append `(,@(compile-sym-lookup sym)
                           (:castclass "Symbol") (:call "DynamicBindings.Pop")))
          (:end-exception-block)
          (:ldloc ,result-key)))))

(defun compile-let-star (bindings body)
  "Compile let* by delegating to compile-let with sequential flag."
  (compile-let bindings body t))

;;; ============================================================
;;; setq
;;; ============================================================

(defun single-value-form-p (expr)
  "Return T if expr is guaranteed to produce a single value (no MV).
   Used to skip MultipleValues.Primary in setq."
  (cond
    ((null expr) t)
    ((eq expr t) t)
    ((numberp expr) t)
    ((characterp expr) t)
    ((stringp expr) t)
    ((keywordp expr) t)
    ;; Variable reference (any symbol evaluates to a single value)
    ((symbolp expr) t)
    ;; (quote ...) or (the type expr)
    ((and (consp expr) (eq (car expr) 'quote)) t)
    ((and (consp expr) (eq (car expr) 'the))
     (single-value-form-p (caddr expr)))
    ;; Non-local exits (return-from, throw, go) transfer control and
    ;; leave the stack unchanged from the compiler's perspective — adding
    ;; an UnwrapMv after would try to pop a non-existent value.
    ((and (consp expr)
          (member (car expr) '(return-from return throw go)))
     t)
    ;; Known single-value operators (direct calls, never produce MV)
    ((and (consp expr) (symbolp (car expr))
          (not (assoc (mangle-name (car expr)) *local-functions* :test #'string=))
          (member (car expr)
                  '(car cdr cons list list* first rest second third
                    cadr cddr caar cdar caddr
                    + - * / 1+ 1- mod rem
                    < > <= >= = /=
                    eq eql equal
                    not null atom consp listp numberp integerp symbolp
                    stringp characterp functionp
                    length last nconc nreverse reverse
                    rplaca rplacd nth nthcdr
                    char-code code-char char
                    ash logand logior logxor lognot
                    typep type-of
                    get
                    aref vector-push-extend
                    setq)))
     t)
    ;; Functions with a declaimed ftype return type are single-value: atomic
    ;; types like fixnum / double-float / etc. cannot appear as multiple values.
    ;; Honors user `(declaim (ftype (function (...) fixnum) NAME))` so calls to
    ;; such NAME elide Runtime.UnwrapMv. (D685)
    ((and (consp expr) (symbolp (car expr))
          (not (assoc (mangle-name (car expr)) *local-functions* :test #'string=))
          (let ((ret (gethash (car expr) *function-return-types*)))
            (and ret
                 (member ret '(fixnum bit double-float single-float
                                      short-float long-float
                                      integer float real rational number
                                      character boolean)))))
     t)
    (t nil)))

(defun compile-setq (var val-expr)
  "Compile (setq var value). Returns the assigned value on stack (single value)."
  ;; If var is a symbol-macro, delegate to SETF of the expansion
  (let ((sm-exp (lookup-symbol-macro var)))
    (when sm-exp
      (return-from compile-setq (compile-expr `(setf ,sm-exp ,val-expr)))))
  ;; Variable assignment binds a single value. Force *in-mv-context* nil
  ;; and *in-tail-position* nil when compiling val-expr so MvReturn is
  ;; unwrapped before storage (mirrors compile-let D577).
  (let ((val-instrs (let ((*in-tail-position* nil)
                          (*in-mv-context* nil))
                      (compile-expr val-expr))))
    ;; Check lexical binding BEFORE special — mirrors compile-var-ref ordering
    (let ((key (lookup-local var)))
      (if key
          (if (boxed-var-p var)
              ;; Boxed: box[0] = value, return value
              (let ((tmp (gen-local "SETQ")))
                `((:declare-local ,tmp "LispObject")
                  ,@val-instrs
                  (:stloc ,tmp)
                  (:ldloc ,key) (:ldc-i4 0) (:ldloc ,tmp) (:stelem-ref)
                  (:ldloc ,tmp)))
              ;; Simple local: store and return value
              `(,@val-instrs
                (:dup)
                (:stloc ,key)))
          ;; No lexical binding — use special/dynamic assignment
          (let ((tmp (gen-local "SETQSPL")))
            `((:declare-local ,tmp "LispObject")
              ,@val-instrs
              (:stloc ,tmp)
              ,@(compile-sym-lookup var)
              (:castclass "Symbol")
              (:ldloc ,tmp)
              (:call "DynamicBindings.Set")))))))

;;; ============================================================
;;; lambda / closure
;;; ============================================================

(defun find-free-vars-with-defaults (params body)
  "Find free variables in body AND in &optional/&key default forms."
  (multiple-value-bind (required optional key rest-param) (parse-lambda-list params)
    (declare (ignore rest-param))
    (let* ((all-params (append required
                               (mapcar #'car optional)
                               (remove nil (mapcar #'third optional))
                               (mapcar #'second key)
                               (remove nil (mapcar #'fourth key))))
           (bound-names (mapcar #'symbol-name all-params))
           (free-ht (make-hash-table :test #'equal)))
      ;; Scan body
      (dolist (form body)
        (find-free-vars-expr form bound-names free-ht))
      ;; Scan default forms in &optional/&key — pass nil for bound
      ;; since scan-lambda-list-defaults handles progressive scoping
      ;; (each default only sees params to its left, not all params)
      (scan-lambda-list-defaults params nil free-ht)
      (let ((keys '())) (maphash (lambda (k v) (declare (ignore v)) (push k keys)) free-ht) keys))))

(defun compile-lambda (params body &optional (fn-name ""))
  "Compile (lambda (params) body...). FN-NAME enables self-TCO when non-empty."
  (multiple-value-bind (required optional key rest-param) (parse-lambda-list params)
    (declare (ignore optional key))
    (let ((free-vars (find-free-vars-with-defaults params body)))
      (if (null free-vars)
          ;; No captures — :make-function or :make-function-direct
          (if (simple-required-only-p params)
              `((:make-function-direct
                 :param-count ,(length required)
                 :body ,(compile-function-body-direct params body fn-name)))
              `((:make-function
                 :param-count ,(length required)
                 :body ,(compile-function-body params body))))
          ;; Closure: build env array, then :make-closure
          (compile-closure params body free-vars)))))

(defun compile-closure (params body free-vars)
  "Compile a closure lambda with captured variables."
  (multiple-value-bind (required optional key rest-param) (parse-lambda-list params)
    (declare (ignore optional key))
  (let* ((n-free (length free-vars))
         ;; Build env array on stack
         (env-build-instrs
           `((:ldc-i4 ,n-free)
             (:newarr "Object")
             ,@(loop for fv in free-vars
                     for i from 0
                     append (compile-env-capture fv i))))
         ;; Determine which free vars are boxed in outer scope
         (outer-boxed-fvs
           (remove-if-not (lambda (fv)
                            (or (let ((entry (assoc fv *locals*
                                                    :key (lambda (k) (symbol-name k))
                                                    :test #'string=)))
                                  (and entry (boxed-var-p (car entry))))
                                ;; Also check *local-functions* for boxed labels functions
                                (let ((lf (find fv *local-functions*
                                                :key #'first :test #'string=)))
                                  (and lf (third lf)))))
                          free-vars))
         ;; Compile inner body with env slots
         (inner-body (compile-closure-body params body free-vars outer-boxed-fvs)))
    `(,@env-build-instrs
      (:make-closure
       :param-count ,(length required)
       :env-size ,n-free
       :env-map ,(loop for fv in free-vars
                       for i from 0
                       collect (let ((entry (assoc fv *locals*
                                                   :key (lambda (k) (symbol-name k))
                                                   :test #'string=)))
                                 (if (and entry (boxed-var-p (car entry)))
                                     (list fv i "boxed")
                                     ;; Check *local-functions* for boxed labels functions
                                     (let ((lf (find fv *local-functions*
                                                     :key #'first :test #'string=)))
                                       (if (and lf (third lf))
                                           (list fv i "boxed")
                                           (list fv i "value"))))))
       :body ,inner-body)))))

(defun compile-env-capture (fv-name idx)
  "Generate instructions to capture a free variable into env[idx]."
  (let ((entry (assoc fv-name *locals*
                      :key (lambda (k) (symbol-name k)) :test #'string=)))
    (if entry
        (let ((key (cdr entry))
              (is-boxed (boxed-var-p (car entry))))
          `((:dup) (:ldc-i4 ,idx)
            ,@(if is-boxed
                  ;; Capture the box (LispObject[]) itself
                  `((:ldloc ,key))
                  ;; Capture the value
                  `((:ldloc ,key)))
            (:stelem-ref)))
        ;; Not in *locals* — check *local-functions* for boxed labels functions
        (let ((lf-entry (find fv-name *local-functions*
                              :key #'first :test #'string=)))
          (if (and lf-entry (third lf-entry))  ;; boxed-p
              `((:dup) (:ldc-i4 ,idx)
                (:ldloc ,(second lf-entry))  ;; the box key
                (:stelem-ref))
              ;; Not found — nil
              `((:dup) (:ldc-i4 ,idx) (:ldsfld "Nil.Instance") (:stelem-ref)))))))

(defun compile-closure-body (params body free-vars &optional outer-boxed-fvs (fn-name ""))
  "Compile function body for closure. Free vars access env (arg 0), params from args (arg 1).
   OUTER-BOXED-FVS is a list of free var name strings that were boxed in the outer scope.
   Handles &rest/&optional/&key parameters."
  (multiple-value-bind (required optional key rest-param aux allow-other-keys-p has-key-p) (parse-lambda-list params)
    (let ((body (if aux `((let* ,aux ,@body)) body)))
    (let* ((all-params (append required
                               (mapcar #'car optional)
                               (remove nil (mapcar #'third optional))
                               (mapcar #'second key)
                               (remove nil (mapcar #'fourth key))
                               (if rest-param (list rest-param) nil)))
           ;; Save outer local-functions for captured flet/labels functions
           (outer-local-fns *local-functions*)
           ;; Save outer block-tags for captured return-from
           (outer-block-tags *block-tags*)
           ;; Save outer go-tags for captured non-local go
           (outer-go-tags *go-tags*)
           (*locals* '())
           (*boxed-vars* '())
           (*block-tags* '())
           (*go-tags* '())
           (*local-functions* '())
           ;; Reset TCO state so inner closures don't inherit outer TCO context.
           ;; *self-fn-local* especially must be cleared — it refers to a local
           ;; declared in the OUTER method.
           (*tco-self-name* nil)
           (*tco-loop-label* nil)
           (*tco-param-entries* nil)
           (*tco-local-fn-key* nil)
           (*tco-leave-instrs* nil)
           (*tco-in-try-catch* nil)
           (*self-fn-local* nil)
           ;; Reset mutual-TCO: closures within labels group must not emit br-to-outer-TCOLOOP
           (*labels-mutual-tco* nil)
           ;; Reset native state: closures don't inherit outer native body context (#130)
           (*long-locals* nil)
           (*native-self-name* nil)
           ;; Closure body's last form is in tail position (D638): MV propagation
           (*in-tail-position* t)
           (n-required (length required))
           (key-start (+ n-required (length optional)))
           ;; Compute boxing needs for params within this closure
           (mutated (find-mutated-vars body))
           (all-var-names (append (mapcar #'symbol-name all-params)
                                  free-vars))
           (captured-inner (find-captured-vars body all-var-names))
           (needs-boxing (intersection mutated captured-inner :test #'string=))
           ;; Env slot locals
           (env-instrs '())
           (env-locals '()))
      ;; Set up free var locals from env
      (loop for fv in free-vars
            for i from 0
            do (let* ((key (gen-local fv))
                      (is-outer-boxed (member fv outer-boxed-fvs :test #'string=)))
                 (push (cons (intern fv :dotcl.cil-compiler) key) env-locals)
                 (setf env-instrs
                       (append env-instrs
                               (if is-outer-boxed
                                   `((:declare-local ,key "LispObject[]")
                                     (:load-env ,i)
                                     (:stloc ,key))
                                   `((:declare-local ,key "LispObject")
                                     (:load-env ,i)
                                     (:stloc ,key)))))))
      ;; Set up params
      (let* ((param-locals
               (loop for p in all-params
                     collect (let ((key (gen-local (symbol-name p))))
                               (cons p key))))
             (*locals* (append param-locals env-locals))
             ;; Shadow symbol-macros whose names match captured env variables or params.
             ;; When a variable like X is both a symbol-macro (x → (svref #:inst 0)) AND
             ;; captured in the env, accessing X inside the closure must use the local
             ;; variable, not the expansion. Without this shadow, the expansion would be
             ;; compiled but its free variables (e.g. #:inst) wouldn't be in *locals*.
             (*symbol-macros*
              (let ((all-local-names
                     (append (mapcar (lambda (p) (symbol-name p)) all-params)
                             free-vars)))
                (remove-if (lambda (entry)
                             (let ((k (car entry)))
                               (member (if (symbolp k) (symbol-name k) "")
                                       all-local-names :test #'string=)))
                           *symbol-macros*)))
             (*boxed-vars* (append
                            (mapcar (lambda (name)
                                      (or (find name all-params :key #'symbol-name :test #'string=)
                                          (intern name :dotcl.cil-compiler)))
                                    needs-boxing)
                            (mapcar (lambda (name) (intern name :dotcl.cil-compiler))
                                    (or outer-boxed-fvs '()))))
             ;; Re-establish *local-functions* for captured flet/labels functions
             (*local-functions*
              (loop for (fn-name fn-key fn-boxed-p) in outer-local-fns
                    for mangled = (concatenate 'string "__LABELFN_" fn-name)
                    for captured-name = (cond
                                          ((member mangled free-vars :test #'string=) mangled)
                                          ((member fn-name free-vars :test #'string=) fn-name)
                                          (t nil))
                    when captured-name
                    collect (let ((env-entry (assoc captured-name env-locals
                                                   :key (lambda (k) (symbol-name k))
                                                   :test #'string=)))
                              (list fn-name (cdr env-entry) fn-boxed-p))))
             ;; Re-establish *block-tags* for captured block tags
             (*block-tags*
              (loop for (bname . binfo) in outer-block-tags
                    for tag-var = (block-tag-var-name bname)
                    for env-entry = (assoc tag-var env-locals
                                           :key (lambda (k) (symbol-name k))
                                           :test #'string=)
                    when env-entry
                    collect (cons bname (list (cdr env-entry) nil nil nil nil (sixth binfo)))))
             ;; Re-establish *go-tags* for captured non-local go's
             ;; Format: (tag-name tb-var-name tb-id-key label-idx)
             ;; After capture: tb-id-key becomes the env local key
             (*go-tags*
              (loop for gt-entry in outer-go-tags
                    for tag-name = (first gt-entry)
                    for tb-var-name = (second gt-entry)
                    for label-idx = (fourth gt-entry)
                    for env-entry = (assoc tb-var-name env-locals
                                          :key (lambda (k) (symbol-name k))
                                          :test #'string=)
                    when env-entry
                    collect (list tag-name tb-var-name (cdr env-entry) label-idx)))
             (param-instrs
               (append
                ;; Required params: use :load-arg i (expands to ldarg 1; ldc i; ldelem-ref)
                (loop for p in required
                      for key = (cdr (assoc p param-locals))
                      for i from 0
                      if (boxed-var-p p)
                        append `((:declare-local ,key "LispObject[]")
                                 (:ldc-i4 1) (:newarr "LispObject") (:dup)
                                 (:ldc-i4 0) (:load-arg ,i) (:stelem-ref)
                                 (:stloc ,key))
                      else
                        append `((:declare-local ,key "LispObject")
                                 (:load-arg ,i) (:stloc ,key)))
                ;; Optional params: check args.Length (arg 1 is LispObject[], with boxing & supplied-p)
                (let ((opt-instrs nil)
                      (remaining-opt-names (mapcar #'car optional)))
                  (loop for (opt-name opt-default sp-var) in optional
                        for i from n-required
                        for key = (cdr (assoc opt-name param-locals))
                        do ;; Remove current+later opt param locals from *locals* so default sees outer scope
                           ;; Only remove param-locals entries, keep env-locals (captured variables)
                           (let* ((params-to-remove (mapcar #'symbol-name remaining-opt-names))
                                  (*locals* (append
                                             (remove-if (lambda (entry)
                                                          (member (symbol-name (car entry))
                                                                  params-to-remove
                                                                  :test #'string=))
                                                        param-locals)
                                             env-locals))
                                  (default-label (gen-label "OPTDEF"))
                                  (done-label (gen-label "OPTDONE")))
                             (setq opt-instrs
                                   (append opt-instrs
                                           (if (boxed-var-p opt-name)
                                               (let ((tmp (gen-local "OPTTMP")))
                                                 `((:declare-local ,tmp "LispObject")
                                                   (:ldarg 1) (:ldlen) (:conv-i4)
                                                   (:ldc-i4 ,(1+ i))
                                                   (:blt ,default-label)
                                                   (:load-arg ,i)
                                                   (:br ,done-label)
                                                   (:label ,default-label)
                                                   ,@(if opt-default
                                                         (compile-expr opt-default)
                                                         (emit-nil))
                                                   (:label ,done-label)
                                                   (:stloc ,tmp)
                                                   (:declare-local ,key "LispObject[]")
                                                   (:ldc-i4 1) (:newarr "LispObject") (:dup)
                                                   (:ldc-i4 0) (:ldloc ,tmp) (:stelem-ref)
                                                   (:stloc ,key)))
                                               `((:declare-local ,key "LispObject")
                                                 (:ldarg 1) (:ldlen) (:conv-i4)
                                                 (:ldc-i4 ,(1+ i))
                                                 (:blt ,default-label)
                                                 (:load-arg ,i)
                                                 (:br ,done-label)
                                                 (:label ,default-label)
                                                 ,@(if opt-default
                                                       (compile-expr opt-default)
                                                       (emit-nil))
                                                 (:label ,done-label)
                                                 (:stloc ,key)))))
                             ;; After initializing, this param is now visible to subsequent defaults
                             (pop remaining-opt-names)
                             (when sp-var
                               (let ((sp-key (cdr (assoc sp-var param-locals)))
                                     (sp-found-label (gen-label "OPTSPF"))
                                     (sp-done-label (gen-label "OPTSPD")))
                                 (setq opt-instrs
                                       (append opt-instrs
                                               `((:declare-local ,sp-key "LispObject")
                                                 (:ldarg 1) (:ldlen) (:conv-i4)
                                                 (:ldc-i4 ,(1+ i))
                                                 (:blt ,sp-found-label)
                                                 ,@(emit-t)
                                                 (:br ,sp-done-label)
                                                 (:label ,sp-found-label)
                                                 ,@(emit-nil)
                                                 (:label ,sp-done-label)
                                                 (:stloc ,sp-key))))))))
                  opt-instrs)
                ;; &key params: search from key-start (with boxing)
                ;; supplied-p emitted right after its key param (left-to-right init order)
                (let ((key-instrs nil)
                      (remaining-key-vars (mapcar #'second key)))
                  (dolist (key-spec key)
                    (let* ((key-name (first key-spec))
                           (var-name (second key-spec))
                           (key-default (third key-spec))
                           (sp-var (fourth key-spec))
                           (explicit-key-p (fifth key-spec))
                           (find-key-fn (if explicit-key-p "Runtime.FindKeyArgByName" "Runtime.FindKeyArg"))
                           (key (cdr (assoc var-name param-locals)))
                           (found-label (gen-label "KEYFOUND"))
                           (done-label (gen-label "KEYDONE")))
                      ;; Key param binding: exclude current+later key params from *locals*
                      ;; so defaults see outer scope (captured vars), not uninitialized params
                      (let* ((params-to-remove (mapcar #'symbol-name remaining-key-vars))
                             (default-instrs
                              (if key-default
                                  (let ((*locals* (append
                                                   (remove-if (lambda (entry)
                                                                (member (symbol-name (car entry))
                                                                        params-to-remove
                                                                        :test #'string=))
                                                              param-locals)
                                                   env-locals)))
                                    (compile-expr key-default))
                                  (emit-nil))))
                        (setq key-instrs
                              (append key-instrs
                                      (if (boxed-var-p var-name)
                                          (let ((tmp (gen-local "KEYTMP")))
                                            `((:declare-local ,tmp "LispObject")
                                              (:ldarg 1) (:ldc-i4 ,key-start)
                                              (:ldstr ,key-name)
                                              (:call ,find-key-fn)
                                              (:dup) (:brtrue ,found-label)
                                              (:pop)
                                              ,@default-instrs
                                              (:br ,done-label)
                                              (:label ,found-label)
                                              (:label ,done-label)
                                              (:stloc ,tmp)
                                              (:declare-local ,key "LispObject[]")
                                              (:ldc-i4 1) (:newarr "LispObject") (:dup)
                                              (:ldc-i4 0) (:ldloc ,tmp) (:stelem-ref)
                                              (:stloc ,key)))
                                          `((:declare-local ,key "LispObject")
                                            (:ldarg 1) (:ldc-i4 ,key-start)
                                            (:ldstr ,key-name)
                                            (:call ,find-key-fn)
                                            (:dup) (:brtrue ,found-label)
                                            (:pop)
                                            ,@default-instrs
                                            (:br ,done-label)
                                            (:label ,found-label)
                                            (:label ,done-label)
                                            (:stloc ,key))))))
                      ;; supplied-p variable right after its key param
                      (when sp-var
                        (let ((sp-key (cdr (assoc sp-var param-locals)))
                              (sp-found-label (gen-label "SPFOUND"))
                              (sp-done-label (gen-label "SPDONE")))
                          (setq key-instrs
                                (append key-instrs
                                        `((:declare-local ,sp-key "LispObject")
                                          (:ldarg 1) (:ldc-i4 ,key-start)
                                          (:ldstr ,key-name)
                                          (:call ,find-key-fn)
                                          (:brtrue ,sp-found-label)
                                          ,@(emit-nil)
                                          (:br ,sp-done-label)
                                          (:label ,sp-found-label)
                                          ,@(emit-t)
                                          (:label ,sp-done-label)
                                          (:stloc ,sp-key))))))
                      ;; After initializing, this key param is now visible to subsequent defaults
                      (pop remaining-key-vars)))
                  key-instrs)
                ;; Rest param: (:ldarg 1) is args array for closures
                (when rest-param
                  (let ((key (cdr (assoc rest-param param-locals)))
                        (n key-start))
                    (if (boxed-var-p rest-param)
                        `((:declare-local ,key "LispObject[]")
                          (:ldc-i4 1) (:newarr "LispObject") (:dup)
                          (:ldc-i4 0) (:ldarg 1) (:ldc-i4 ,n) (:call "Runtime.CollectRestArgs")
                          (:stelem-ref) (:stloc ,key))
                        `((:declare-local ,key "LispObject")
                          (:ldarg 1) (:ldc-i4 ,n) (:call "Runtime.CollectRestArgs")
                          (:stloc ,key))))))))
        (let* ((arity-instrs
                 (cond
                   ((and (null optional) (null key) (null rest-param) (not has-key-p))
                    `((:ldstr ,fn-name) (:ldarg 1) (:ldc-i4 ,n-required)
                      (:call "Runtime.CheckArityExact")))
                   ((and optional (null key) (null rest-param) (not has-key-p))
                    (let ((n-max (+ n-required (length optional))))
                      `((:ldstr ,fn-name) (:ldarg 1) (:ldc-i4 ,n-required)
                        (:call "Runtime.CheckArityMin")
                        (:ldstr ,fn-name) (:ldarg 1) (:ldc-i4 ,n-max)
                        (:call "Runtime.CheckArityMax"))))
                   (t
                    `((:ldstr ,fn-name) (:ldarg 1) (:ldc-i4 ,n-required)
                      (:call "Runtime.CheckArityMin")))))
               (key-check-instrs
                 (when (and key (not allow-other-keys-p))
                   (let ((n-keys (length key)))
                     `((:ldstr ,fn-name)
                       (:ldarg 1) (:ldc-i4 ,key-start)
                       (:ldc-i4 ,n-keys) (:newarr "String")
                       ,@(loop for i from 0
                               for key-spec in key
                               append `((:dup) (:ldc-i4 ,i) (:ldstr ,(first key-spec)) (:stelem-ref)))
                       (:call "Runtime.CheckNoUnknownKeys"))))))
          ;; Handle special params in closure body (declare + globally special)
          (let* ((special-param-syms
                   (union (fn-body-special-params body (mapcar #'symbol-name all-params))
                          (remove-if-not #'global-special-p all-params)))
                 (sp-names (mapcar #'symbol-name special-param-syms))
                 (special-push-instrs
                   (loop for p in special-param-syms
                         for pkey = (cdr (assoc p param-locals))
                         append `(,@(compile-sym-lookup p)
                                  (:castclass "Symbol")
                                  (:ldloc ,pkey)
                                  (:call "DynamicBindings.Push"))))
                 ;; Also find free vars (non-params) declared special in body.
                 ;; These must be removed from *locals* so body references use
                 ;; DynamicBindings.Get instead of the closure-captured lexical value.
                 ;; Unwrap optional (block name ...) wrapper to find leading declares.
                 (body-inner-forms
                   (if (and (= (length body) 1)
                            (consp (car body))
                            (eq (caar body) 'block))
                       (cddar body)
                       body))
                 (body-declared-specials
                   (multiple-value-bind (specials _rest) (extract-specials body-inner-forms)
                     (declare (ignore _rest))
                     specials))
                 (free-special-names
                   (mapcar #'symbol-name
                           (remove-if (lambda (s)
                                        (member (symbol-name s)
                                                (mapcar #'symbol-name all-params)
                                                :test #'string=))
                                      body-declared-specials)))
                 (all-special-names (append sp-names free-special-names))
                 (*locals* (remove-if (lambda (entry)
                                        (member (let ((k (car entry)))
                                                  (if (symbolp k) (symbol-name k) ""))
                                                all-special-names :test #'string=))
                                      *locals*))
                 (body-instrs (compile-progn body)))
            (merge-disjoint-locals
             (if special-param-syms
                 `(,@arity-instrs
                   ,@key-check-instrs
                   ,@env-instrs ,@param-instrs
                   ,@(compile-let-with-specials '() special-push-instrs body-instrs special-param-syms)
                   (:ret))
                 `(,@arity-instrs
                   ,@key-check-instrs
                   ,@env-instrs ,@param-instrs
                   ,@body-instrs
                   (:ret)))))))))))



;;; ============================================================
;;; funcall
;;; ============================================================

(defun compile-funcall (args)
  "Compile (funcall fn arg1 arg2 ...).
   Per CL spec, if fn is a symbol, calls symbol-function on it first."
  (when (null args)
    (return-from compile-funcall
      (compile-static-program-error "FUNCALL: too few arguments (expected function designator)")))
  (let ((fn-expr (car args)))
    (if (and (consp fn-expr)
             (or (eq (car fn-expr) 'quote) (eq (car fn-expr) 'function))
             (symbolp (cadr fn-expr))
             ;; Only optimize if sym is NOT shadowed by a local flet/labels.
             (not (assoc (mangle-name (cadr fn-expr)) *local-functions* :test #'string=)))
        ;; (funcall 'sym ...) or (funcall #'sym ...) → compile as named call
        (compile-named-call (cadr fn-expr) (cdr args))
        ;; General case: evaluate fn, coerce symbol to function if needed
        (let ((call-args (cdr args))
              (n-call-args (length (cdr args))))
          (if (<= n-call-args 8)
              (let* ((fn-tmp (gen-local "FNTMP"))
                     (da (compile-direct-call-args call-args))
                     (temps (car da))
                     (eval-instrs (cdr da)))
                `((:declare-local ,fn-tmp "LispFunction")
                  ,@(let ((*in-mv-context* nil) (*in-tail-position* nil))
                      (compile-expr fn-expr))
                  (:call "Runtime.CoerceToFunction")
                  (:stloc ,fn-tmp)
                  ,@eval-instrs
                  (:ldloc ,fn-tmp)
                  ,@(loop for tmp in temps append `((:ldloc ,tmp)))
                  (:callvirt ,(format nil "LispFunction.Invoke~D" n-call-args))))
              `(,@(let ((*in-mv-context* nil) (*in-tail-position* nil))
                    (compile-expr fn-expr))
                (:call "Runtime.CoerceToFunction")
                ,@(compile-args-array call-args)
                (:callvirt "LispFunction.Invoke")))))))

;;; ============================================================
;;; function special form + flet / labels
;;; ============================================================

(defvar *builtin-varargs-methods*
  '(("+" . "Runtime.AddN") ("-" . "Runtime.SubtractN")
    ("*" . "Runtime.MultiplyN") ("/" . "Runtime.DivideN")
    ("=" . "Runtime.NumEqualN") ("/=" . "Runtime.NumNotEqualN")
    ("<" . "Runtime.LessThanN") (">" . "Runtime.GreaterThanN")
    ("<=" . "Runtime.LessEqualN") (">=" . "Runtime.GreaterEqualN"))
  "Builtins that accept params LispObject[] directly.")

(defvar *builtin-unary-methods*
  '(("CAR" . "Runtime.Car") ("CDR" . "Runtime.Cdr")
    ("NOT" . "Runtime.Not") ("NULL" . "Runtime.Not")
    ("ATOM" . "Runtime.Atom") ("CONSP" . "Runtime.Consp")
    ("LISTP" . "Runtime.Listp") ("NUMBERP" . "Runtime.Numberp")
    ("INTEGERP" . "Runtime.Integerp") ("SYMBOLP" . "Runtime.Symbolp")
    ("STRINGP" . "Runtime.Stringp") ("CHARACTERP" . "Runtime.Characterp")
    ("FUNCTIONP" . "Runtime.Functionp") ("LENGTH" . "Runtime.Length")
    ("ABS" . "Runtime.Abs") ("NREVERSE" . "Runtime.Nreverse")
    ("LAST" . "Runtime.Last") ("COPY-LIST" . "Runtime.CopyList")
    ("CADR" . "Runtime.Cadr") ("CDDR" . "Runtime.Cddr")
    ("CAAR" . "Runtime.Caar") ("CDAR" . "Runtime.Cdar")
    ("CADDR" . "Runtime.Caddr") ("PRINT" . "Runtime.Print")
    ("PRIN1" . "Runtime.Prin1") ("PRINC" . "Runtime.Princ")
    ("SYMBOL-NAME" . "Runtime.SymbolName")
    ;; STRING-UPCASE/DOWNCASE removed: they take &key start end, so not truly unary.
    ;; Wrapping as unary drops the LispObject[] and passes a single LispObject,
    ;; but Runtime.StringUpcase expects LispObject[].
    ("CHAR-CODE" . "Runtime.CharCode") ("CODE-CHAR" . "Runtime.CodeChar")
    ("TYPE-OF" . "Runtime.TypeOf")
    ("RATIONALP" . "Runtime.Rationalp") ("FLOATP" . "Runtime.Floatp")
    ("COMPLEXP" . "Runtime.Complexp"))
  "Builtins that take exactly 1 arg.")

(defvar *builtin-binary-methods*
  '(("CONS" . "Runtime.MakeCons") ("EQ" . "Runtime.Eq")
    ("EQL" . "Runtime.Eql") ("EQUAL" . "Runtime.Equal")
    ("MOD" . "Runtime.Mod") ("REM" . "Runtime.Rem")
    ("NTH" . "Runtime.Nth") ("NTHCDR" . "Runtime.Nthcdr")
    ("MEMBER" . "Runtime.Member") ("ASSOC" . "Runtime.Assoc")
    ("RPLACA" . "Runtime.Rplaca") ("RPLACD" . "Runtime.Rplacd")
    ("APPLY" . "Runtime.Apply"))
  "Builtins that take exactly 2 args.")

;;; (TCO defvars moved to top of file for proper SBCL special-var recognition)

(defun compile-builtin-function-ref (name-str)
  "Compile a reference to a built-in function. Returns instruction list or NIL."
  (let ((varargs (assoc name-str *builtin-varargs-methods* :test #'string=))
        (unary (assoc name-str *builtin-unary-methods* :test #'string=))
        (binary (assoc name-str *builtin-binary-methods* :test #'string=)))
    (cond
      (varargs
       ;; Varargs: (lambda args) → call Runtime.XxxN(args)
       `((:make-function :param-count 0 :name ,name-str
          :body ((:ldarg 0) (:call ,(cdr varargs)) (:ret)))))
      (unary
       ;; Unary: check arity, then call Runtime.Xxx(arg)
       `((:make-function :param-count 0 :name ,name-str
          :body ((:ldstr ,name-str) (:ldarg 0) (:call "Runtime.CheckUnaryArity")
                 (:ldarg 0) (:ldc-i4 0) (:ldelem-ref)
                 (:call ,(cdr unary)) (:ret)))))
      (binary
       ;; Binary: check arity, then call Runtime.Xxx(a, b)
       `((:make-function :param-count 0 :name ,name-str
          :body ((:ldstr ,name-str) (:ldarg 0) (:call "Runtime.CheckBinaryArity")
                 (:ldarg 0) (:ldc-i4 0) (:ldelem-ref)
                 (:ldarg 0) (:ldc-i4 1) (:ldelem-ref)
                 (:call ,(cdr binary)) (:ret)))))
      ;; list: special case — varargs using Runtime.List
      ((string= name-str "LIST")
       `((:make-function :param-count 0 :name ,name-str
          :body ((:ldarg 0) (:call "Runtime.List") (:ret)))))
      ((string= name-str "LIST*")
       `((:make-function :param-count 0 :name ,name-str
          :body ((:ldarg 0) (:call "Runtime.ListStar") (:ret)))))
      ((string= name-str "VALUES")
       `((:make-function :param-count 0 :name ,name-str
          :body ((:ldarg 0) (:call "Runtime.Values") (:ret)))))
      (t nil))))

(defun compile-function-special (thing)
  "Compile (function name) or (function (lambda ...))."
  (cond
    ((and (consp thing) (eq (car thing) 'lambda))
     (compile-lambda (cadr thing) (cddr thing)))
    ;; (function (named-lambda name lambda-list . body)) — SBCL extension
    ;; Treat as plain lambda, discarding the name
    ((and (consp thing) (symbolp (car thing))
          (string= (symbol-name (car thing)) "NAMED-LAMBDA"))
     (compile-lambda (caddr thing) (cdddr thing)))
    ;; (function (setf name)) — setf function reference via sym.SetfFunction (#58 Phase 2)
    ((and (consp thing) (eq (car thing) 'setf) (symbolp (cadr thing)))
     `(,@(compile-sym-lookup (cadr thing))
       (:castclass "Symbol")
       (:call "CilAssembler.GetSetfFunctionBySymbol")))
    ((symbolp thing)
     ;; Check local functions first
     (let ((local-fn (assoc (symbol-name thing) *local-functions* :test #'string=)))
       (if local-fn
           (let ((key (second local-fn))
                 (boxed-p (third local-fn)))
             (if boxed-p
                 `((:ldloc ,key) (:ldc-i4 0) (:ldelem-ref))
                 `((:ldloc ,key))))
           ;; Check builtin
           (let ((builtin-instrs (compile-builtin-function-ref (symbol-name thing))))
             (if builtin-instrs
                 builtin-instrs
                 ;; Global user-defined function — use symbol-based lookup so that
                 ;; package-qualified names (e.g. #'bt2:current-thread) resolve to
                 ;; the correct package's symbol, not a same-named symbol in another
                 ;; package (e.g. dotcl:current-thread). GetFunctionBySymbol checks
                 ;; sym.Function first, then falls back to cross-package search.
                 `(,@(compile-sym-lookup thing)
                   (:castclass "Symbol")
                   (:call "CilAssembler.GetFunctionBySymbol")))))))
    (t (error "FUNCTION: unsupported argument ~s" thing))))

(defun compile-macrolet (macro-defs body)
  "Compile (macrolet ((name (params) body...) ...) body...).
   Temporarily registers local macros in *macros*, compiles body, then restores."
  (let ((saved '()))
    ;; Save existing macro entries and register local macros
    (dolist (def macro-defs)
      (let* ((name (car def))
             (params (cadr def))
             (mbody (cddr def))
             (old-entry (gethash name *macros*)))
        (push (cons name old-entry) saved)
        ;; Build a macro expander lambda and eval it.
        ;; If lambda list starts with &whole var, bind var to the whole form,
        ;; then destructure the rest against (cdr form) per CL macro lambda list rules.
        (let* ((whole-var (when (and (consp params) (eq (car params) '&whole))
                            (cadr params)))
               (rest-params (if whole-var (cddr params) params)))
          ;; Strip &environment from rest-params (handle separately)
          (let* ((env-var nil)
                 (clean-params
                  (let ((result '()) (p rest-params))
                    (loop
                      (when (null p) (return))
                      (cond
                        ((eq (car p) '&environment)
                         (setq env-var (cadr p))
                         (setq p (cddr p)))
                        (t (push (car p) result) (setq p (cdr p)))))
                    (nreverse result))))
            (let* ((expander-form
                    (cond
                      ((and whole-var (consp whole-var))
                       ;; &whole (pattern) — destructure whole form
                       `(lambda (form)
                          ,(if env-var `(declare (ignore ,env-var)) '(progn))
                          (destructuring-bind ,whole-var form
                            (destructuring-bind ,clean-params (cdr form)
                              ,@mbody))))
                      (whole-var
                       ;; &whole var — bind var to entire form
                       `(lambda (form)
                          (let ((,whole-var form))
                            (destructuring-bind ,clean-params (cdr form)
                              ,@mbody))))
                      (env-var
                       ;; &environment but no &whole
                       ;; Build env as cons: (macros-ht . symbol-macros-alist)
                       `(lambda (form)
                          (let ((,env-var (cons *macros*
                                               (let ((ht (make-hash-table :test #'equal)))
                                                 (dolist (entry *symbol-macros* ht)
                                                   (setf (gethash (symbol-name (car entry)) ht) (cdr entry)))))))
                            (destructuring-bind ,clean-params (cdr form)
                              ,@mbody))))
                      (t
                       `(lambda (form)
                          (destructuring-bind ,clean-params (cdr form)
                            ,@mbody)))))
                   ;; Wrap with surrounding flet/labels so macrolet body can call
                   ;; enclosing locally-defined functions at expansion time (issue #76)
                   ;; NOTE: eval here requires Reflection.Emit (unavailable in NativeAOT).
                   ;; For NativeAOT compatibility, replace with an S-expression interpreter.
                   (eval-form (if *compile-time-flet-defs*
                                  `(flet ,*compile-time-flet-defs* ,expander-form)
                                  expander-form))
                   (expander-fn (eval eval-form)))
              (setf (gethash name *macros*) expander-fn))))))
    ;; Compile body with local macros active
    ;; Handle (declare (special ...)) in body — remove those vars from *locals*
    ;; so they use dynamic binding (like locally does)
    (multiple-value-bind (declared-specials real-body) (extract-specials body)
    (let* ((*locals* (if declared-specials
                         (remove-if (lambda (entry)
                                      (member (symbol-name (car entry))
                                              (mapcar #'symbol-name declared-specials)
                                              :test #'string=))
                                    *locals*)
                         *locals*))
           (result (compile-progn real-body)))
      ;; Restore original macro entries
      (dolist (entry saved)
        (if (cdr entry)
            (setf (gethash (car entry) *macros*) (cdr entry))
            (remhash (car entry) *macros*)))
      result))))

(defun compile-symbol-macrolet (bindings body)
  "Compile (symbol-macrolet ((sym expansion)...) body...).
   Temporarily extends *symbol-macros* with new bindings."
  ;; Validate: symbol-macro names must not be constants or special variables
  (dolist (b bindings)
    (let ((name (car b)))
      (when (or (constantp name)
                (global-special-p name))
        (compile-expr `(error 'program-error))
        (return-from compile-symbol-macrolet
          (compile-expr `(error 'program-error))))))
  ;; Check for (declare (special ...)) in body that conflicts with symbol-macro names
  (let ((binding-names (mapcar #'car bindings)))
    (dolist (form body)
      (when (and (consp form) (eq (car form) 'declare))
        (dolist (decl (cdr form))
          (when (and (consp decl) (eq (car decl) 'special))
            (dolist (sname (cdr decl))
              (when (member sname binding-names)
                (return-from compile-symbol-macrolet
                  (compile-expr `(error 'program-error))))))))))
  (let ((*symbol-macros* (append (mapcar (lambda (b) (cons (car b) (cadr b)))
                                         bindings)
                                 *symbol-macros*)))
    ;; Process free special declarations (like locally does)
    (multiple-value-bind (declared-specials real-body) (extract-specials body)
      (if (null declared-specials)
          (compile-progn real-body)
          (let* ((*specials* (append declared-specials *specials*))
                 (special-names (mapcar #'symbol-name declared-specials))
                 (*locals* (remove-if
                            (lambda (entry)
                              (let ((k (car entry)))
                                (member (if (symbolp k) (symbol-name k) "")
                                        special-names :test #'string=)))
                            *locals*)))
            (compile-progn real-body))))))

(defun compile-flet (fn-defs body)
  "Compile (flet ((name (params) body...) ...) body...)."
  (let ((fn-instrs '())
        (new-local-fns '())
        (new-locals '()))
    ;; Compile each function definition in OUTER scope (flet functions can't see each other)
    (dolist (fdef fn-defs)
      (let* ((name (car fdef))
             (params (cadr fdef))
             (fn-body (cddr fdef))
             (name-str (mangle-name name))
             (key (gen-local name-str)))
        ;; Compile the lambda (in current scope, not extended)
        ;; CL spec: flet creates an implicit block named after the function
        ;; For (setf sym) names, use progn instead of block (block requires a symbol)
        (let ((lambda-instrs (if (and (symbolp name)
                                      (some (lambda (f) (form-has-return-from-p name f)) fn-body))
                                 (compile-lambda params `((block ,name ,@fn-body)))
                                 (compile-lambda params fn-body))))
          (setf fn-instrs
                (append fn-instrs
                        `((:declare-local ,key "LispObject")
                          ,@lambda-instrs
                          (:stloc ,key))))
          (push (list name-str key nil) new-local-fns)
          ;; Also track symbolp names in *locals* so closures can capture flet functions
          (when (symbolp name)
            (push (cons (intern (symbol-name name) :dotcl.cil-compiler) key) new-locals)))))
    ;; Compile body with extended local-functions AND locals.
    ;; Also track flet source defs so compile-defmacro can wrap its eval
    ;; with flet bindings (e.g. SBCL macros.lisp wraps defmacro in flet).
    (let ((*local-functions* (append (nreverse new-local-fns) *local-functions*))
          (*locals* (append (nreverse new-locals) *locals*))
          (*compile-time-flet-defs* (append fn-defs *compile-time-flet-defs*)))
      `(,@fn-instrs
        ,@(compile-progn body)))))

(defun labels-required-only-params-p (params)
  "Return T if PARAMS is a required-only lambda list with no &optional/&key/&rest/&aux."
  (not (some (lambda (p) (member p '(&optional &key &rest &aux &allow-other-keys)))
             params)))

(defun compile-labels (fn-defs body)
  "Compile (labels ((name (params) body...) ...) body...).
   When all functions have the same required-only arity (>= 2 fns), uses a dispatch
   loop for mutual tail call optimization (#124/D919). Otherwise uses boxed closures."
  (let* ((n-fns (length fn-defs))
         (first-arity (and fn-defs (length (cadr (first fn-defs))))))
    (if (and (>= n-fns 2)
             (every (lambda (f) (labels-required-only-params-p (cadr f))) fn-defs)
             (every (lambda (f) (= (length (cadr f)) first-arity)) fn-defs))
        (compile-labels-mutual-tco fn-defs body first-arity)
        (compile-labels-boxed fn-defs body))))

(defun compile-labels-boxed (fn-defs body)
  "Compile (labels ...) using boxed closures (no mutual-TCO optimization)."
  (let ((box-instrs '())
        (new-local-fns '())
        (fn-compile-list '()))
    ;; Phase 1: Allocate boxes for all function names
    (dolist (fdef fn-defs)
      (let* ((name (car fdef))
             (name-str (mangle-name name))
             (key (gen-local name-str)))
        (setf box-instrs
              (append box-instrs
                      `((:declare-local ,key "LispObject[]")
                        (:ldc-i4 1) (:newarr "LispObject")
                        (:stloc ,key))))
        (push (list name-str key t) new-local-fns)
        (push (list name (cadr fdef) (cddr fdef) key) fn-compile-list)))
    (setf new-local-fns (nreverse new-local-fns))
    (setf fn-compile-list (nreverse fn-compile-list))
    ;; Phase 2: Compile each function body in scope of ALL labels names
    (let* ((*local-functions* (append new-local-fns *local-functions*))
           ;; Also make boxes available as locals for capture (only symbol names).
           ;; CL is a Lisp-2: function and variable namespaces are separate.
           ;; When a labels function name clashes with a lexical variable, use a
           ;; mangled name (__LABELFN_xxx) so both can coexist in *locals* without
           ;; the function box shadowing the variable binding.
           (new-locals (remove nil
                         (mapcar (lambda (lf)
                                   (let* ((name-str (first lf)))
                                     (unless (char= (char name-str 0) #\()
                                       (let* ((sym (intern name-str :dotcl.cil-compiler))
                                              ;; Check for name clash using string= (package-independent)
                                              (clash-p (assoc name-str *locals*
                                                              :key (lambda (k) (if (symbolp k) (symbol-name k) nil))
                                                              :test #'string=))
                                              (local-sym (if clash-p
                                                             (intern (concatenate 'string "__LABELFN_" name-str)
                                                                     :dotcl.cil-compiler)
                                                             sym)))
                                         (cons local-sym (second lf))))))
                                 new-local-fns)))
           (*locals* (append new-locals *locals*))
           (*boxed-vars* (append (mapcar #'car new-locals)
                                 *boxed-vars*))
           (store-instrs '()))
      ;; Compile each function and store into its box.
      ;; CL spec: labels creates an implicit block named after the function.
      ;; For (setf sym) names, use progn instead of block (block requires a symbol).
      ;; Pass name-str + set *tco-local-fn-key* so compile-lambda enables self-TCO (#125).
      (dolist (entry fn-compile-list)
        (let ((name (first entry))
              (params (second entry))
              (fn-body (third entry))
              (key (fourth entry)))
          (let* ((name-str (mangle-name name))
                 (lambda-instrs (let ((*tco-local-fn-key* key))
                                  (if (and (symbolp name)
                                           (some (lambda (f) (form-has-return-from-p name f)) fn-body))
                                      (compile-lambda params `((block ,name ,@fn-body)) name-str)
                                      (compile-lambda params fn-body name-str)))))
            (setf store-instrs
                  (append store-instrs
                          `((:ldloc ,key) (:ldc-i4 0)
                            ,@lambda-instrs
                            (:stelem-ref)))))))
      ;; Compile body
      `(,@box-instrs
        ,@store-instrs
        ,@(compile-progn body)))))

(defun compile-labels-build-new-locals (new-local-fns)
  "Shared helper: build the *locals* additions for a labels group (box-as-variable bindings)."
  (remove nil
    (mapcar (lambda (lf)
              (let* ((name-str (first lf)))
                (unless (char= (char name-str 0) #\()
                  (let* ((sym (intern name-str :dotcl.cil-compiler))
                         (clash-p (assoc name-str *locals*
                                         :key (lambda (k) (if (symbolp k) (symbol-name k) nil))
                                         :test #'string=))
                         (local-sym (if clash-p
                                        (intern (concatenate 'string "__LABELFN_" name-str)
                                                :dotcl.cil-compiler)
                                        sym)))
                    (cons local-sym (second lf))))))
            new-local-fns)))

(defun compile-labels-mutual-tco (fn-defs body arity)
  "Compile (labels ...) where all functions have the same required ARITY.
   Emits closures in boxes for non-tail calls, plus a dispatch loop for
   mutual tail call optimization (#124/D919).
   Dispatch loop structure:
     TCOLOOP: dispatch by WHICH_FN index → fn body sections
     Each fn body section: tail calls to siblings → br TCOLOOP; normal return → br TCOEND.
   The labels body is compiled with *labels-mutual-tco* active so its own
   tail calls to labels fns also enter the dispatch loop directly."
  ;; Phase 1: allocate boxes (for non-tail calls), same as compile-labels-boxed
  (let ((box-instrs '())
        (new-local-fns '())
        (fn-compile-list '()))
    (dolist (fdef fn-defs)
      (let* ((name (car fdef))
             (name-str (mangle-name name))
             (key (gen-local name-str)))
        (setf box-instrs
              (append box-instrs
                      `((:declare-local ,key "LispObject[]")
                        (:ldc-i4 1) (:newarr "LispObject")
                        (:stloc ,key))))
        (push (list name-str key t) new-local-fns)
        (push (list name (cadr fdef) (cddr fdef) key) fn-compile-list)))
    (setf new-local-fns (nreverse new-local-fns))
    (setf fn-compile-list (nreverse fn-compile-list))
    ;; Phase 2: compile closures into boxes; *labels-mutual-tco* is NIL here
    ;; (compile-function-body-* resets it) so closures don't emit br-to-outer-TCOLOOP
    (let* ((*local-functions* (append new-local-fns *local-functions*))
           (new-locals (compile-labels-build-new-locals new-local-fns))
           (*locals* (append new-locals *locals*))
           (*boxed-vars* (append (mapcar #'car new-locals) *boxed-vars*))
           (store-instrs '()))
      (dolist (entry fn-compile-list)
        (let ((name (first entry))
              (params (second entry))
              (fn-body (third entry))
              (key (fourth entry)))
          (let* ((name-str (mangle-name name))
                 (lambda-instrs (let ((*tco-local-fn-key* key))
                                  (if (and (symbolp name)
                                           (some (lambda (f) (form-has-return-from-p name f)) fn-body))
                                      (compile-lambda params `((block ,name ,@fn-body)) name-str)
                                      (compile-lambda params fn-body name-str)))))
            (setf store-instrs
                  (append store-instrs
                          `((:ldloc ,key) (:ldc-i4 0)
                            ,@lambda-instrs
                            (:stelem-ref)))))))
      ;; Phase 3: dispatch loop infrastructure
      ;; shared-param-keys: one LispObject local per param position, shared across all fn bodies
      ;; which-fn-key: System.Int32 local that selects which fn to dispatch to
      (let* ((shared-param-keys
               (loop for i from 0 below arity
                     collect (gen-local (format nil "LMARG~D" i))))
             (which-fn-key (gen-local "LMWFN"))
             (tcoloop-label (gen-label "LMTCO"))
             (tcoend-label (gen-label "LMEND"))
             ;; mtco-table: (name-str fn-index which-fn-key tcoloop-label shared-param-keys)
             (mtco-table
               (loop for fdef in fn-defs
                     for i from 0
                     collect (list (mangle-name (car fdef))
                                   i
                                   which-fn-key
                                   tcoloop-label
                                   shared-param-keys)))
             ;; Int32 type string recognized by CilAssembler
             (int32-type "Int32")
             ;; Per-fn CIL labels for dispatch targets
             (fn-labels
               (loop for i from 0 below (length fn-defs)
                     collect (gen-label (format nil "LMF~D" i)))))
        ;; Phase 4: compile labels body with *labels-mutual-tco* active
        ;; Tail calls to labels fns emit: store args, set which-fn, br tcoloop-label
        (let* ((*labels-mutual-tco* (append mtco-table *labels-mutual-tco*))
               (body-instrs (compile-progn body)))
          ;; Phase 5: dispatch instructions (beq for fns 0..N-2; fn N-1 falls through)
          (let ((dispatch-instrs
                  (loop for lbl in (butlast fn-labels)
                        for i from 0
                        append `((:ldloc ,which-fn-key)
                                 (:ldc-i4 ,i)
                                 (:beq ,lbl)))))
            ;; Phase 6: compile fn body sections inline (no compile-lambda wrapper)
            ;; Each section: fn's params bound to shared-param-keys, *labels-mutual-tco* active
            ;; Sections emitted in order: fn[N-1] (fall-through target), fn[0], ..., fn[N-2]
            (let* ((fn-body-sections
                     (loop for fdef in fn-defs
                           for lbl in fn-labels
                           collect
                           (let* ((params (cadr fdef))
                                  (fn-body (cddr fdef))
                                  (fn-instrs
                                    (let ((*locals*
                                            (append (loop for p in params
                                                          for key in shared-param-keys
                                                          collect (cons p key))
                                                    *locals*))
                                          ;; Reset outer self-TCO: dispatch bodies have their own context
                                          (*tco-self-name* nil)
                                          (*tco-self-symbol* nil)
                                          (*tco-loop-label* nil)
                                          (*tco-param-entries* nil)
                                          (*self-fn-local* nil)
                                          (*tco-local-fn-key* nil)
                                          ;; Dispatch body is always in tail position;
                                          ;; its result IS the result of the labels form
                                          (*in-tail-position* t)
                                          (*labels-mutual-tco* (append mtco-table *labels-mutual-tco*)))
                                      (let ((name (car fdef)))
                                        (if (and (symbolp name)
                                                 (some (lambda (f) (form-has-return-from-p name f))
                                                       fn-body))
                                            (compile-progn `((block ,name ,@fn-body)))
                                            (compile-progn fn-body))))))
                             (cons lbl fn-instrs))))
                   ;; Emit order: fn[N-1] first (fall-through from dispatch), then fn[0]..fn[N-2]
                   (sections-in-emit-order
                     (append (last fn-body-sections) (butlast fn-body-sections)))
                   ;; All section code: label + body + br to TCOEND
                   (all-section-code
                     (loop for (lbl . body-code) in sections-in-emit-order
                           append `((:label ,lbl)
                                    ,@body-code
                                    (:br ,tcoend-label)))))
              ;; Assemble complete instruction list
              `(,@(loop for key in shared-param-keys
                        collect `(:declare-local ,key "LispObject"))
                (:declare-local ,which-fn-key ,int32-type)
                ,@box-instrs
                ,@store-instrs
                ,@body-instrs
                (:br ,tcoend-label)
                (:label ,tcoloop-label)
                ,@dispatch-instrs
                ,@all-section-code
                (:label ,tcoend-label)))))))))

;;; ============================================================
;;; block / return-from
;;; ============================================================

(defun compile-block (name body)
  "Compile (block name body...).
   If no non-local return-from is used, skip the try/catch overhead entirely."
  (let* ((tag-key (gen-local "BTAG"))
         (tag-var-sym (intern (block-tag-var-name name) :dotcl.cil-compiler))
         (result-key (gen-local "BRES"))
         (end-label (gen-label "BEND"))
         (match-label (gen-label "BMATCH"))
         (ex-key (gen-local "BEX"))
         (needs-catch (list nil))
         ;; Entry format: (tag-key result-key end-label local-result-key local-end-label needs-catch)
         (*block-tags* (acons name (list tag-key result-key end-label result-key end-label needs-catch) *block-tags*))
         (*locals* (acons tag-var-sym tag-key *locals*))
         (body-instrs (let ((*in-tail-position* nil)
                            (*in-mv-context* t))
                        (compile-progn body))))
    (if (car needs-catch)
        ;; Non-local return detected: need try/catch
        `((:declare-local ,tag-key "LispObject")
          (:ldsfld "Nil.Instance") (:ldsfld "Nil.Instance") (:call "Runtime.MakeCons")
          (:stloc ,tag-key)
          (:declare-local ,result-key "LispObject")
          (:begin-exception-block)
          ,@body-instrs
          (:stloc ,result-key)
          (:leave ,end-label)
          (:begin-catch-block "BlockReturnException")
          (:declare-local ,ex-key "BlockReturnException")
          (:stloc ,ex-key)
          (:ldloc ,ex-key) (:callvirt "BlockReturnException.get_Tag")
          (:ldloc ,tag-key)
          (:beq ,match-label)
          (:rethrow)
          (:label ,match-label)
          (:ldloc ,ex-key) (:callvirt "BlockReturnException.get_Value")
          (:stloc ,result-key)
          (:leave ,end-label)
          (:end-exception-block)
          (:label ,end-label)
          (:ldloc ,result-key))
        ;; All returns are local: no try/catch needed
        `((:declare-local ,result-key "LispObject")
          ,@body-instrs
          (:stloc ,result-key)
          (:label ,end-label)
          (:ldloc ,result-key)))))

(defun compile-return-from (name value-expr)
  "Compile (return-from name value).
   Local return (same compilation unit, not in finally block) uses leave.
   Non-local return (from closure or finally block) throws BlockReturnException."
  (let ((entry (assoc name *block-tags*)))
    (unless entry (error "return-from: no block named ~s" name))
    (let ((tag-key (first (cdr entry)))
          (local-result-key (fourth (cdr entry)))
          (local-end-label (fifth (cdr entry))))
      (let ((needs-catch (sixth (cdr entry))))
        ;; value-expr's multiple values become the block's return values;
        ;; preserve MvReturn wrapping so the block's caller sees MV.
        (if (and local-result-key (not *in-finally-block*))
            ;; Local return: store result and leave to block end
            `(,@(if value-expr
                    (let ((*in-mv-context* t)) (compile-expr value-expr))
                    (emit-nil))
              (:stloc ,local-result-key)
              (:leave ,local-end-label))
            ;; Non-local return: throw BlockReturnException
            (progn
              (when needs-catch (setf (car needs-catch) t))
              (let ((val-key (gen-local "RVAL")))
                `((:declare-local ,val-key "LispObject")
                  ,@(if value-expr
                        (let ((*in-mv-context* t)) (compile-expr value-expr))
                        (emit-nil))
                  (:stloc ,val-key)
                  (:ldloc ,tag-key)
                  (:ldloc ,val-key)
                  (:newobj "BlockReturnException")
                  (:throw)))))))))

;;; ============================================================
;;; catch / throw
;;; ============================================================

(defun compile-catch (tag-expr body)
  "Compile (catch tag body...).
   Uses try/catch/finally for CatchThrowException with EQ tag matching.
   Pushes tag to CatchTagStack so (throw ...) inside (eval ...) can propagate
   correctly to an outer (catch ...) even across the eval boundary (D696)."
  (let ((tag-key (gen-local "CTAG"))
        (result-key (gen-local "CRES"))
        (end-label (gen-label "CEND"))
        (match-label (gen-label "CMATCH"))
        (ex-key (gen-local "CEX")))
    `((:declare-local ,tag-key "LispObject")
      ,@(compile-expr tag-expr)
      (:stloc ,tag-key)
      ;; Register tag so (throw ...) inside (eval ...) can propagate to this catch
      (:ldloc ,tag-key)
      (:call "CatchTagStack.Push")
      (:declare-local ,result-key "LispObject")
      (:begin-exception-block)
      ;; Body in MV-propagating position: catch returns body's values (CL spec).
      ,@(let ((*in-tail-position* nil) (*in-mv-context* t)) (compile-progn body))
      (:stloc ,result-key)
      (:leave ,end-label)
      (:begin-catch-block "CatchThrowException")
      (:declare-local ,ex-key "CatchThrowException")
      (:stloc ,ex-key)
      ;; Check tag identity (EQ)
      (:ldloc ,ex-key) (:callvirt "CatchThrowException.get_Tag")
      (:ldloc ,tag-key)
      (:beq ,match-label)
      (:rethrow)
      (:label ,match-label)
      (:ldloc ,ex-key) (:callvirt "CatchThrowException.get_Value")
      (:stloc ,result-key)
      (:leave ,end-label)
      (:begin-finally-block)
      (:call "CatchTagStack.Pop")
      (:end-exception-block)
      (:label ,end-label)
      (:ldloc ,result-key))))

(defun compile-throw (tag-expr result-expr)
  "Compile (throw tag result).
   Stores tag and result in locals before constructing the exception,
   so that non-local exits within result-expr don't leave tag on stack.
   Unmatched throws are caught at the Eval boundary and converted to CONTROL-ERROR."
  (let ((tag-key (gen-local "TTAG"))
        (res-key (gen-local "TRES")))
    `((:declare-local ,tag-key "LispObject")
      (:declare-local ,res-key "LispObject")
      ,@(compile-expr tag-expr)
      (:stloc ,tag-key)
      ,@(compile-expr result-expr)
      (:stloc ,res-key)
      (:ldloc ,tag-key) (:ldloc ,res-key)
      (:newobj "CatchThrowException")
      (:throw))))

;;; ============================================================
;;; tagbody / go
;;; ============================================================

(defun parse-tagbody-forms (forms)
  "Parse tagbody forms into segments: ((label . forms-list) ...).
   The first segment has label nil."
  (let ((segments '())
        (current-label nil)
        (current-forms '()))
    (dolist (form forms)
      (if (or (and (symbolp form) (not (null form))) (integerp form))
          (progn
            (push (cons current-label (reverse current-forms)) segments)
            (setf current-label form)
            (setf current-forms '()))
          (push form current-forms)))
    (push (cons current-label (reverse current-forms)) segments)
    (reverse segments)))

(defun compile-tagbody (forms)
  "Compile (tagbody forms...).
   Local go (same tagbody, not in closure) uses index + leave instead of exceptions.
   Non-local go (from closures) still uses GoException throw/catch."
  (let* ((segments (parse-tagbody-forms forms))
         (tb-id-key (gen-local "TBID"))
         (index-key (gen-local "TBIDX"))
         (done-key (gen-local "TBDONE"))
         (end-label (gen-label "TBEND"))
         (loop-label (gen-label "TBLOOP"))
         (leave-label (gen-label "TBLEAVE"))
         (match-label (gen-label "TBMATCH"))
         (ex-key (gen-local "TBEX"))
         ;; Build tag→index map (tags start from index 0)
         (tag-counter 0)
         (tag-indices
           (let ((result '()))
             (dolist (seg segments)
               (when (car seg)
                 (push (cons (car seg) tag-counter) result))
               (incf tag-counter))
             (reverse result)))
         ;; Segment labels
         (seg-labels (loop for i from 0 below (length segments)
                           collect (gen-label (format nil "SEG~d" i))))
         ;; Synthetic variable for the tagbody ID (for non-local go from closures)
         (tb-var-name (concatenate 'string "%TBID-" (symbol-name tb-id-key) "%"))
         (tb-var-sym (intern tb-var-name :dotcl.cil-compiler))
         (*locals* (acons tb-var-sym tb-id-key *locals*))
         ;; Extended format: (tag-name tb-var-name tb-id-key label-idx index-key leave-label)
         ;; 5th & 6th elements enable local go optimization
         (*go-tags* (append
                     (mapcar (lambda (ti) (list (car ti) tb-var-name tb-id-key (cdr ti)
                                                index-key loop-label))
                             tag-indices)
                     *go-tags*)))
    `((:declare-local ,tb-id-key "LispObject")
      ;; Use a unique Cons cell as the tagbody ID (LispObject, capturable by closures)
      (:ldsfld "Nil.Instance") (:ldsfld "Nil.Instance") (:call "Runtime.MakeCons")
      (:stloc ,tb-id-key)
      (:declare-local ,index-key "Int32")
      (:ldc-i4 0) (:stloc ,index-key)
      (:declare-local ,done-key "Boolean")
      (:ldc-i4 0) (:stloc ,done-key)
      ;; Outer loop
      (:label ,loop-label)
      (:ldloc ,done-key) (:brtrue ,end-label)
      (:begin-exception-block)
      ;; Switch on index
      (:ldloc ,index-key)
      (:switch ,seg-labels)
      (:br ,leave-label)
      ;; Segments (fall-through)
      ,@(let ((*in-tail-position* nil))
           (loop for seg in segments
                 for label in seg-labels
                 append `((:label ,label)
                          ,@(loop for form in (cdr seg)
                                  append (compile-and-pop form)))))
      ;; Normal completion
      (:label ,leave-label)
      (:ldc-i4 1) (:stloc ,done-key)
      (:leave ,loop-label)
      ;; Catch GoException (for non-local go from closures)
      (:begin-catch-block "GoException")
      (:declare-local ,ex-key "GoException")
      (:stloc ,ex-key)
      ;; Check tagbody identity
      (:ldloc ,ex-key) (:callvirt "GoException.get_TagbodyId")
      (:ldloc ,tb-id-key)
      (:beq ,match-label)
      (:rethrow)
      (:label ,match-label)
      (:ldloc ,ex-key) (:callvirt "GoException.get_TargetLabel")
      (:stloc ,index-key)
      (:leave ,loop-label)
      (:end-exception-block)
      (:br ,loop-label)
      ;; End: tagbody always returns NIL as single value
      (:label ,end-label)
      (:call "MultipleValues.Reset")  ; tagbody return is always single NIL
      ,@(emit-nil))))

(defun compile-go (tag)
  "Compile (go tag).
   Local go (5th element in *go-tags* entry present) sets index + leave.
   Non-local go (from closures, no 5th element) uses GoException throw."
  (let ((entry (assoc tag *go-tags*)))
    (unless entry (error "go: no tagbody tag named ~s" tag))
    ;; Format: (tag-name tb-var-name tb-id-key label-idx [index-key loop-label])
    (let ((tb-id-key (third entry))
          (label-idx (fourth entry))
          (local-index-key (fifth entry))
          (local-loop-label (sixth entry)))
      (if (and local-index-key (not *in-finally-block*))
          ;; Local go: set index and leave try block — no exception
          `((:ldc-i4 ,label-idx)
            (:stloc ,local-index-key)
            (:leave ,local-loop-label))
          ;; Non-local go (or go from finally block): throw GoException
          `((:ldloc ,tb-id-key)
            (:ldc-i4 ,label-idx)
            (:newobj "GoException")
            (:throw))))))

;;; ============================================================
;;; unwind-protect
;;; ============================================================

(defun compile-unwind-protect (protected-form cleanup-forms)
  "Compile (unwind-protect protected cleanup...).
   Saves/restores MultipleValues so body's secondary values are preserved
   (e.g. (progv ... (values x y z))). The restore is done AFTER the exception
   block (not in finally) so it only runs on normal return, not during
   non-local exits (return-from, throw)."
  (let ((result-key (gen-local "UPRES"))
        (mv-count-key (gen-local "UPMVC"))
        (mv-vals-key (gen-local "UPMVV"))
        (normal-label (gen-label "UPNORM")))
    `((:declare-local ,result-key "LispObject")
      (:declare-local ,mv-count-key "Int32")
      (:declare-local ,mv-vals-key "LispObject[]")
      ,@(emit-nil) (:stloc ,result-key)
      (:begin-exception-block)
      ;; Protected form in MV-propagating position: unwind-protect returns body's values.
      ;; *in-tail-position* nil (TCO illegal across try boundary).
      ,@(let ((*in-tail-position* nil) (*in-mv-context* t)) (compile-expr protected-form))
      ;; If protected-form did a non-local exit (leave/throw),
      ;; the stloc below is unreachable. Use a label so dead-code
      ;; elimination in the assembler can handle it cleanly.
      (:label ,normal-label)
      (:stloc ,result-key)
      ;; Save MultipleValues state before cleanup runs
      (:call "MultipleValues.SaveCount") (:stloc ,mv-count-key)
      (:call "MultipleValues.SaveValues") (:stloc ,mv-vals-key)
      (:begin-finally-block)
      ,@(let ((*in-tail-position* nil)
              (*in-finally-block* t))
           (loop for form in cleanup-forms
                 append (compile-and-pop form)))
      (:end-exception-block)
      ;; Restore MultipleValues AFTER exception block (only on normal return)
      (:ldloc ,mv-count-key) (:ldloc ,mv-vals-key)
      (:call "MultipleValues.RestoreSaved")
      (:ldloc ,result-key))))

;;; ============================================================
;;; handler-case
;;; ============================================================

(defun compile-handler-case (body-form clauses)
  "Compile (handler-case body (type1 (var) handler1...) (type2 () handler2...) ...).
   Uses HandlerClusterStack so handler-case takes priority over enclosing handler-bind
   handlers (innermost wins, per CL spec).

   Key CIL constraint: brfalse/brtrue inside a catch block can only branch WITHIN that
   catch block; use 'leave' to exit the catch block to a label in an enclosing try."
  (let* ((result-key (gen-local "HCRES"))
         (outer-end-label (gen-label "HCOUTEREND"))
         (inner-end-label (gen-label "HCINNEREND"))
         (hc-tag-key (gen-local "HCTAG"))
         (cond-key (gen-local "HCCOND"))
         (hcex-key (gen-local "HCINVEX"))
         (ex-key (gen-local "HCEX"))
         (dotnet-ex-key (gen-local "HCDOTNETEX"))
         (ci-key (gen-local "HCCIKEY"))
         (nomatch-hcex-label (gen-label "HCNOMATCHHCEX"))
         ;; Parse clauses: each is (type-name var handler-body)
         (parsed (mapcar (lambda (clause)
                           (let* ((type-spec (car clause))
                                  (type-name (cond ((symbolp type-spec) (symbol-name type-spec))
                                                   ((and (typep type-spec 'standard-object)
                                                         (class-name type-spec))
                                                    (symbol-name (class-name type-spec)))
                                                   (t (format nil "~A" type-spec))))
                                  (lambda-list (cadr clause))
                                  (var (if (and lambda-list (car lambda-list))
                                           (car lambda-list)
                                           nil))
                                  (handler-body (cddr clause)))
                             (list type-name var handler-body)))
                         clauses))
         (n (length parsed))
         ;; Generate unified clause body labels (shared across all catch dispatches)
         (clause-labels (loop for i from 0 below n
                              collect (gen-label (format nil "HCCLAUSE~d" i))))
         ;; Skip labels for ceq+brfalse dispatch (must stay WITHIN each catch block)
         (ci-skip-labels (loop for i from 0 below n
                               collect (gen-label (format nil "HCCISKIP~d" i))))
         (le-skip-labels (loop for i from 0 below n
                               collect (gen-label (format nil "HCLESKIP~d" i))))
         (dn-skip-labels (loop for i from 0 below n
                               collect (gen-label (format nil "HCDNSKIP~d" i))))
         (dn-rethrow-label (gen-label "HCDNRETHROW")))
    `(;; Create unique tag for this handler-case instance
      (:declare-local ,hc-tag-key "Object")
      (:newobj "Object") (:stloc ,hc-tag-key)
      ;; Shared condition local (set before dispatching to clause body)
      (:declare-local ,cond-key "LispObject")
      ,@(emit-nil) (:stloc ,cond-key)
      ;; clauseIndex local for HandlerCaseInvocationException dispatch
      (:declare-local ,ci-key "Int32")
      ;; Build HandlerBinding[] for our handler-case cluster
      (:ldc-i4 ,n)
      (:newarr "HandlerBinding")
      ,@(loop for (type-name var handler-body) in parsed
              for i from 0
              append `((:dup) (:ldc-i4 ,i)
                        ;; Type specifier symbol
                        (:ldstr ,type-name) (:call "Startup.Sym")
                        ;; Handler function: MakeHandlerCaseFunction(tag, clauseIndex)
                        (:ldloc ,hc-tag-key)
                        (:ldc-i4 ,i)
                        (:call "Startup.MakeHandlerCaseFunction")
                        (:newobj "HandlerBinding")
                        (:stelem-ref)))
      (:call "HandlerClusterStack.PushCluster")
      ;; Result local
      (:declare-local ,result-key "LispObject")
      ,@(emit-nil) (:stloc ,result-key)
      ;; try-catch: body + exception dispatch
      ;; PopCluster is done explicitly before handler body or on normal exit,
      ;; NOT in a finally block. This ensures handler bodies run with the
      ;; handler-case cluster already removed (per CL spec: handlers are
      ;; executed after unwinding, outside the handler's dynamic scope).
      (:begin-exception-block)
      ;; Body in MV-propagating position: handler-case returns body's values (CL spec).
      ;; Self-TCO inside handler-case: use `leave` to exit the try block and prepend
      ;; PopCluster so each iteration has a clean handler stack (#126).
      ,@(let ((*in-try-block* t)         ; protect: :ret invalid in try/catch region
              (*tco-in-try-catch* (if *tco-self-name* t *tco-in-try-catch*))
              (*in-mv-context* t)
              ;; When TCO is active, prepend PopCluster to *tco-leave-instrs* so the
              ;; self-call emits PopCluster before `leave TCOLOOP` (#126).
              (*tco-leave-instrs*
               (if *tco-self-name*
                   (cons '(:call "HandlerClusterStack.PopCluster") *tco-leave-instrs*)
                   *tco-leave-instrs*)))
          (compile-expr body-form))
      (:stloc ,result-key)
      (:leave ,inner-end-label)
      ;; Catch 1: HandlerCaseInvocationException (main path via HandlerClusterStack.Signal)
      ;; NOTE: brfalse/brtrue inside a catch block can only branch WITHIN the catch block.
      ;; Use 'leave' to exit the catch block to labels after the try-catch.
      (:begin-catch-block "HandlerCaseInvocationException")
      (:declare-local ,hcex-key "HandlerCaseInvocationException")
      (:stloc ,hcex-key)
      ;; Check tag — brfalse to nomatch-hcex-label (within this catch block)
      (:ldloc ,hcex-key) (:callvirt "HandlerCaseInvocationException.get_Tag")
      (:ldloc ,hc-tag-key) (:ceq)
      (:brfalse ,nomatch-hcex-label) ;; within catch
      ;; Tag matches: extract condition and dispatch by clauseIndex
      (:ldloc ,hcex-key) (:callvirt "HandlerCaseInvocationException.get_Condition")
      (:stloc ,cond-key)
      (:ldloc ,hcex-key) (:callvirt "HandlerCaseInvocationException.get_ClauseIndex")
      (:stloc ,ci-key)
      ;; For each clause: check ci == i, if so leave to clause body (after try-catch)
      ,@(loop for label in clause-labels
              for ci-skip in ci-skip-labels
              for i from 0
              append `((:ldloc ,ci-key) (:ldc-i4 ,i) (:ceq) (:brfalse ,ci-skip) ;; within catch
                       (:leave ,label) ;; exit catch to clause body after try-catch
                       (:label ,ci-skip))) ;; within catch
      ;; Out-of-range clauseIndex falls through to nomatch
      (:label ,nomatch-hcex-label) ;; reached by: tag mismatch OR out-of-range index
      (:call "HandlerClusterStack.PopCluster")
      (:rethrow)
      ;; Catch 2: LispErrorException (fallback when Signal didn't find our cluster,
      ;;          or for LispErrors whose type doesn't match any clause)
      (:begin-catch-block "LispErrorException")
      (:declare-local ,ex-key "LispErrorException")
      (:stloc ,ex-key)
      (:ldloc ,ex-key) (:callvirt "LispErrorException.get_Condition")
      (:castclass "LispObject") (:stloc ,cond-key)
      ,@(loop for (type-name var handler-body) in parsed
              for label in clause-labels
              for skip in le-skip-labels
              append `((:ldloc ,cond-key)
                       (:ldstr ,type-name) (:call "Startup.Sym")
                       (:call "Runtime.Typep") (:call "Runtime.IsTruthy")
                       (:brfalse ,skip) ;; within catch: skip to next type check
                       (:leave ,label)  ;; exit catch to clause body after try-catch
                       (:label ,skip))) ;; within catch
      (:call "HandlerClusterStack.PopCluster")
      (:rethrow)
      ;; Catch 3: System.Exception (raw .NET exceptions not yet wrapped)
      (:begin-catch-block "System.Exception")
      (:declare-local ,dotnet-ex-key "System.Exception")
      (:stloc ,dotnet-ex-key)
      ;; Re-throw Lisp control-flow exceptions (RETURN-FROM, THROW, GO, etc.)
      ;; so they are not mistakenly caught as errors.
      (:ldloc ,dotnet-ex-key) (:call "Runtime.IsLispControlFlowException")
      (:brtrue ,dn-rethrow-label)  ;; brtrue within catch to rethrow path
      (:ldloc ,dotnet-ex-key) (:callvirt "System.Exception.get_Message")
      (:call "Runtime.WrapDotNetException")
      (:stloc ,cond-key)
      ,@(loop for (type-name var handler-body) in parsed
              for label in clause-labels
              for skip in dn-skip-labels
              append `((:ldloc ,cond-key)
                       (:ldstr ,type-name) (:call "Startup.Sym")
                       (:call "Runtime.Typep") (:call "Runtime.IsTruthy")
                       (:brfalse ,skip) ;; within catch
                       (:leave ,label)  ;; exit catch to clause body
                       (:label ,skip))) ;; within catch
      (:label ,dn-rethrow-label)
      (:call "HandlerClusterStack.PopCluster")
      (:rethrow)
      (:end-exception-block) ;; end try-catch
      ;; Normal exit: PopCluster and jump to end
      (:label ,inner-end-label)
      (:call "HandlerClusterStack.PopCluster")
      (:br ,outer-end-label)
      ;; Clause bodies (AFTER try-catch, outside exception block)
      ;; PopCluster before executing handler body per CL spec.
      ,@(loop for (type-name var handler-body) in parsed
              for label in clause-labels
              append (multiple-value-bind (declared-specials real-body)
                         (extract-specials handler-body)
                       (let* ((var-is-special (and var
                                                   (or (member var declared-specials)
                                                       (global-special-p var))))
                              (*specials* (append declared-specials *specials*))
                              (*locals* (if (and var (not var-is-special))
                                            (let ((var-key (gen-local "HCV")))
                                              (acons var var-key *locals*))
                                            *locals*)))
                         (let ((var-key (if (and var (not var-is-special))
                                            (lookup-local var) nil)))
                           `((:label ,label)
                             (:call "HandlerClusterStack.PopCluster")
                             ,@(cond
                                 (var-is-special
                                  ;; Bind as special (dynamic) variable with try/finally.
                                  ;; Set *in-try-block* before compiling body so TCO hooks
                                  ;; emit `leave` instead of `br` (mirrors compile-let, D920).
                                  (let ((tmp-key (gen-local "HCTMP")))
                                    `((:declare-local ,tmp-key "LispObject")
                                      (:ldloc ,cond-key) (:stloc ,tmp-key)
                                      ,@(compile-let-with-specials
                                         '()
                                         `(,@(compile-sym-lookup var)
                                           (:castclass "Symbol")
                                           (:ldloc ,tmp-key)
                                           (:call "DynamicBindings.Push"))
                                         (let ((*in-try-block* (or *in-try-block* (list var))))
                                           (compile-progn real-body))
                                         (list var)))))
                                 (var
                                  `((:declare-local ,var-key "LispObject")
                                    (:ldloc ,cond-key) (:stloc ,var-key)
                                    ,@(compile-progn real-body)))
                                 (t (compile-progn real-body)))
                             (:stloc ,result-key)
                             (:br ,outer-end-label))))))
      (:label ,outer-end-label)
      (:ldloc ,result-key))))

;;; ============================================================
;;; restart-case
;;; ============================================================

(defun compile-restart-case (body-form clauses)
  "Compile (restart-case body (name (params...) body...) ...).
   Uses RestartClusterStack + RestartInvocationException matching on tag.
   Handler bodies execute AFTER try-catch with PopCluster first,
   so they run outside the restart-case's dynamic scope."
  (let* ((is-signaling-body (labels ((check-signaling (form)
                                        (cond ((not (consp form)) nil)
                                              ((member (car form) '(error signal cerror warn)) t)
                                              ;; Macro form: expand and check (CLHS 9.2.2.5)
                                              ((and (symbolp (car form))
                                                    (find-macro-expander (car form))
                                                    (not (assoc (symbol-name (car form)) *local-functions* :test #'string=)))
                                               (check-signaling (cached-macroexpand form (find-macro-expander (car form)))))
                                              (t nil))))
                                (or (check-signaling body-form)
                                    ;; Also check symbol-macrolet expansion
                                    (and (symbolp body-form)
                                         (lookup-symbol-macro body-form)
                                         (check-signaling (lookup-symbol-macro body-form))))))
         (result-key (gen-local "RCRES"))
         (try-end-label (gen-label "RCTRYEND"))
         (done-label (gen-label "RCDONE"))
         (ex-key (gen-local "RCEX"))
         (args-key (gen-local "RCARGS"))
         ;; Parse clauses: ((name params body...) ...)
         ;; Extract :report, :interactive, :test keyword options before body
         (parsed (mapcar (lambda (clause)
                           (let ((name (symbol-name (car clause)))
                                 (params (cadr clause))
                                 (rest (cddr clause))
                                 (report nil)
                                 (interactive nil)
                                 (test-fn nil))
                             ;; Parse keyword options from the beginning of rest
                             (loop while rest
                                   do (cond ((eq (car rest) :report)
                                             (setq report (cadr rest))
                                             (setq rest (cddr rest)))
                                            ((eq (car rest) :interactive)
                                             (setq interactive (cadr rest))
                                             (setq rest (cddr rest)))
                                            ((eq (car rest) :test)
                                             (setq test-fn (cadr rest))
                                             (setq rest (cddr rest)))
                                            (t (return))))
                             (list name params rest report interactive (car clause) test-fn)))
                         clauses))
         ;; Generate a unique tag object for each restart
         (tag-keys (loop for i from 0 below (length parsed)
                         collect (gen-local (format nil "RCTAG~d" i))))
         ;; Generate labels for each clause
         (clause-labels (loop for i from 0 below (length parsed)
                              collect (gen-label (format nil "RC~d" i)))))
    `((:declare-local ,result-key "LispObject")
      ,@(emit-nil) (:stloc ,result-key)
      (:declare-local ,args-key "LispObject[]")
      ;; Create tag objects for each restart
      ,@(loop for tk in tag-keys
              append `((:declare-local ,tk "Object")
                       (:newobj "Object") (:stloc ,tk)))
      ;; Build LispRestart[] array and push cluster
      (:ldc-i4 ,(length parsed))
      (:newarr "LispRestart")
      ,@(loop for (name params handler-body report interactive name-sym test-fn) in parsed
              for tk in tag-keys
              for i from 0
              append `((:dup) (:ldc-i4 ,i)
                       (:ldstr ,name)   ;; restart name
                       (:ldnull)        ;; handler (unused, dispatch is via tag)
                       ,@(if (stringp report)
                             `((:ldstr ,report))
                             `((:ldnull)))  ;; description
                       (:ldloc ,tk)     ;; tag
                       (:ldc-i4 0)      ;; isBindRestart = false
                       (:newobj "LispRestart")
                       ;; Set NameSymbol to original symbol
                       ,@(when name-sym
                           `((:dup)
                             ,@(compile-expr (list 'quote name-sym))
                             (:castclass "Symbol")
                             (:callvirt "LispRestart.set_NameSymbol")))
                       ;; Set ReportFunction if report is a lambda/function form
                       ,@(if (and report (not (stringp report)))
                             `((:dup)
                               ,@(compile-expr report)
                               (:callvirt "LispRestart.set_ReportFunction"))
                             nil)
                       ;; Set InteractiveFunction if provided
                       ;; CLHS: :interactive takes a function designator (symbol → #'name)
                       ,@(if interactive
                             `((:dup)
                               ,@(compile-expr (if (symbolp interactive)
                                                   `(function ,interactive)
                                                   interactive))
                               (:callvirt "LispRestart.set_InteractiveFunction"))
                             nil)
                       ;; Set TestFunction if provided (symbol → #'name)
                       ,@(if test-fn
                             `((:dup)
                               ,@(compile-expr (if (symbolp test-fn)
                                                   `(function ,test-fn)
                                                   test-fn))
                               (:callvirt "LispRestart.set_TestFunction"))
                             nil)
                       (:stelem-ref)))
      (:call "RestartClusterStack.PushCluster")
      ;; try-catch for body + RestartInvocationException
      (:begin-exception-block)
      ;; Body in MV-propagating position: restart-case returns body's values.
      ,@(let ((*in-tail-position* nil) (*in-mv-context* t))
           (compile-expr (if is-signaling-body
                             `(let ((%rc-restarts (%top-cluster-restarts)))
                                (handler-bind ((condition
                                               (lambda (%rc-cond)
                                                 (%associate-condition-restarts
                                                  %rc-cond
                                                  %rc-restarts))))
                                  ,body-form))
                             body-form)))
      (:stloc ,result-key)
      (:leave ,try-end-label)
      ;; Catch RestartInvocationException
      (:begin-catch-block "RestartInvocationException")
      (:declare-local ,ex-key "RestartInvocationException")
      (:stloc ,ex-key)
      ;; Save arguments array
      (:ldloc ,ex-key)
      (:callvirt "RestartInvocationException.get_Arguments")
      (:stloc ,args-key)
      ;; Match tag to find which restart was invoked
      ;; ceq+brfalse to skip, leave to exit catch to clause-label
      ,@(let ((skip-labels (loop for i from 0 below (length parsed)
                                 collect (gen-label (format nil "RCSKIP~d" i)))))
          (loop for (name params handler-body report interactive name-sym test-fn) in parsed
                for tk in tag-keys
                for label in clause-labels
                for skip in skip-labels
                append `((:ldloc ,ex-key)
                         (:callvirt "RestartInvocationException.get_Tag")
                         (:ldloc ,tk)
                         (:ceq)
                         (:brfalse ,skip)  ;; skip if not matching (within catch)
                         (:leave ,label)   ;; exit catch to clause body
                         (:label ,skip)))) ;; within catch
      ;; No match → PopCluster + rethrow
      (:call "RestartClusterStack.PopCluster")
      (:rethrow)
      (:end-exception-block)
      ;; Normal exit: PopCluster and jump to done
      (:label ,try-end-label)
      (:call "RestartClusterStack.PopCluster")
      (:br ,done-label)
      ;; Clause bodies (AFTER try-catch, with PopCluster before handler body)
      ,@(loop for (name params handler-body report interactive name-sym test-fn) in parsed
              for label in clause-labels
              append (if (and params (car params))
                         ;; Has parameters: bind args via Runtime.RestartArg
                         (let ((*locals* *locals*)
                               (param-bindings nil))
                           ;; Process declare forms at start of handler-body
                           (let ((effective-body handler-body))
                             (loop while (and effective-body
                                             (consp (car effective-body))
                                             (eq (caar effective-body) 'declare))
                                   do (pop effective-body))
                             ;; Parse lambda list: split into positional, optional, rest, key, aux
                             (let ((positional nil)
                                   (optional nil)
                                   (rest-var nil)
                                   (key-params nil)
                                   (aux-params nil)
                                   (mode :positional))
                               (dolist (p params)
                                 (cond ((eq p '&rest)  (setq mode :rest))
                                       ((eq p '&optional) (setq mode :optional))
                                       ((eq p '&key) (setq mode :key))
                                       ((eq p '&aux) (setq mode :aux))
                                       ((eq p '&allow-other-keys) nil)
                                       ((eq mode :positional) (push p positional))
                                       ((eq mode :optional) (push p optional))
                                       ((eq mode :rest) (setq rest-var p))
                                       ((eq mode :key) (push p key-params))
                                       ((eq mode :aux) (push p aux-params))))
                               (setq positional (nreverse positional))
                               (setq optional (nreverse optional))
                               (setq key-params (nreverse key-params))
                               (setq aux-params (nreverse aux-params))
                               ;; Bind positional params
                               (let ((idx 0))
                                 (dolist (var positional)
                                   (let ((var-key (gen-local "RCV")))
                                     (setf *locals* (acons var var-key *locals*))
                                     (push `((:declare-local ,var-key "LispObject")
                                             (:ldloc ,args-key)
                                             (:ldc-i4 ,idx)
                                             (:call "Runtime.RestartArg")
                                             (:stloc ,var-key))
                                           param-bindings))
                                   (incf idx))
                                 ;; Bind optional params (RestartArg returns NIL for out-of-bounds)
                                 (dolist (var optional)
                                   (let* ((var-name (if (consp var) (car var) var))
                                          (var-key (gen-local "RCV")))
                                     (setf *locals* (acons var-name var-key *locals*))
                                     (push `((:declare-local ,var-key "LispObject")
                                             (:ldloc ,args-key)
                                             (:ldc-i4 ,idx)
                                             (:call "Runtime.RestartArg")
                                             (:stloc ,var-key))
                                           param-bindings))
                                   (incf idx))
                                 ;; Bind &rest param
                                 (when rest-var
                                   (let ((var-key (gen-local "RCV")))
                                     (setf *locals* (acons rest-var var-key *locals*))
                                     (push `((:declare-local ,var-key "LispObject")
                                             (:ldloc ,args-key)
                                             (:ldc-i4 ,idx)
                                             (:call "Runtime.RestartArgsAsList")
                                             (:stloc ,var-key))
                                           param-bindings)))
                                 ;; Bind &key params via Runtime.RestartKeyArg
                                 (dolist (kp key-params)
                                   (let* ((var-name (if (consp kp) (car kp) kp))
                                          (keyword-name (intern (symbol-name var-name) "KEYWORD"))
                                          (var-key (gen-local "RCV")))
                                     (setf *locals* (acons var-name var-key *locals*))
                                     (push `((:declare-local ,var-key "LispObject")
                                             (:ldloc ,args-key)
                                             ,@(compile-expr (list 'quote keyword-name))
                                             (:ldc-i4 ,idx)
                                             (:call "Runtime.RestartKeyArg")
                                             (:stloc ,var-key))
                                           param-bindings)))
                                 ;; Bind &aux params via compile-expr of init form
                                 (dolist (ap aux-params)
                                   (let* ((var-name (if (consp ap) (car ap) ap))
                                          (init-form (if (consp ap) (cadr ap) nil))
                                          (var-key (gen-local "RCV")))
                                     (setf *locals* (acons var-name var-key *locals*))
                                     (push `((:declare-local ,var-key "LispObject")
                                             ,@(compile-expr init-form)
                                             (:stloc ,var-key))
                                           param-bindings)))))
                             `((:label ,label)
                               (:call "RestartClusterStack.PopCluster")
                               ,@(apply #'append (nreverse param-bindings))
                               ,@(compile-progn effective-body)
                               (:stloc ,result-key)
                               (:br ,done-label))))
                         ;; No parameter
                         `((:label ,label)
                           (:call "RestartClusterStack.PopCluster")
                           ,@(compile-progn handler-body)
                           (:stloc ,result-key)
                           (:br ,done-label))))
      (:label ,done-label)
      (:ldloc ,result-key))))

;;; ============================================================
;;; restart-bind
;;; ============================================================

(defun compile-restart-bind (bindings body)
  "Compile (restart-bind ((name function &key ...) ...) body...).
   Creates LispRestart[] with isBindRestart=true, pushes cluster,
   body in try/finally with PopCluster. Unlike restart-case, there
   is no catch block - invoke-restart calls the handler directly."
  (let* ((n (length bindings))
         (result-key (gen-local "RBRES")))
    `(;; Build LispRestart[] array
      (:ldc-i4 ,n)
      (:newarr "LispRestart")
      ,@(loop for binding in bindings
              for i from 0
              append (let* ((name (car binding))
                            (fn-form (cadr binding))
                            (name-str (if name (symbol-name name) "NIL"))
                            (rest-args (cddr binding))
                            (test-fn nil)
                            (report-fn nil)
                            (interactive-fn nil))
                       ;; Parse keyword args
                       (loop while rest-args
                             do (cond ((eq (car rest-args) :test-function)
                                       (setq test-fn (cadr rest-args))
                                       (setq rest-args (cddr rest-args)))
                                      ((eq (car rest-args) :report-function)
                                       (setq report-fn (cadr rest-args))
                                       (setq rest-args (cddr rest-args)))
                                      ((eq (car rest-args) :interactive-function)
                                       (setq interactive-fn (cadr rest-args))
                                       (setq rest-args (cddr rest-args)))
                                      (t (setq rest-args (cddr rest-args)))))
                       `((:dup) (:ldc-i4 ,i)
                         (:ldstr ,name-str)         ;; restart name
                         ;; handler function: compile the function form
                         ,@(compile-expr fn-form)
                         (:castclass "LispFunction")
                         (:callvirt "LispFunction.get_RawFunction")
                         (:ldnull)                  ;; description
                         (:ldnull)                  ;; tag (unused for bind restarts)
                         (:ldc-i4 1)                ;; isBindRestart = true
                         (:newobj "LispRestart")
                         ;; Set NameSymbol
                         ,@(when name
                             `((:dup)
                               ,@(compile-expr (list 'quote name))
                               (:castclass "Symbol")
                               (:callvirt "LispRestart.set_NameSymbol")))
                         ;; Set TestFunction if provided
                         ,@(when test-fn
                             `((:dup)
                               ,@(compile-expr test-fn)
                               (:callvirt "LispRestart.set_TestFunction")))
                         ;; Set ReportFunction if provided
                         ,@(when report-fn
                             `((:dup)
                               ,@(compile-expr report-fn)
                               (:callvirt "LispRestart.set_ReportFunction")))
                         ;; Set InteractiveFunction if provided
                         ,@(when interactive-fn
                             `((:dup)
                               ,@(compile-expr interactive-fn)
                               (:callvirt "LispRestart.set_InteractiveFunction")))
                         (:stelem-ref))))
      ;; PushCluster
      (:call "RestartClusterStack.PushCluster")
      ;; try/finally to ensure PopCluster
      (:declare-local ,result-key "LispObject")
      ,@(emit-nil) (:stloc ,result-key)
      (:begin-exception-block)
      ;; Body in MV-propagating position: restart-bind returns body's values.
      ,@(let ((*in-tail-position* nil) (*in-mv-context* t)) (compile-progn body))
      (:stloc ,result-key)
      (:begin-finally-block)
      (:call "RestartClusterStack.PopCluster")
      (:end-exception-block)
      (:ldloc ,result-key))))

;;; ============================================================
;;; handler-bind
;;; ============================================================

(defun compile-handler-bind (bindings body)
  "Compile (handler-bind ((type handler-fn) ...) body...).
   Creates HandlerBinding[] array, pushes cluster, body in try/finally with PopCluster."
  (let* ((n (length bindings))
         (result-key (gen-local "HBRES")))
    `(;; Build HandlerBinding[] array
      (:ldc-i4 ,n)
      (:newarr "HandlerBinding")
      ,@(loop for binding in bindings
              for i from 0
              append (let ((type-spec (car binding))
                           (handler-form (cadr binding)))
                       `((:dup) (:ldc-i4 ,i)
                         ;; type specifier as symbol
                         ,@(compile-quoted type-spec)
                         ;; handler function
                         ,@(compile-expr handler-form)
                         (:call "Runtime.CoerceToFunction")
                         (:newobj "HandlerBinding")
                         (:stelem-ref))))
      ;; PushCluster
      (:call "HandlerClusterStack.PushCluster")
      ;; try { try { body } catch(System.Exception) { wrap+signal+rethrow } } finally { PopCluster }
      (:declare-local ,result-key "LispObject")
      ,@(emit-nil) (:stloc ,result-key)
      ,@(let ((inner-end-label (gen-label "HBINNER")))
          `((:begin-exception-block)  ;; outer try (for finally)
            (:begin-exception-block)  ;; inner try (for catch)
            ;; Body in MV-propagating position: handler-bind returns body's values.
            ,@(let ((*in-tail-position* nil) (*in-mv-context* t)) (compile-progn body))
            (:stloc ,result-key)
            (:leave ,inner-end-label)
            ;; Catch raw .NET exceptions: wrap as LispCondition, signal through handlers,
            ;; then throw as LispErrorException (so handler-bind handlers can
            ;; do non-local exits via return-from/throw).
            ;; Lisp control exceptions (BlockReturn, CatchThrow, Go, etc.) are rethrown.
            (:begin-catch-block "System.Exception")
            (:call "Runtime.RewrapNonLispException")
            (:end-exception-block)  ;; end inner try-catch
            (:label ,inner-end-label)
            (:begin-finally-block)
            (:call "HandlerClusterStack.PopCluster")
            (:end-exception-block)  ;; end outer try-finally
            (:ldloc ,result-key))))))

;;; ============================================================
;;; and / or / cond
;;; ============================================================

(defun compile-and (args)
  "Compile (and a b c) with short-circuit evaluation."
  (cond
    ((null args) (emit-t))
    ((null (cdr args)) (compile-expr (car args)))
    (t (let ((end-label (gen-label "ANDEND"))
             (result-key (gen-local "AND")))
         `((:declare-local ,result-key "LispObject")
           ,@(loop for (arg . rest) on args
                   if rest
                     ;; Intermediate arg: normalize to primary value for IsTruthy check
                     append `(,@(compile-expr arg)
                              (:call "MultipleValues.Primary")
                              (:stloc ,result-key)
                              (:ldloc ,result-key)
                              (:call "Runtime.IsTruthy")
                              (:brfalse ,end-label))
                   else
                     ;; Last arg: pass through all values
                     append `(,@(compile-expr arg)
                              (:stloc ,result-key)))
           (:label ,end-label)
           (:ldloc ,result-key))))))

(defun compile-or (args)
  "Compile (or a b c) with short-circuit evaluation."
  (cond
    ((null args) (emit-nil))
    ((null (cdr args)) (compile-expr (car args)))
    (t (let ((end-label (gen-label "OREND"))
             (result-key (gen-local "OR")))
         `((:declare-local ,result-key "LispObject")
           ,@(loop for (arg . rest) on args
                   if rest
                     ;; Intermediate arg: normalize to primary value for IsTruthy check
                     append `(,@(compile-expr arg)
                              (:call "MultipleValues.Primary")
                              (:stloc ,result-key)
                              (:ldloc ,result-key)
                              (:call "Runtime.IsTruthy")
                              (:brtrue ,end-label))
                   else
                     ;; Last arg: pass through all values
                     append `(,@(compile-expr arg)
                              (:stloc ,result-key)))
           (:label ,end-label)
           (:ldloc ,result-key))))))

(defun compile-cond (clauses &optional shared-tmp)
  "Compile (cond (test1 body1...) ...) to nested conditional.
   shared-tmp: shared LispObject local for no-body arms (#114)."
  (if (null clauses)
      (emit-nil)
      (let* ((clause (car clauses))
             (test (car clause))
             (body (cdr clause)))
        (if (eq test t)
            ;; Default clause
            (if body (compile-progn body) (emit-t))
            ;; Normal clause
            (let ((else-label (gen-label "CELSE"))
                  (end-label (gen-label "CEND")))
              (let ((fused (compile-if-fused-comparison-p test)))
                (if body
                    ;; With body: evaluate test, if true evaluate body
                    (cond
                      (fused
                        ;; Fused comparison: skip IsTruthy
                        `(,@(let ((*in-tail-position* nil))
                              (ecase (first fused)
                                (:binary (compile-binary-call (third fused) (second fused)))
                                (:unary (compile-unary-call (third fused) (second fused)))
                                (:fixnum-cmp (compile-fixnum-cmp (third fused) (second fused)))
               (:double-cmp (compile-double-cmp (third fused) (second fused)))))
                          (:brfalse ,else-label)
                          ,@(compile-progn body)
                          (:br ,end-label)
                          (:label ,else-label)
                          ,@(compile-cond (cdr clauses) shared-tmp)
                          (:label ,end-label)))
                      ;; (cond ((and ...) body) ...): chain boolean branches
                      ((and (consp test) (member (car test) '(and or)) (cddr test))
                       `(,@(compile-boolean-branch test else-label nil)
                         ,@(compile-progn body)
                         (:br ,end-label)
                         (:label ,else-label)
                         ,@(compile-cond (cdr clauses) shared-tmp)
                         (:label ,end-label)))
                      (t
                        ;; Default: normalize MV state, then IsTruthy
                        `(,@(compile-expr test)
                          (:call "MultipleValues.Primary")
                          (:call "Runtime.IsTruthy")
                          (:brfalse ,else-label)
                          ,@(compile-progn body)
                          (:br ,end-label)
                          (:label ,else-label)
                          ,@(compile-cond (cdr clauses) shared-tmp)
                          (:label ,end-label))))
                    ;; No body: all no-body arms share one CTMP slot (#114)
                    (let ((tmp (or shared-tmp (gen-local "CTMP"))))
                      `(,@(unless shared-tmp `((:declare-local ,tmp "LispObject")))
                        ,@(compile-expr test)
                        (:call "MultipleValues.Primary")
                        (:stloc ,tmp)
                        (:ldloc ,tmp)
                        (:call "Runtime.IsTruthy")
                        (:brfalse ,else-label)
                        (:ldloc ,tmp)
                        (:br ,end-label)
                        (:label ,else-label)
                        ,@(compile-cond (cdr clauses) tmp)
                        (:label ,end-label))))))))))

;;; ============================================================
;;; compile-form handler registrations
;;; Must be after all compile-* helpers are defined (both cil-compiler.lisp
;;; and cil-forms.lisp).  Populates *compile-form-handlers* for O(1) dispatch.
;;; ============================================================

(let ((h *compile-form-handlers*))

  ;; Arithmetic
  (setf (gethash '+ h) (lambda (expr) (compile-add (cdr expr))))
  (setf (gethash '- h) (lambda (expr) (compile-sub (cdr expr))))
  (setf (gethash '* h) (lambda (expr) (compile-mul (cdr expr))))
  (setf (gethash '/ h) (lambda (expr) (compile-div (cdr expr))))

  ;; Comparison (N-arg: (< a b c) means (and (< a b) (< b c)))
  (setf (gethash '> h) (lambda (expr) (compile-nary-comparison (cdr expr) '> "Runtime.GreaterThan")))
  (setf (gethash '< h) (lambda (expr) (compile-nary-comparison (cdr expr) '< "Runtime.LessThan")))
  (setf (gethash '>= h) (lambda (expr) (compile-nary-comparison (cdr expr) '>= "Runtime.GreaterEqual")))
  (setf (gethash '<= h) (lambda (expr) (compile-nary-comparison (cdr expr) '<= "Runtime.LessEqual")))
  (setf (gethash '= h) (lambda (expr) (compile-nary-comparison (cdr expr) '= "Runtime.NumEqual")))
  (setf (gethash '/= h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.NumNotEqualN"))))

  ;; Equality
  (setf (gethash 'eq h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Eq")))
  (setf (gethash 'eql h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Eql")))
  (setf (gethash 'equal h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Equal")))

  ;; List ops
  (setf (gethash 'cons h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.MakeCons" "CONS")))
  (setf (gethash 'car h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Car" "CAR")))
  (setf (gethash 'cdr h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Cdr" "CDR")))
  (setf (gethash 'list h) (lambda (expr) (compile-list-call (cdr expr))))
  (setf (gethash 'list* h) (lambda (expr) (compile-list-star-call (cdr expr))))
  (setf (gethash 'append h) (lambda (expr) (compile-append (cdr expr))))
  (setf (gethash 'length h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Length" "LENGTH")))
  (setf (gethash 'rplaca h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Rplaca" "RPLACA")))
  (setf (gethash 'rplacd h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Rplacd" "RPLACD")))
  (setf (gethash 'nreverse h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Nreverse" "NREVERSE")))
  (setf (gethash 'nth h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Nth" "NTH")))
  (setf (gethash 'nthcdr h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Nthcdr" "NTHCDR")))
  (setf (gethash 'last h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              (compile-unary-call (cdr expr) "Runtime.Last" "LAST")
              (compile-named-call 'last (cdr expr)))))
  (setf (gethash 'nconc h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (case (length args)
              (0 (emit-nil))
              (1 (let ((*in-tail-position* nil)) (compile-expr (first args))))
              (t (compile-named-call 'nconc args))))))
  (setf (gethash 'copy-list h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.CopyList" "COPY-LIST")))
  (setf (gethash 'member h)
        (lambda (expr)
          (let ((n-args (length (cdr expr))) (args (cdr expr)))
            (cond
              ((< n-args 2)
               (compile-static-program-error
                (format nil "MEMBER: wrong number of arguments: ~a (expected at least 2)" n-args)))
              ((and (= n-args 4) (eq (third args) :test)
                    (let ((tf (fourth args)))
                      (and (consp tf) (eq (car tf) 'function) (eq (cadr tf) 'eq))))
               (compile-binary-call (list (first args) (second args)) "Runtime.MemberEq"))
              ((cddr args) (compile-named-call 'member args))
              (t (compile-binary-call args "Runtime.Member" "MEMBER"))))))
  (setf (gethash 'assoc h)
        (lambda (expr)
          (let ((n-args (length (cdr expr))) (args (cdr expr)))
            (cond
              ((< n-args 2)
               (compile-static-program-error
                (format nil "ASSOC: wrong number of arguments: ~a (expected at least 2)" n-args)))
              ((and (= n-args 4) (eq (third args) :test)
                    (let ((tf (fourth args)))
                      (and (consp tf) (eq (car tf) 'function) (eq (cadr tf) 'eq))))
               (compile-binary-call (list (first args) (second args)) "Runtime.AssocEq"))
              ((cddr args) (compile-named-call 'assoc args))
              (t (compile-binary-call args "Runtime.Assoc" "ASSOC"))))))
  (setf (gethash 'cadr h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Cadr" "CADR")))
  (setf (gethash 'cddr h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Cddr" "CDDR")))
  (setf (gethash 'caar h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Caar" "CAAR")))
  (setf (gethash 'cdar h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Cdar" "CDAR")))
  (setf (gethash 'caddr h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Caddr" "CADDR")))

  ;; List accessor aliases
  (setf (gethash 'first h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Car" "FIRST")))
  (setf (gethash 'rest h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Cdr" "REST")))
  (setf (gethash 'second h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Cadr" "SECOND")))
  (setf (gethash 'third h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Caddr" "THIRD")))

  ;; Type predicates
  (setf (gethash 'not h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Not" "NOT")))
  (setf (gethash 'null h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Not" "NULL")))
  (setf (gethash 'atom h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Atom" "ATOM")))
  (setf (gethash 'consp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Consp" "CONSP")))
  (setf (gethash 'listp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Listp" "LISTP")))
  (setf (gethash 'numberp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Numberp")))
  (setf (gethash 'integerp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Integerp")))
  (setf (gethash 'symbolp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Symbolp")))
  (setf (gethash 'stringp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Stringp")))
  (setf (gethash 'characterp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Characterp")))
  (setf (gethash 'functionp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Functionp")))
  (setf (gethash 'rationalp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Rationalp")))
  (setf (gethash 'floatp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Floatp")))
  (setf (gethash 'complexp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Complexp")))
  (setf (gethash 'type-of h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.TypeOf")))
  (setf (gethash 'typep h)
        (lambda (expr)
          (let* ((a (cdr expr)) (nargs (length a)))
            (cond
              ((< nargs 2)
               (compile-static-program-error
                (format nil "TYPEP: too few arguments: ~D (expected 2-3)" nargs)))
              ((> nargs 3)
               (compile-static-program-error
                (format nil "TYPEP: too many arguments: ~D (expected 2-3)" nargs)))
              (t
               (let ((type-arg (cadr a)))
                 (if (and (consp type-arg) (eq (car type-arg) 'quote) (symbolp (cadr type-arg)))
                     (let* ((type-name (cadr type-arg))
                            (predicate (cdr (assoc type-name
                                             '((cons . "Runtime.Consp")
                                               (list . "Runtime.Listp")
                                               (null . "Runtime.Not")
                                               (number . "Runtime.Numberp")
                                               (integer . "Runtime.Integerp")
                                               (rational . "Runtime.Rationalp")
                                               (float . "Runtime.Floatp")
                                               (complex . "Runtime.Complexp")
                                               (symbol . "Runtime.Symbolp")
                                               (string . "Runtime.Stringp")
                                               (character . "Runtime.Characterp")
                                               (function . "Runtime.Functionp")
                                               (atom . "Runtime.Atom")
                                               (vector . "Runtime.Vectorp")
                                               (hash-table . "Runtime.Hash_table_p")
                                               (package . "Runtime.Packagep"))))))
                       (if predicate
                           (compile-unary-call (list (car a)) predicate "TYPEP")
                           (compile-binary-call (list (car a) type-arg) "Runtime.Typep")))
                     (compile-binary-call (list (car a) type-arg) "Runtime.Typep"))))))))
  (setf (gethash 'vectorp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Vectorp")))
  (setf (gethash 'keywordp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Keywordp")))
  (setf (gethash 'hash-table-p h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Hash_table_p")))
  (setf (gethash 'packagep h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Packagep")))
  (setf (gethash 'subtypep h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (if (= (length args) 3)
                (compile-binary-call (list (first args) (second args)) "Runtime.Subtypep")
                (compile-binary-call args "Runtime.Subtypep")))))

  ;; Array ops
  (setf (gethash 'svref h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Aref")))
  (setf (gethash 'aref h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (case (length args)
              (2 (compile-binary-call args "Runtime.Aref"))
              (3 (compile-ternary-call args "Runtime.Aref2D"))
              (4 (compile-quaternary-call args "Runtime.Aref3D"))
              (t `(,@(compile-args-array args) (:call "Runtime.ArefMulti")))))))
  (setf (gethash '%aref-set h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (case (length args)
              (3 (compile-ternary-call args "Runtime.ArefSet"))
              (4 (compile-quaternary-call args "Runtime.ArefSet2D"))
              (5 (compile-quinary-call args "Runtime.ArefSet3D"))
              (t `(,@(compile-args-array args) (:call "Runtime.ArefSetMulti")))))))
  (setf (gethash '%char-set h) (lambda (expr) (compile-ternary-call (cdr expr) "Runtime.CharSet")))
  (setf (gethash 'vector-push-extend h)
        (lambda (expr)
          (if (= (length (cdr expr)) 2)
              (compile-binary-call (cdr expr) "Runtime.VectorPushExtend2")
              (compile-named-call 'vector-push-extend (cdr expr)))))
  (setf (gethash 'vector-push h)
        (lambda (expr)
          (if (= (length (cdr expr)) 2)
              (compile-binary-call (cdr expr) "Runtime.VectorPush2")
              (compile-named-call 'vector-push (cdr expr)))))
  (setf (gethash '%set-symbol-value h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.SetSymbolValue")))

  ;; error
  (setf (gethash 'error h)
        (lambda (expr)
          `(,@(compile-args-array (cdr expr))
            (:call "Runtime.LispErrorFormat"))))

  ;; locally: preserves top-level-ness (uses *compile-was-toplevel*)
  (setf (gethash 'locally h)
        (lambda (expr)
          (let ((*at-toplevel* *compile-was-toplevel*))
            (multiple-value-bind (declared-specials real-body) (extract-specials (cdr expr))
              (if (null declared-specials)
                  (compile-progn real-body)
                  (let* ((*specials* (append declared-specials *specials*))
                         (special-names (mapcar #'symbol-name declared-specials))
                         (*locals* (remove-if
                                    (lambda (entry)
                                      (let ((k (car entry)))
                                        (member (if (symbolp k) (symbol-name k) "")
                                                special-names :test #'string=)))
                                    *locals*)))
                    (compile-progn real-body)))))))

  ;; handler-case
  (setf (gethash 'handler-case h)
        (lambda (expr)
          (let* ((body-form (cadr expr))
                 (clauses (cddr expr))
                 (no-error-clause (find :no-error clauses :key #'car))
                 (error-clauses (remove :no-error clauses :key #'car)))
            (if no-error-clause
                (let* ((ne-lambda-list (cadr no-error-clause))
                       (ne-body (cddr no-error-clause))
                       (block-tag (gensym "HC-NO-ERROR"))
                       (wrapped-clauses
                         (mapcar (lambda (clause)
                                   (let ((type (car clause))
                                         (var-list (cadr clause))
                                         (clause-body (cddr clause)))
                                     `(,type ,var-list
                                       (return-from ,block-tag (progn ,@clause-body)))))
                                 error-clauses)))
                  (compile-expr
                   `(block ,block-tag
                      (multiple-value-call (lambda ,ne-lambda-list ,@ne-body)
                        (handler-case ,body-form ,@wrapped-clauses)))))
                (compile-handler-case body-form clauses)))))
  (setf (gethash 'handler-bind h) (lambda (expr) (compile-handler-bind (cadr expr) (cddr expr))))

  ;; restart-case / restart-bind / invoke-restart / find-restart / compute-restarts
  (setf (gethash 'restart-case h) (lambda (expr) (compile-restart-case (cadr expr) (cddr expr))))
  (setf (gethash 'restart-bind h) (lambda (expr) (compile-restart-bind (cadr expr) (cddr expr))))
  (setf (gethash 'invoke-restart h)
        (lambda (expr)
          `(,@(compile-expr (cadr expr))
            ,@(compile-args-array (cddr expr))
            (:call "Runtime.InvokeRestart"))))
  (setf (gethash 'find-restart h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              (compile-unary-call (cdr expr) "Runtime.FindRestart")
              `(,@(compile-args-array (cdr expr)) (:call "Runtime.FindRestartN")))))
  (setf (gethash 'compute-restarts h)
        (lambda (expr)
          (if (null (cdr expr))
              '((:call "Runtime.ComputeRestarts"))
              `(,@(compile-args-array (cdr expr)) (:call "Runtime.ComputeRestartsN")))))

  ;; signal / warn
  (setf (gethash 'signal h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              (compile-unary-call (cdr expr) "Runtime.LispSignal")
              `(,@(compile-args-array (cdr expr)) (:call "Runtime.LispSignalFormat")))))
  (setf (gethash 'warn h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              (compile-unary-call (cdr expr) "Runtime.LispWarn")
              `(,@(compile-args-array (cdr expr)) (:call "Runtime.LispWarnFormat")))))

  ;; Struct primitives
  (setf (gethash '%make-struct h)
        (lambda (expr)
          `(,@(compile-expr (cadr expr))
            ,@(compile-args-array (cddr expr))
            (:call "Runtime.MakeStruct"))))
  (setf (gethash '%struct-ref h)
        (lambda (expr)
          (let ((obj (first (cdr expr))) (idx (second (cdr expr))))
            (if (integerp idx)
                `(,@(let ((*in-tail-position* nil)) (compile-expr obj))
                  (:ldc-i4 ,idx) (:call "Runtime.StructRefI"))
                (compile-binary-call (cdr expr) "Runtime.StructRef")))))
  (setf (gethash '%struct-set h)
        (lambda (expr)
          (let ((obj (first (cdr expr))) (idx (second (cdr expr))) (val (third (cdr expr))))
            (if (integerp idx)
                `(,@(let ((*in-tail-position* nil)) (compile-expr obj))
                  (:ldc-i4 ,idx)
                  ,@(let ((*in-tail-position* nil)) (compile-expr val))
                  (:call "Runtime.StructSetI"))
                `(,@(compile-expr obj)
                  ,@(compile-expr idx)
                  ,@(compile-expr val)
                  (:call "Runtime.StructSet"))))))
  (setf (gethash '%struct-typep h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.StructTypep")))
  (setf (gethash '%copy-struct h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.CopyStruct")))

  ;; CLOS primitives
  (setf (gethash '%make-class h)
        (lambda (expr)
          `(,@(compile-expr (second expr))
            ,@(compile-expr (third expr))
            ,@(compile-expr (fourth expr))
            (:call "Runtime.MakeClass"))))
  (setf (gethash '%make-slot-def h)
        (lambda (expr)
          `(,@(compile-expr (second expr))
            ,@(compile-expr (third expr))
            ,@(compile-expr (fourth expr))
            (:call "Runtime.MakeSlotDef"))))
  (setf (gethash '%make-slot-def-with-allocation h)
        (lambda (expr)
          `(,@(compile-expr (second expr))
            ,@(compile-expr (third expr))
            ,@(compile-expr (fourth expr))
            ,@(compile-expr (fifth expr))
            (:call "Runtime.MakeSlotDefWithAllocation"))))
  (setf (gethash '%register-class h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.RegisterClass")))
  (setf (gethash '%set-class-default-initargs h)
        (lambda (expr)
          `(,@(compile-expr (second expr))
            ,@(compile-expr (third expr))
            (:call "Runtime.SetClassDefaultInitargs"))))
  (setf (gethash 'find-class h)
        (lambda (expr)
          (if (null (cddr expr))
              (compile-unary-call (cdr expr) "Runtime.FindClass")
              (compile-named-call 'find-class (cdr expr)))))
  (setf (gethash '%find-class-or-nil h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.FindClassOrNil")))
  (setf (gethash 'class-of h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.ClassOf")))
  (setf (gethash 'boundp h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Boundp")))
  (setf (gethash 'symbol-value h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.SymbolValue")))
  (setf (gethash 'fdefinition h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Fdefinition")))
  (setf (gethash '%getenv h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Getenv")))
  (setf (gethash 'slot-value h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.SlotValue")))
  (setf (gethash 'slot-boundp h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.SlotBoundp")))
  (setf (gethash '%set-slot-value h)
        (lambda (expr)
          `(,@(compile-expr (second expr))
            ,@(compile-expr (third expr))
            ,@(compile-expr (fourth expr))
            (:call "Runtime.SetSlotValue"))))
  (setf (gethash '%allocate-instance h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.MakeInstanceRaw")))
  (setf (gethash '%slot-exists-p h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.SlotExists")))
  (setf (gethash '%change-class h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.ChangeClass"))))

  ;; GF primitives
  (setf (gethash '%make-gf h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.MakeGF")))
  (setf (gethash '%register-gf h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.RegisterGF")))
  (setf (gethash '%set-method-combination h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.SetMethodCombination")))
  (setf (gethash '%set-method-combination-order h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.SetMethodCombinationOrder")))
  (setf (gethash '%set-method-combination-args h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.SetMethodCombinationArgs")))
  (setf (gethash '%set-gf-lambda-list-info h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.SetGFLambdaListInfo"))))
  (setf (gethash '%set-method-lambda-list-info h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.SetMethodLambdaListInfo"))))
  (setf (gethash '%find-gf h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.FindGF")))
  (setf (gethash '%clear-defgeneric-inline-methods h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.ClearDefgenericInlineMethods")))
  (setf (gethash '%mark-defgeneric-inline-method h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.MarkDefgenericInlineMethod")))
  (setf (gethash '%make-method h)
        (lambda (expr)
          `(,@(compile-expr (second expr))
            ,@(compile-expr (third expr))
            ,@(compile-expr (fourth expr))
            (:call "Runtime.MakeMethod"))))
  (setf (gethash '%add-method h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.AddMethod")))
  (setf (gethash '%gf-methods h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.GetGFMethods")))
  (setf (gethash '%method-specializers h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.MethodSpecializers")))
  (setf (gethash '%method-qualifiers h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.MethodQualifiers")))
  (setf (gethash '%method-function h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.MethodFunction")))
  (setf (gethash 'call-next-method h)
        (lambda (expr)
          `(,@(compile-args-array (cdr expr)) (:call "Runtime.CallNextMethod"))))
  (setf (gethash 'next-method-p h)
        (lambda (expr)
          (if (cdr expr)
              `((:ldstr ,(format nil "NEXT-METHOD-P was called with ~D argument~:P; accepts exactly 0."
                                 (length (cdr expr))))
                (:call "Runtime.ProgramError"))
              '((:call "Runtime.NextMethodP")))))
  (setf (gethash '%make-instance-with-initargs h)
        (lambda (expr)
          `(,@(compile-expr (second expr))
            ,@(compile-args-array (cddr expr))
            (:call "Runtime.MakeInstanceWithInitargs"))))

  ;; Derived predicates
  (setf (gethash 'zerop h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              `(,@(compile-expr (cadr expr)) ,@(emit-fixnum 0) (:call "Runtime.NumEqual"))
              (compile-named-call 'zerop (cdr expr)))))
  (setf (gethash 'plusp h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              `(,@(compile-expr (cadr expr)) ,@(emit-fixnum 0) (:call "Runtime.GreaterThan"))
              (compile-named-call 'plusp (cdr expr)))))
  (setf (gethash 'minusp h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              `(,@(compile-expr (cadr expr)) ,@(emit-fixnum 0) (:call "Runtime.LessThan"))
              (compile-named-call 'minusp (cdr expr)))))
  (setf (gethash 'evenp h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              (let ((tmp (gensym "EVENP")))
                (compile-expr
                  `(let ((,tmp ,(cadr expr)))
                     (if (integerp ,tmp) (= (mod ,tmp 2) 0)
                         (error 'type-error :datum ,tmp :expected-type 'integer)))))
              (compile-named-call 'evenp (cdr expr)))))
  (setf (gethash 'oddp h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              (let ((tmp (gensym "ODDP")))
                (compile-expr
                  `(let ((,tmp ,(cadr expr)))
                     (if (integerp ,tmp) (not (= (mod ,tmp 2) 0))
                         (error 'type-error :datum ,tmp :expected-type 'integer)))))
              (compile-named-call 'oddp (cdr expr)))))

  ;; Math
  (setf (gethash 'abs h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Abs")))
  (setf (gethash 'mod h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Mod")))
  (setf (gethash 'rem h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Rem")))
  (dolist (op-method '((floor . "Runtime.FloorOp") (truncate . "Runtime.TruncateOp")
                        (ceiling . "Runtime.CeilingOp") (round . "Runtime.RoundOp")))
    (let ((op (car op-method)) (method (cdr op-method)))
      (setf (gethash op h)
            (lambda (expr)
              (let ((args (cdr expr)))
                (cond ((= (length args) 1) (compile-binary-call (list (car args) 1) method))
                      ((= (length args) 2) (compile-binary-call args method))
                      (t (compile-named-call op args))))))))
  (setf (gethash 'min h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (cond ((= (length args) 0) (compile-named-call 'min args))
                  ((= (length args) 1) (compile-named-call 'min args))
                  ((= (length args) 2) (compile-binary-call args "Runtime.Min"))
                  (t (compile-expr (reduce (lambda (a b) `(min ,a ,b)) args)))))))
  (setf (gethash 'max h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (cond ((= (length args) 0) (compile-named-call 'max args))
                  ((= (length args) 1) (compile-named-call 'max args))
                  ((= (length args) 2) (compile-binary-call args "Runtime.Max"))
                  (t (compile-expr (reduce (lambda (a b) `(max ,a ,b)) args)))))))
  (setf (gethash 'gcd h)
        (lambda (expr)
          (if (= (length (cdr expr)) 2)
              (compile-binary-call (cdr expr) "Runtime.Gcd")
              (compile-named-call 'gcd (cdr expr)))))
  (setf (gethash 'lcm h)
        (lambda (expr)
          (if (= (length (cdr expr)) 2)
              (compile-binary-call (cdr expr) "Runtime.Lcm")
              (compile-named-call 'lcm (cdr expr)))))
  (setf (gethash 'expt h)
        (lambda (expr)
          (if (= (length (cdr expr)) 2)
              (compile-binary-call (cdr expr) "Runtime.Expt")
              (compile-named-call 'expt (cdr expr)))))
  (setf (gethash 'ash h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (if (= (length args) 2)
                (or (compile-ash-fast args)
                    (compile-binary-call args "Runtime.Ash"))
                (compile-named-call 'ash args)))))
  (setf (gethash 'lognot h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (cond
              ((and (= (length args) 1) (fixnum-typed-p (first args)))
               (compile-fixbit-not args))
              ((= (length args) 1)
               (compile-unary-call args "Runtime.Lognot"))
              (t (compile-named-call 'lognot args))))))
  (setf (gethash 'integer-length h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              (compile-unary-call (cdr expr) "Runtime.IntegerLength")
              (compile-named-call 'integer-length (cdr expr)))))
  (setf (gethash 'logbitp h)
        (lambda (expr)
          (if (= (length (cdr expr)) 2)
              (compile-binary-call (cdr expr) "Runtime.Logbitp")
              (compile-named-call 'logbitp (cdr expr)))))
  (dolist (item (list (list 'logior "Runtime.Logior2" "Runtime.Logior" 0 :or)
                      (list 'logand "Runtime.Logand2" "Runtime.Logand" -1 :and)
                      (list 'logxor "Runtime.Logxor2" "Runtime.Logxor" 0 :xor)))
    (let ((op (first item)) (method2 (second item)) (methodN (third item))
          (zero-val (fourth item)) (cil-op (fifth item)))
      (setf (gethash op h)
            (lambda (expr)
              (let ((args (cdr expr)))
                (case (length args)
                  (0 (emit-fixnum zero-val))
                  (1 (let ((*in-tail-position* nil)) (compile-expr (first args))))
                  (2 (if (and (fixnum-typed-p (first args))
                              (fixnum-typed-p (second args)))
                         (compile-fixbit-binop args cil-op)
                         (compile-binary-call args method2)))
                  (t `(,@(compile-args-array args) (:call ,methodN)))))))))

  ;; I/O
  (setf (gethash 'print h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond ((= nargs 1) `(,@(compile-expr (cadr expr)) (:call "Runtime.Print")))
                  ((= nargs 2) `(,@(compile-expr (cadr expr)) ,@(compile-expr (caddr expr)) (:call "Runtime.Print2")))
                  (t (compile-static-program-error (format nil "PRINT: wrong number of arguments: ~a (expected 1-2)" nargs)))))))
  (setf (gethash 'prin1 h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond ((= nargs 1) `(,@(compile-expr (cadr expr)) (:call "Runtime.Prin1")))
                  ((= nargs 2) `(,@(compile-expr (cadr expr)) ,@(compile-expr (caddr expr)) (:call "Runtime.Prin12")))
                  (t (compile-static-program-error (format nil "PRIN1: wrong number of arguments: ~a (expected 1-2)" nargs)))))))
  (setf (gethash 'princ h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond ((= nargs 1) `(,@(compile-expr (cadr expr)) (:call "Runtime.Princ")))
                  ((= nargs 2) `(,@(compile-expr (cadr expr)) ,@(compile-expr (caddr expr)) (:call "Runtime.Princ2")))
                  (t (compile-static-program-error (format nil "PRINC: wrong number of arguments: ~a (expected 1-2)" nargs)))))))
  (setf (gethash 'terpri h)
        (lambda (expr)
          (if (> (length (cdr expr)) 1)
              (compile-static-program-error (format nil "TERPRI: wrong number of arguments: ~a (expected 0-1)" (length (cdr expr))))
              `(,@(compile-args-array (cdr expr)) (:call "Runtime.Terpri")))))
  (setf (gethash 'fresh-line h)
        (lambda (expr)
          (if (> (length (cdr expr)) 1)
              (compile-static-program-error (format nil "FRESH-LINE: wrong number of arguments: ~a (expected 0-1)" (length (cdr expr))))
              `(,@(compile-args-array (cdr expr)) (:call "Runtime.FreshLine")))))
  (setf (gethash 'format h)
        (lambda (expr)
          (let ((stream-tmp (gen-local "FMTDST")) (args-tmp (gen-local "FMTARGS")))
            `((:declare-local ,stream-tmp "LispObject")
              (:declare-local ,args-tmp "LispObject[]")
              ,@(compile-expr (cadr expr)) (:stloc ,stream-tmp)
              ,@(compile-args-array (cddr expr)) (:stloc ,args-tmp)
              (:ldloc ,stream-tmp) (:ldloc ,args-tmp)
              (:call "Runtime.Format")))))
  (setf (gethash 'open h)
        (lambda (expr)
          (if (null (cdr expr))
              (compile-static-program-error "OPEN: wrong number of arguments: 0 (expected at least 1)")
              (let ((path-tmp (gen-local "OPNDST")) (args-tmp (gen-local "OPNARGS")))
                `((:declare-local ,path-tmp "LispObject")
                  (:declare-local ,args-tmp "LispObject[]")
                  ,@(compile-expr (cadr expr)) (:stloc ,path-tmp)
                  ,@(compile-args-array (cddr expr)) (:stloc ,args-tmp)
                  (:ldloc ,path-tmp) (:ldloc ,args-tmp)
                  (:call "Runtime.OpenFile"))))))
  (setf (gethash 'open-stream-p h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.OpenStreamP")))
  (setf (gethash 'read-line h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((= nargs 0) `(,@(compile-expr '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadLine")))
              ((= nargs 1) `(,@(compile-expr (cadr expr)) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadLine")))
              ((= nargs 2) `(,@(compile-expr (cadr expr)) ,@(compile-expr (caddr expr)) ,@(emit-nil) (:call "Runtime.ReadLine")))
              ((<= nargs 4) `(,@(compile-expr (cadr expr)) ,@(compile-expr (caddr expr)) ,@(compile-expr (cadddr expr)) (:call "Runtime.ReadLine")))
              (t (compile-static-program-error (format nil "READ-LINE: wrong number of arguments: ~a (expected 0-4)" nargs)))))))
  (setf (gethash 'read-char h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((= nargs 0) `(,@(compile-expr '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadChar")))
              ((= nargs 1) `(,@(compile-expr (cadr expr)) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadChar")))
              ((= nargs 2) `(,@(compile-expr (cadr expr)) ,@(compile-expr (caddr expr)) ,@(emit-nil) (:call "Runtime.ReadChar")))
              ((<= nargs 4) `(,@(compile-expr (cadr expr)) ,@(compile-expr (caddr expr)) ,@(compile-expr (cadddr expr)) (:call "Runtime.ReadChar")))
              (t (compile-static-program-error (format nil "READ-CHAR: wrong number of arguments: ~a (expected 0-4)" nargs)))))))
  (setf (gethash 'read-char-no-hang h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((= nargs 0) `(,@(compile-expr '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadCharNoHang")))
              ((= nargs 1) `(,@(compile-expr (cadr expr)) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadCharNoHang")))
              ((= nargs 2) `(,@(compile-expr (cadr expr)) ,@(compile-expr (caddr expr)) ,@(emit-nil) (:call "Runtime.ReadCharNoHang")))
              ((<= nargs 4) `(,@(compile-expr (cadr expr)) ,@(compile-expr (caddr expr)) ,@(compile-expr (cadddr expr)) (:call "Runtime.ReadCharNoHang")))
              (t (compile-static-program-error (format nil "READ-CHAR-NO-HANG: wrong number of arguments: ~a (expected 0-4)" nargs)))))))
  (setf (gethash 'listen h)
        (lambda (expr)
          (if (null (cdr expr))
              `(,@(compile-expr '*standard-input*) (:call "Runtime.Listen"))
              (compile-unary-call (cdr expr) "Runtime.Listen"))))
  (setf (gethash 'clear-input h)
        (lambda (expr)
          (if (null (cdr expr))
              `(,@(compile-expr '*standard-input*) (:call "Runtime.ClearInput"))
              (compile-unary-call (cdr expr) "Runtime.ClearInput"))))
  (setf (gethash 'write-byte h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.WriteByte")))
  (setf (gethash 'peek-char h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((= nargs 0) `(,@(emit-nil) ,@(compile-expr '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.PeekChar")))
              ((= nargs 1) `(,@(compile-expr (cadr expr)) ,@(compile-expr '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.PeekChar")))
              ((= nargs 2) `(,@(compile-expr (cadr expr)) ,@(compile-expr (caddr expr)) ,@(emit-t) ,@(emit-nil) (:call "Runtime.PeekChar")))
              ((= nargs 3) `(,@(compile-expr (cadr expr)) ,@(compile-expr (caddr expr)) ,@(compile-expr (cadddr expr)) ,@(emit-nil) (:call "Runtime.PeekChar")))
              ((<= nargs 5) `(,@(compile-expr (cadr expr)) ,@(compile-expr (caddr expr)) ,@(compile-expr (cadddr expr)) ,@(compile-expr (car (cddddr expr))) (:call "Runtime.PeekChar")))
              (t (compile-static-program-error (format nil "PEEK-CHAR: wrong number of arguments: ~a (expected 0-5)" nargs)))))))
  (setf (gethash 'unread-char h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              `(,@(compile-expr (cadr expr)) ,@(compile-expr '*standard-input*) (:call "Runtime.UnreadChar"))
              (compile-binary-call (cdr expr) "Runtime.UnreadChar"))))
  (setf (gethash 'write-char h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              `(,@(compile-expr (cadr expr)) ,@(compile-expr '*standard-output*) (:call "Runtime.WriteChar"))
              (compile-binary-call (cdr expr) "Runtime.WriteChar"))))
  (setf (gethash 'write-string h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.WriteString"))))
  (setf (gethash 'write-line h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.WriteLine"))))
  (setf (gethash 'directory h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.Directory"))))
  (setf (gethash 'probe-file h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.ProbeFile")))
  (setf (gethash 'truename h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Truename")))
  (setf (gethash 'delete-file h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.DeleteFile")))
  (setf (gethash 'file-author h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.FileAuthor")))
  (setf (gethash 'file-error-pathname h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.FileErrorPathname")))
  (setf (gethash 'rename-file h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.RenameFile")))
  (setf (gethash 'file-write-date h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.FileWriteDate")))

  ;; String streams / pathnames
  (setf (gethash 'make-string-output-stream h)
        (lambda (expr)
          (if (null (cdr expr))
              '((:call "Runtime.MakeStringOutputStream"))
              (compile-named-call 'make-string-output-stream (cdr expr)))))
  (setf (gethash 'get-output-stream-string h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.GetOutputStreamString")))
  (setf (gethash 'make-string-input-stream h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.MakeStringInputStream"))))
  (setf (gethash 'make-pathname h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.MakePathname"))))
  (setf (gethash 'pathname h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Pathname")))
  (setf (gethash 'namestring h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Namestring")))

  ;; Read
  (setf (gethash 'read h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((> nargs 4) (compile-static-program-error (format nil "READ: too many arguments: ~D (expected at most 4)" nargs)))
              ((= nargs 0) `(,@(compile-expr '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadFromStream")))
              ((= nargs 1) `(,@(compile-expr (cadr expr)) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadFromStream")))
              ((>= nargs 3) `(,@(compile-expr (cadr expr)) ,@(compile-expr (caddr expr)) ,@(compile-expr (cadddr expr)) (:call "Runtime.ReadFromStream")))
              (t `(,@(compile-expr (cadr expr)) ,@(compile-expr (caddr expr)) ,@(emit-nil) (:call "Runtime.ReadFromStream")))))))
  (setf (gethash 'read-from-string h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.ReadFromString"))))
  (setf (gethash 'read-preserving-whitespace h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((> nargs 4) (compile-static-program-error (format nil "READ-PRESERVING-WHITESPACE: too many arguments: ~D (expected at most 4)" nargs)))
              ((= nargs 0) `(,@(compile-expr '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadPreservingWhitespace")))
              ((= nargs 1) `(,@(compile-expr (cadr expr)) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadPreservingWhitespace")))
              ((>= nargs 3) `(,@(compile-expr (cadr expr)) ,@(compile-expr (caddr expr)) ,@(compile-expr (cadddr expr)) (:call "Runtime.ReadPreservingWhitespace")))
              (t `(,@(compile-expr (cadr expr)) ,@(compile-expr (caddr expr)) ,@(emit-nil) (:call "Runtime.ReadPreservingWhitespace")))))))

  ;; Eval / gensym / misc
  (setf (gethash 'eval h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Eval")))
  (setf (gethash 'gensym h)
        (lambda (expr)
          (if (cdr expr)
              (compile-unary-call (cdr expr) "Runtime.Gensym")
              '((:call "Runtime.Gensym0")))))
  (setf (gethash 'streamp h)
        (lambda (expr)
          (if (and (cdr expr) (null (cddr expr)))
              `(,@(compile-expr (cadr expr)) ,@(compile-quoted 'stream) (:call "Runtime.Typep"))
              (compile-named-call 'streamp (cdr expr)))))
  (setf (gethash 'lisp-implementation-type h)
        (lambda (expr)
          (if (null (cdr expr))
              '((:ldstr "dotcl") (:newobj "LispString"))
              (compile-named-call 'lisp-implementation-type (cdr expr)))))

  ;; String / char ops
  (setf (gethash 'random h)
        (lambda (expr)
          (let ((n (length (cdr expr))))
            (cond
              ((= n 1) (compile-unary-call (cdr expr) "Runtime.Random" "RANDOM"))
              ((= n 2) (compile-binary-call (cdr expr) "Runtime.Random2" "RANDOM"))
              (t (compile-static-program-error (format nil "RANDOM: wrong number of arguments: ~a (expected 1 or 2)" n)))))))
  (setf (gethash 'string-trim h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.StringTrim")))
  (setf (gethash 'string-left-trim h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.StringLeftTrim")))
  (setf (gethash 'string-right-trim h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.StringRightTrim")))
  (setf (gethash 'char-code h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.CharCode")))
  (setf (gethash 'code-char h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.CodeChar")))
  (setf (gethash 'digit-char-p h)
        (lambda (expr)
          (let ((args (cdr expr)) (nargs (length (cdr expr))))
            (cond
              ((= nargs 0) (compile-static-program-error "DIGIT-CHAR-P: too few arguments: 0 (expected 1-2)"))
              ((> nargs 2) (compile-static-program-error (format nil "DIGIT-CHAR-P: too many arguments: ~D (expected 1-2)" nargs)))
              ((= nargs 1) `(,@(compile-expr (car args)) (:ldc-i4 10) (:call "Fixnum.Make") (:call "Runtime.DigitCharP")))
              (t (compile-binary-call args "Runtime.DigitCharP"))))))
  (setf (gethash 'string h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.String")))
  (setf (gethash 'char h)
        (lambda (expr)
          (if (= (length (cdr expr)) 2)
              (compile-binary-call (cdr expr) "Runtime.CharAccess")
              (compile-named-call 'char (cdr expr)))))
  (setf (gethash 'search h)
        (lambda (expr)
          (let ((n-args (length (cdr expr))))
            (cond
              ((< n-args 2) (compile-static-program-error (format nil "SEARCH: wrong number of arguments: ~a (expected at least 2)" n-args)))
              ((cddr (cdr expr)) (compile-named-call 'search (cdr expr)))
              (t (compile-binary-call (cdr expr) "Runtime.Search" "SEARCH"))))))

  ;; Symbol ops
  (setf (gethash 'symbol-name h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.SymbolName")))
  (setf (gethash 'symbol-package h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.SymbolPackage")))

  ;; Package ops (eq-accessible versions; string= versions remain in compile-form fallback)
  (setf (gethash 'hash-table-pairs h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.HashTablePairs")))
  (setf (gethash 'find-package h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.FindPackage")))
  (setf (gethash 'package-name h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.PackageName")))
  (setf (gethash 'package-error-package h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.PackageErrorPackage")))
  (setf (gethash 'delete-package h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.DeletePackage")))
  (setf (gethash 'find-symbol h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.FindSymbolL"))))
  (setf (gethash 'unintern h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.UninternSymbol"))))
  (setf (gethash 'package-used-by-list h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.PackageUsedByList")))
  (setf (gethash 'package-use-list h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.PackageUseListL")))
  (setf (gethash 'package-shadowing-symbols h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.PackageShadowingSymbols")))
  (setf (gethash 'package-nicknames h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.PackageNicknamesList")))
  (setf (gethash 'list-all-packages h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.ListAllPackagesV"))))
  (setf (gethash 'make-package h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.MakePackageK"))))
  (setf (gethash 'rename-package h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.RenamePackage"))))

  ;; Sequence ops
  (setf (gethash 'elt h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Elt")))
  (setf (gethash 'subseq h) (lambda (expr) (compile-subseq (cdr expr))))
  (setf (gethash 'concatenate h) (lambda (expr) (compile-concatenate (cdr expr))))
  (setf (gethash 'sort h)
        (lambda (expr)
          (if (cddr (cdr expr))
              (compile-named-call 'sort (cdr expr))
              (compile-binary-call (cdr expr) "Runtime.Sort"))))
  (setf (gethash 'stable-sort h)
        (lambda (expr)
          (if (cddr (cdr expr))
              (compile-named-call 'stable-sort (cdr expr))
              (compile-binary-call (cdr expr) "Runtime.Sort"))))
  (setf (gethash 'reverse h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.Reverse" "REVERSE")))
  (setf (gethash 'coerce h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Coerce")))

  ;; Higher-order
  (setf (gethash 'apply h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (if (cddr args)
                (compile-expr `(apply ,(car args) (list* ,@(cdr args))))
                (compile-binary-call args "Runtime.Apply")))))
  (setf (gethash 'maphash h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Maphash")))
  (setf (gethash 'mapcar h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (if (cddr args)
                `(,@(compile-expr (car args)) ,@(compile-args-array (cdr args)) (:call "Runtime.MapcarN"))
                (compile-binary-call args "Runtime.Mapcar")))))

  ;; Property list
  (setf (gethash 'get h)
        (lambda (expr)
          (let* ((args (cdr expr)) (nargs (length args)))
            (cond
              ((< nargs 2) (compile-static-program-error (format nil "GET: too few arguments: ~D (expected at least 2)" nargs)))
              ((> nargs 3) (compile-static-program-error (format nil "GET: too many arguments: ~D (expected at most 3)" nargs)))
              (t `(,@(compile-expr (first args))
                   ,@(compile-expr (second args))
                   ,@(if (third args) (compile-expr (third args)) (emit-nil))
                   (:call "Runtime.GetProp")))))))
  (setf (gethash 'put-prop h)
        (lambda (expr)
          `(,@(compile-expr (cadr expr))
            ,@(compile-expr (caddr expr))
            ,@(compile-expr (cadddr expr))
            (:call "Runtime.PutProp"))))
  (setf (gethash 'remprop h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Remprop")))
  (setf (gethash 'copy-symbol h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (cond ((= (length args) 1) (compile-unary-call args "Runtime.CopySymbol"))
                  ((= (length args) 2) (compile-binary-call args "Runtime.CopySymbolFull"))
                  (t (compile-static-program-error (format nil "COPY-SYMBOL: wrong number of arguments: ~a" (length args))))))))

  ;; Hash table
  (setf (gethash 'make-hash-table h)
        (lambda (expr)
          (let ((test-expr nil)
                (has-other-kw nil))
            (loop for (k v) on (cdr expr) by #'cddr
                  when (and (keywordp k) (string= (symbol-name k) "TEST"))
                    do (setf test-expr v)
                  else when (keywordp k)
                    ;; :SYNCHRONIZED or other keywords → fall back to variadic call
                    do (setf has-other-kw t))
            (cond
              ;; Any keyword other than :test → use variadic function path
              ;; (keeps SYNCHRONIZED handling in the registered LispFunction)
              (has-other-kw
               (compile-named-call 'make-hash-table (cdr expr)))
              (test-expr
               (let ((literal-name
                      (cond
                        ((and (consp test-expr) (eq (car test-expr) 'function)) (symbol-name (cadr test-expr)))
                        ((and (consp test-expr) (eq (car test-expr) 'quote)) (symbol-name (cadr test-expr)))
                        (t nil))))
                 (if literal-name
                     `((:ldstr ,literal-name) (:call "Startup.Keyword") (:call "Runtime.MakeHashTable"))
                     `(,@(compile-expr test-expr) (:call "Runtime.MakeHashTable")))))
              (t '((:call "Runtime.MakeHashTable0")))))))
  (setf (gethash 'gethash h) (lambda (expr) (compile-gethash (cdr expr))))
  (setf (gethash 'puthash h) (lambda (expr) (compile-puthash (cdr expr))))
  (setf (gethash 'remhash h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Remhash")))

  ;; Values
  (setf (gethash 'values h) (lambda (expr) (compile-values-call (cdr expr))))
  (setf (gethash 'multiple-value-list h)
        (lambda (expr)
          `((:call "MultipleValues.Reset")
            ,@(let ((*in-mv-context* t))
                (compile-expr (cadr expr)))
            (:call "Runtime.MultipleValuesList1"))))

  ;; Shorthand
  (setf (gethash '1+ h)
        (lambda (expr)
          (cond
            ((and (= (length (cdr expr)) 1)
                  (fixnum-typed-p (cadr expr)))
             ;; Native: unbox arg, add 1, box
             `(,@(compile-as-long (cadr expr))
               (:ldc-i8 1) (:add)
               (:call "Fixnum.Make")))
            ((= (length (cdr expr)) 1)
             (compile-unary-call (cdr expr) "Runtime.Increment" "1+"))
            (t (compile-named-call '1+ (cdr expr))))))
  (setf (gethash '1- h)
        (lambda (expr)
          (cond
            ((and (= (length (cdr expr)) 1)
                  (fixnum-typed-p (cadr expr)))
             `(,@(compile-as-long (cadr expr))
               (:ldc-i8 1) (:sub)
               (:call "Fixnum.Make")))
            ((= (length (cdr expr)) 1)
             (compile-unary-call (cdr expr) "Runtime.Decrement" "1-"))
            (t (compile-named-call '1- (cdr expr))))))

  ;; Special forms
  (setf (gethash 'if h) (lambda (expr) (compile-if (cdr expr))))
  (setf (gethash 'when h) (lambda (expr) (compile-expr `(if ,(cadr expr) (progn ,@(cddr expr))))))
  (setf (gethash 'unless h) (lambda (expr) (compile-expr `(if ,(cadr expr) nil (progn ,@(cddr expr))))))
  (setf (gethash 'cond h) (lambda (expr) (compile-cond (cdr expr))))
  (setf (gethash 'and h) (lambda (expr) (compile-and (cdr expr))))
  (setf (gethash 'or h) (lambda (expr) (compile-or (cdr expr))))
  ;; progn preserves top-level-ness per CLHS 3.2.3.1
  (setf (gethash 'progn h) (lambda (expr) (let ((*at-toplevel* *compile-was-toplevel*)) (compile-progn (cdr expr)))))
  (setf (gethash 'let h) (lambda (expr) (compile-let (cadr expr) (cddr expr) nil)))
  (setf (gethash 'let* h) (lambda (expr) (compile-let-star (cadr expr) (cddr expr))))
  (setf (gethash 'setq h)
        (lambda (expr)
          (let ((pairs (cdr expr)))
            (if (null pairs)
                (emit-nil)
                (if (<= (length pairs) 2)
                    (compile-setq (first pairs) (second pairs))
                    (let ((forms '()))
                      (loop while pairs
                            do (push `(setq ,(pop pairs) ,(pop pairs)) forms))
                      (compile-progn (nreverse forms))))))))
  (setf (gethash 'lambda h) (lambda (expr) (compile-lambda (cadr expr) (cddr expr))))
  (setf (gethash 'funcall h) (lambda (expr) (compile-funcall (cdr expr))))
  (setf (gethash 'defun h)
        (lambda (expr)
          ;; In compile-file mode, ALSO evaluate the defun so the function
          ;; is callable during subsequent compile-time macro expansion in
          ;; the same file. SBCL does this; relying on it is the de-facto
          ;; convention for libraries like alexandria, where a macro body
          ;; uses a sibling defun (e.g., once-only calls make-gensym-list).
          ;; *at-toplevel* is reset to NIL inside compile-form, so we test
          ;; *compile-was-toplevel* (captured prior). try-eval prevents the
          ;; failure of one defun (e.g., references not-yet-defined fn) from
          ;; aborting compile-file as a whole.
          ;; Runtime.TryEval (the C# entry that this `try-eval` call resolves
          ;; to via cil-compiler.lisp's STRING= shortcut) binds
          ;; *compile-file-mode* to NIL during eval so the recursive
          ;; compile-form does NOT re-enter this branch and infinitely
          ;; recurse on the same defun (D857).
          (when (and *compile-was-toplevel* *compile-file-mode*
                     (not *cross-compiling*))
            (try-eval expr))
          (compile-defun (cadr expr) (caddr expr) (cdddr expr))))
  ;; %inline-cs-spliced: dotcl-cs:inline-cs macro expansion target.
  ;; Form: (%inline-cs-spliced ((arg1 arg2 ...)) :returns long ((:LDARG-0) ...))
  ;;   ARGN are Lisp value forms (compiled to LispObject Fixnum).
  ;;   The instr-list comes from (dotcl-cs:disassemble-cs ...) at macro
  ;;   expansion time — opcodes like (:LDARG-0) refer to the corresponding
  ;;   ARGN. We unbox each arg to int64, store in a fresh local, translate
  ;;   LDARG-N to ldloc that local, drop the trailing :RET, and box the
  ;;   final stack-top via Fixnum.Make. Only fixnum bindings + fixnum
  ;;   return are supported for the MVP (#122 Phase 2). Other types throw.
  ;; Use the DOTCL-INTERNAL symbol so Startup.Sym at runtime returns
  ;; the same Symbol instance that the macro-expanded user form refers to.
  (setf (gethash (intern "%INLINE-CS-SPLICED" "DOTCL-INTERNAL") h)
        (lambda (expr)
          (let* ((args (cadr expr))
                 (returns-kw (caddr expr))      ; :returns
                 (returns-type (cadddr expr))   ; long (must be :returns long)
                 (instrs (car (cddddr expr))))  ; the SIL instruction list
            (unless (eq returns-kw :returns)
              (error "%inline-cs-spliced: missing :returns keyword"))
            (unless (or (eq returns-type 'long) (string= (symbol-name returns-type) "LONG"))
              (error "%inline-cs-spliced: only :returns long supported (got ~S)" returns-type))
            (let* ((arg-locals (loop for i from 0 below (length args)
                                     collect (gen-local (format nil "INLINE_~A" i))))
                   (prelude '())
                   (translated '()))
              ;; Prelude: compile each arg, unbox to int64, stloc to local
              (loop for a in args
                    for tk in arg-locals
                    do (setf prelude
                             (append prelude
                                     `((:declare-local ,tk "Int64")
                                       ,@(let ((*in-tail-position* nil)
                                               (*in-mv-context* nil))
                                           (compile-expr a))
                                       (:unbox-fixnum)
                                       (:stloc ,tk)))))
              ;; Translate the SIL: LDARG-N → ldloc local-N, drop RET
              (dolist (ins instrs)
                (let ((op (car ins)))
                  (cond
                    ((member op '(:ldarg.0 :ldarg-0))
                     (push `(:ldloc ,(nth 0 arg-locals)) translated))
                    ((member op '(:ldarg.1 :ldarg-1))
                     (push `(:ldloc ,(nth 1 arg-locals)) translated))
                    ((member op '(:ldarg.2 :ldarg-2))
                     (push `(:ldloc ,(nth 2 arg-locals)) translated))
                    ((member op '(:ldarg.3 :ldarg-3))
                     (push `(:ldloc ,(nth 3 arg-locals)) translated))
                    ((eq op :ldarg)
                     (push `(:ldloc ,(nth (cadr ins) arg-locals)) translated))
                    ((member op '(:ldarg.s))
                     (push `(:ldloc ,(nth (cadr ins) arg-locals)) translated))
                    ((eq op :ret)
                     ;; Drop — we splice the result, caller boxes.
                     nil)
                    (t
                     (push ins translated)))))
              (append prelude
                      (nreverse translated)
                      ;; Box the final int64 to LispObject Fixnum
                      `((:call "Fixnum.Make")))))))
  (setf (gethash 'block h) (lambda (expr) (compile-block (cadr expr) (cddr expr))))
  (setf (gethash 'return-from h) (lambda (expr) (compile-return-from (cadr expr) (if (cddr expr) (caddr expr) nil))))
  (setf (gethash 'tagbody h) (lambda (expr) (compile-tagbody (cdr expr))))
  (setf (gethash 'go h) (lambda (expr) (compile-go (cadr expr))))
  (setf (gethash 'unwind-protect h) (lambda (expr) (compile-unwind-protect (cadr expr) (cddr expr))))
  (setf (gethash 'catch h) (lambda (expr) (compile-catch (cadr expr) (cddr expr))))
  (setf (gethash 'throw h) (lambda (expr) (compile-throw (cadr expr) (caddr expr))))
  (setf (gethash 'function h) (lambda (expr) (compile-function-special (cadr expr))))
  (setf (gethash 'flet h) (lambda (expr) (compile-flet (cadr expr) (cddr expr))))
  (setf (gethash 'labels h) (lambda (expr) (compile-labels (cadr expr) (cddr expr))))
  ;; macrolet and symbol-macrolet preserve top-level-ness per CLHS 3.2.3.1
  (setf (gethash 'macrolet h)
        (lambda (expr) (let ((*at-toplevel* *compile-was-toplevel*)) (compile-macrolet (cadr expr) (cddr expr)))))
  (setf (gethash 'symbol-macrolet h)
        (lambda (expr) (let ((*at-toplevel* *compile-was-toplevel*)) (compile-symbol-macrolet (cadr expr) (cddr expr)))))
  (setf (gethash 'define-symbol-macro h)
        (lambda (expr)
          (let ((name (cadr expr)) (expansion (caddr expr)))
            (setf (gethash name *global-symbol-macros*) expansion)
            (compile-expr `(%register-symbol-macro-rt ',name ',expansion)))))
  (dolist (sym '(defvar defparameter defconstant))
    (let ((sym sym))
      (setf (gethash sym h)
            (lambda (expr)
              (compile-defvar (cadr expr) (caddr expr) (not (null (cddr expr)))
                              (eq sym 'defvar) (eq sym 'defconstant) (cadddr expr))))))
  (setf (gethash 'the h) (lambda (expr) (compile-expr (caddr expr))))
  (setf (gethash 'load-time-value h)
        (lambda (expr)
          (let ((ltv-id (incf *ltv-counter*)))
            (compile-expr `(if (%has-ltv-slot ,ltv-id)
                               (%get-ltv-slot ,ltv-id)
                               (%set-ltv-slot ,ltv-id ,(cadr expr)))))))
  (setf (gethash 'declare h) (lambda (expr) (declare (ignore expr)) (emit-nil)))
  (setf (gethash 'declaim h)
        (lambda (expr)
          (let ((proclaim-forms nil))
            (dolist (spec (cdr expr))
              (cond
                ((and (consp spec) (eq (car spec) 'special))
                 (dolist (sym (cdr spec))
                   (pushnew sym *specials*)
                   (pushnew sym *global-specials*))
                 (push `(proclaim ',spec) proclaim-forms))
                ;; (ftype (function (arg-types...) return-type) name...)
                ((and (consp spec) (eq (car spec) 'ftype)
                      (consp (cadr spec))
                      (eq (car (cadr spec)) 'function)
                      (cddr (cadr spec)))
                 (let ((ret (car (last (cadr spec)))))
                   (dolist (name (cddr spec))
                     (when (symbolp name)
                       (setf (gethash name *function-return-types*) ret)))))))
            (if proclaim-forms
                (compile-progn (append (nreverse proclaim-forms) (list nil)))
                (emit-nil)))))
  ;; eval-when preserves top-level-ness per CLHS 3.2.3.1
  (setf (gethash 'eval-when h)
        (lambda (expr) (let ((*at-toplevel* *compile-was-toplevel*)) (compile-eval-when (cadr expr) (cddr expr)))))
  (setf (gethash 'multiple-value-call h)
        (lambda (expr)
          (let* ((fn-form (cadr expr)) (arg-forms (cddr expr)) (fn-var (gensym "MVC-FN")))
            (compile-expr
              `(let ((,fn-var ,fn-form))
                 (apply ,fn-var
                        (append ,@(mapcar (lambda (a) `(multiple-value-list ,a)) arg-forms))))))))
  (setf (gethash 'multiple-value-prog1 h)
        (lambda (expr)
          (let* ((first-form (cadr expr)) (rest-forms (cddr expr)) (mv-var (gensym "MVP1")))
            (compile-expr
              `(let ((,mv-var (multiple-value-list ,first-form)))
                 ,@rest-forms
                 (values-list ,mv-var))))))

  nil) ; end compile-form handler registration
