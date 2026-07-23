;;; cil-forms.lisp — Special forms, functions, control flow
;;; Part of the CIL compiler (A2 instruction list architecture)

(in-package :dotcl.cil-compiler)

;;; Per-compilation dynamic state (TCO state, typed-local tracking, etc.) is
;;; declared in cil-compiler.lisp — see the
;;; "--- Per-compilation dynamic state ---" section there.

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
   Used in native function bodies for TCO self-call argument evaluation."
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
                 ;; allow when the shadow IS the labels fn being compiled.
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
              ;; handler-case try/catch: use `leave` to exit cleanly.
              ;; try/finally (special-var LET) already suppressed above via *in-try-block*.
              ,(if *tco-in-try-catch*
                   `(:leave ,*tco-loop-label*)
                   `(:br ,*tco-loop-label*))))))
      ;; Mutual-TCO: tail call to a labels sibling → update shared params + br TCOLOOP
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
              ,(if (eq *self-fn-local* :arg0) '(:ldarg 0) `(:ldloc ,*self-fn-local*))
              ,@(unless skip-reset '((:call "MultipleValues.Reset")))
              ,@(loop for tmp in temps append `((:ldloc ,tmp)))
              (:callvirt ,(invoke-name n-args)))))))
    ;; Inline struct accessor: (accessor-name obj) → StructRefI with raw int index
    ;; Only when not shadowed by a local function (flet/labels)
    (when (and (symbolp name)
               (= (length args) 1)
               (not (assoc (mangle-name name) *local-functions* :test #'string=)))
      (let ((slot-idx (gethash name *struct-accessors*)))
        (when slot-idx
          (return-from compile-named-call
            `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                  (compile-expr (car args)))
              (:ldc-i4 ,slot-idx)
              (:call "Runtime.StructRefI"))))))
    ;; Inline CLOS simple-reader accessor: (reader obj) → Runtime.ReaderIC(obj, cell),
    ;; a per-call-site monomorphic inline cache. On a warm hit it reads the slot straight
    ;; from the instance vector (no GF resolution, no dispatch, no name→index lookup); any
    ;; miss re-resolves and anything that isn't a plain instance-slot simple reader falls
    ;; back to full dispatch. The DEFCLASS-populated registry is only a
    ;; compile-time hint that NAME is likely a reader; ReaderIC re-validates at run time
    ;; (SimpleReaderSlot flag + epoch), so a redefined/extended accessor stays correct.
    (when (and (symbolp name)
               (symbol-package name)
               (not *cross-compiling*)
               (= (length args) 1)
               (not (assoc (mangle-name name) *local-functions* :test #'string=))
               (gethash name *clos-accessor-readers*))
      (return-from compile-named-call
        `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
              (compile-expr (car args)))
          (:reader-ic ,(symbol-name name)
                      ,(package-name (symbol-package name))))))
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
                  (:callvirt ,(invoke-name n-args))))
              `((:declare-local ,args-tmp "LispObject[]")
                ,@(compile-args-array args) (:stloc ,args-tmp)
                ,@(if boxed-p
                      `((:ldloc ,key) (:ldc-i4 0) (:ldelem-ref))
                      `((:ldloc ,key)))
                (:castclass "LispFunction")
                ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                (:ldloc ,args-tmp)
                (:callvirt "LispFunction.Invoke"))))
        ;; Not a local function. In a Lisp-2 the operator position never names a
        ;; lexical variable, so a symbol that is a boxed local *variable*
        ;; (mutated + captured, hence living in a LispObject[1] cell) and happens
        ;; to share a global function's name must still call the GLOBAL function
        ;; here — it must NOT funcall the variable's cell. Boxed local *functions*
        ;; (flet/labels, including ones captured into a closure and re-established
        ;; in *local-functions*) are handled by the local-fn branch above. So
        ;; always take the global-function path.
        ;; Global function — use symbol-based lookup (fixes flat namespace
        ;; collision). A (setf SYM) name resolves via the TARGET symbol's
        ;; SetfFunction (symbol identity), NOT via the "(SETF NAME)" string path:
        ;; CilAssembler.GetFunction does a cross-package name search for (setf ...)
        ;; names, so a same-named accessor in another package (e.g. clump's
        ;; (setf parent) vs spatial-trees' (setf parent)) could be picked up by
        ;; iteration order, dispatching to the wrong GF.
        (let* ((setf-sym-p (and (consp name) (eq (car name) 'setf) (symbolp (cadr name))))
                     (load-fn (cond (setf-sym-p
                                     `(,@(compile-sym-lookup (cadr name))
                                       (:castclass "Symbol")
                                       (:call "CilAssembler.GetSetfFunctionBySymbol")))
                                    ((symbolp name)
                                     `(,@(compile-fn-sym-lookup name)
                                       (:castclass "Symbol")
                                       (:call "CilAssembler.GetFunctionBySymbol")))
                                    (t
                                     `((:ldstr ,(mangle-name name))
                                       (:call "CilAssembler.GetFunction"))))))
                (if (<= n-args 8)
                    (if (and (symbolp name) (every #'simple-expr-p args))
                        ;; Fast path: simple args → skip temps, push directly
                        `(,@load-fn
                          ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                          ,@(loop for arg in args
                                  append (let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr arg)))
                          (:callvirt ,(invoke-name n-args)))
                        (let* ((da (compile-direct-call-args args))
                               (temps (car da))
                               (eval-instrs (cdr da)))
                          `(,@eval-instrs
                            ,@load-fn
                            ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                            ,@(loop for tmp in temps append `((:ldloc ,tmp)))
                            (:callvirt ,(invoke-name n-args)))))
                    `((:declare-local ,args-tmp "LispObject[]")
                      ,@(compile-args-array args) (:stloc ,args-tmp)
                      ,@load-fn
                      ,@(unless skip-reset '((:call "MultipleValues.Reset")))
                      (:ldloc ,args-tmp)
                      (:callvirt "LispFunction.Invoke"))))))))


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
        ;; native r8 compare. Emitted as compile-double-cmp.
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
        ;; Fixnum-typed unary sign predicate → native i8 compare against 0.
        ;; (zerop (logand ...)) etc. lower the arg via compile-as-long, so a
        ;; native logand feeds ceq directly with no Fixnum.Make round-trip.
        ((and (= nargs 1)
              (member op '(zerop minusp plusp))
              (fixnum-typed-p (first args)))
         (list :fixnum-cmp
               (ecase op (zerop :eq) (minusp :lt) (plusp :gt))
               (list (first args) 0)))
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
       `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr expr))
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
             `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (cadr cond-expr)))
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
       `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr cond-expr))
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

;;; --- Oversized-progn chunking ---
;;;
;;; The CLR imposes a per-method IL size limit (a few MB for a DynamicMethod). A
;;; progn/implicit-progn body with very many forms — e.g. a data-driven test
;;; suite that bundles 1000+ assertions into one (progn (is ...) ...) — emits one
;;; oversized method that JITs to InvalidProgramException. Split such a body into
;;; groups, each wrapped in an immediately-called closure ((lambda () group...)),
;;; so every group compiles to its own method. Closures reuse the existing
;;; capture / boxing / non-local-exit machinery, so semantics are preserved.
;;;
;;; Only the leading forms are grouped; the last form stays inline so its tail
;;; position (self-TCO, multiple-value return) is unchanged.
;;;
;;; Refuse to split when a wrapped form could observe a semantic or codegen change:
;;;  - an enclosing lexical variable captured *by value* (non-boxed) and mutated
;;;    inside the progn — the closure would capture a stale copy; and
;;;  - any enclosing native raw-slot local (Int64 / native float / numeric array)
;;;    — a closure capture loads a raw value where an object is required (invalid
;;;    IL). Refusing here is cheap: a progn large enough to need splitting is never
;;;    a hot native numeric loop.

(defvar *progn-chunk-threshold* 256
  "Split a progn body into closures when it has more than this many forms.")
(defvar *progn-chunk-size* 64
  "Leading forms per closure chunk when splitting an oversized progn body.")

(defun %list-longer-than-p (list n)
  "T if LIST has more than N proper elements. Tolerant of an improper (dotted)
   tail — compile-progn is occasionally handed one (e.g. tagbody segments), and
   LENGTH would signal on it where the LAST/BUTLAST path below does not."
  (do ((tail list (cdr tail))
       (i 0 (1+ i)))
      ((not (consp tail)) nil)
    (when (> i n) (return t))))

(defun %group-forms (forms n)
  "Partition FORMS into consecutive groups of at most N."
  (loop for tail = forms then (nthcdr n tail)
        while tail
        collect (loop repeat n for x in tail collect x)))

(defun %wrap-chunk (group)
  "Wrap GROUP (a list of forms) as an immediately-applied nullary lambda
   ((lambda () form...)) — compiled to its own method, so the group's CIL lands
   outside the enclosing method's IL budget."
  (list (list* 'lambda nil group)))

(defun %chunk-progn-forms (forms)
  "Rewrite oversized FORMS into leading closure chunks plus the inline last form."
  (let ((head (butlast forms))
        (last-form (car (last forms))))
    (append (mapcar #'%wrap-chunk (%group-forms head *progn-chunk-size*))
            (list last-form))))

(defun %progn-chunk-safe-p (forms)
  "T if the enclosing scope permits wrapping FORMS in closures without changing
   semantics or emitting invalid IL — see the section comment."
  (and (null *long-locals*)
       (null *native-double-locals*)
       (null *native-single-locals*)
       (null *numeric-array-locals*)
       (let ((value-captured
               (loop for pair in *locals*
                     for sym = (car pair)
                     unless (boxed-var-p sym) collect (var-name sym))))
         (or (null value-captured)
             (null (intersection
                    (nth-value 0 (find-mutated-and-captured-vars forms value-captured))
                    value-captured :test #'string=))))))

(defun compile-progn (forms)
  (cond
    ((null forms) (emit-nil))
    ;; Single form — inherits *in-tail-position* from caller
    ((null (cdr forms)) (compile-expr (car forms)))
    ;; Oversized non-toplevel body: split into closure chunks (below). A TOPLEVEL
    ;; progn must NOT be chunked — its forms are definitions (defun/defvar/
    ;; defmacro/eval-when) whose top-level processing (compile-time side effects,
    ;; :toplevel-boundary, macro registration) would be lost inside a lambda. The
    ;; whole dotcl core is cross-compiled as one giant toplevel progn, so chunking
    ;; it corrupts everything (e.g. the loop keyword universe). Toplevel progns
    ;; already have their own :toplevel-boundary splitting.
    ((and (not *at-toplevel*)
          (%list-longer-than-p forms *progn-chunk-threshold*)
          (%progn-chunk-safe-p forms))
     (compile-progn (%chunk-progn-forms forms)))
    (t (append
        ;; Non-last forms: never in tail position, and their values are
        ;; discarded so an MV-propagating context (e.g. block value position)
        ;; must not leak into them — subforms would skip UnwrapMv and leak
        ;; raw MvReturn objects into single-value positions.
        (let ((*in-tail-position* nil)
              (*in-mv-context* nil))
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
      ;; Symbol (not LispObject): the symbol is castclass'd to Symbol below, and
      ;; DynamicBindings.Set's first parameter is Symbol. Declaring the local as
      ;; LispObject would widen it back, emitting an unverifiable covariant call
      ;; (LispObject passed where Symbol is required) that CoreCLR/NativeAOT JIT
      ;; tolerate but Unity IL2CPP's strict C++ codegen rejects. All other uses of
      ;; this local pass it to LispObject parameters, where Symbol upcasts cleanly.
      (:declare-local ,sym-local "Symbol")
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
    ;; (quote X) is data, not code — never descend. X may be a circular literal
    ;; (e.g. '#1=(1 2 3 . #1#)); walking it would loop forever.
    ((eq (car form) 'quote) nil)
    ;; Recurse into subforms (handle dotted pairs safely)
    (t (do ((cur form (cdr cur)))
           ((atom cur) (when cur (form-has-return-from-p name cur)))
         (when (form-has-return-from-p name (car cur))
           (return t))))))

(defun form-macroexpands-to-return-from-p (name form depth)
  "Like FORM-HAS-RETURN-FROM-P, but also expands global macro calls (up to
   *macro-expand-depth-limit*) so a (return-from NAME) produced by a macro is
   found even when the macro call is nested inside a special form such as PROGN
   (dotcl/dotcl issue 51: the top-level-only scan missed `(progn (some-macro))`).
   Scans BOTH the expansion and the original subforms, so it never misses a
   return-from and only ever over-keeps the implicit block (safe). Conservative:
   answers T if a macro expander errors. Uses cached-macroexpand, so the pre-pass
   expansion is shared with code-gen (identical gensyms).
   DEPTH is the current macro-expansion depth; entry callers pass 0. It is a
   required parameter (not &optional) so the compiler gives this hot recursive
   tree-walker a required-only lambda list, which enables the direct-delegate
   fast path instead of the args-array entry."
  (cond
    ((atom form) nil)
    ((and (eq (car form) 'return-from) (consp (cdr form)) (eq (cadr form) name)) t)
    ;; nested block/defun shadowing NAME — its own return-from is not ours
    ((and (eq (car form) 'block) (consp (cdr form)) (eq (cadr form) name)) nil)
    ((and (eq (car form) 'defun) (consp (cdr form)) (eq (cadr form) name)) nil)
    ((eq (car form) 'quote) nil)
    (t
     (or
      ;; A global macro call may expand to a return-from (possibly after more
      ;; expansions). Local macrolet macros are handled separately by
      ;; form-has-macrolet-p (they are not in the global *macros* table).
      (and (symbolp (car form))
           (< depth *macro-expand-depth-limit*)
           (gethash (car form) *macros*)
           (handler-case
               (let ((expanded (cached-macroexpand form (gethash (car form) *macros*))))
                 (and (not (equal expanded form))
                      (form-macroexpands-to-return-from-p name expanded (1+ depth))))
             (error () t)))
      ;; Structural recursion into the original subforms (macro arguments may
      ;; themselves contain a return-from).
      (do ((cur form (cdr cur)))
          ((atom cur) (when cur (form-macroexpands-to-return-from-p name cur depth)))
        (when (form-macroexpands-to-return-from-p name (car cur) depth)
          (return t)))))))

(defun form-has-macrolet-p (form)
  "T if FORM contains a MACROLET / SYMBOL-MACROLET anywhere in the tree. Such a
   local macro can expand to (return-from <enclosing-defun> …) — which
   form-has-return-from-p cannot see, because the expander body is a quasiquoted
   template, not a literal return-from. When present we conservatively keep the
   defun's implicit block wrapper (disable the use-direct fast path) so the
   generated return-from resolves."
  (cond
    ((atom form) nil)
    ((eq (car form) 'quote) nil)
    ((member (car form) '(macrolet symbol-macrolet)) t)
    (t (do ((cur form (cdr cur)))
           ((atom cur) nil)
         (when (form-has-macrolet-p (car cur)) (return t))))))

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

(defvar *sil-dump-pattern* :unread
  "Cached upcased DOTCL_DUMP_SIL_FOR value (or NIL if unset); :UNREAD before the
   first compiled defun reads the env var. See %maybe-dump-defun-sil.")

(defun %maybe-dump-defun-sil (name result)
  "Diagnostic: if the env var DOTCL_DUMP_SIL_FOR is set to a substring that
   NAME contains (case-insensitive), print RESULT — the emitted instruction list,
   which carries the :body SIL — to *error-output*, bracketed by ;;DUMP-SIL /
   ;;END-DUMP-SIL markers. Lets a state-dependent miscompile be inspected as
   actually emitted during a full build (make-host-1), where the compiled fasl
   retains no SIL. Gated by an env var (not a special var) so it is stable against
   cross-compiled-vs-reader symbol identity and needs no in-image setter: run e.g.
   DOTCL_DUMP_SIL_FOR=%DEFKNOWN before the build. Returns RESULT."
  ;; The pattern is read from the env var once and cached (both write and read are
  ;; compiler-internal, so *sil-dump-pattern* has stable identity). Skipped during
  ;; cross-compile: this fn runs on the SBCL host then, where %getenv is not a host
  ;; function; the (%getenv ...) call is only reached at runtime, where it compiles
  ;; to the Runtime.Getenv intrinsic. Caching keeps the common (unset) case to one
  ;; env read for the whole process instead of one per compiled defun.
  (when (eq *sil-dump-pattern* :unread)
    (setf *sil-dump-pattern*
          (unless *cross-compiling*
            (let ((pat (%getenv "DOTCL_DUMP_SIL_FOR")))
              (and (stringp pat) (> (length pat) 0) (string-upcase pat))))))
  (when *sil-dump-pattern*
    (let ((nm (cond ((consp name) (format nil "~S" name))
                    ((symbolp name) (symbol-name name))
                    (t nil))))
      (when (and nm (search *sil-dump-pattern* (string-upcase nm)))
        (format *error-output* "~&;;DUMP-SIL ~A~%~S~%;;END-DUMP-SIL~%" nm result)
        (finish-output *error-output*))))
  result)

(defun compile-defun (name params body)
  "Compile (defun name (params) body...) → :defmethod directive + return symbol."
  ;; Pre-pass: infer return type before body compilation so self-calls inside the
  ;; body benefit from the single-value elision path.
  (when (and (symbolp name) (not (gethash name *function-return-types*)))
    (let ((inferred (infer-body-return-type body (mangle-name name))))
      (when inferred
        (setf (gethash name *function-return-types*) inferred))))
  (multiple-value-bind (required optional key rest-param) (parse-lambda-list params)
    (let* ((param-names (mapcar #'var-name required))
           ;; Every variable the lambda list binds in the body. The analysis
           ;; context below must report ALL of them as bound — not just the
           ;; required ones — or a (declare (fixnum X)) on an &optional/&key
           ;; param never satisfies fixnum-typed-p during the analysis pass and
           ;; the cached dotimes expansion misses its fixnum declaration (the
           ;; cl-bench array functions take (&optional (size 2000)) and lost
           ;; the entire native loop/aref path this way).
           (all-param-vars (append required
                                   (mapcar #'car optional)
                                   (remove nil (mapcar #'third optional))
                                   (mapcar #'second key)
                                   (remove nil (mapcar #'fourth key))
                                   (when rest-param (list rest-param))))
           (block-name (cond ((and (consp name) (eq (car name) 'setf)) (cadr name))
                             ((consp name) (cadr name)) ; (cas foo) → block named foo
                             (t name)))
           ;; Establish the enclosing function's numeric type-declaration context
           ;; for the macro expansions performed during this analysis pass. The
           ;; *macroexpand-cache* shares ONE expansion between the analysis and
           ;; the later code-gen pass (so gensyms stay identical). A context-
           ;; dependent macro such as dotimes consults fixnum-typed-p on its
           ;; count-form to decide whether to inject (declare (fixnum ...)); if
           ;; the analysis expansion runs with these vars unbound, the cached
           ;; form lacks the declaration and the native int64 loop path can never
           ;; fire at code-gen. Bind them (and a dummy *locals* so lookup-local
           ;; reports params as bound) only around the analysis computations —
           ;; the dummy keys never reach emission. Code-gen re-validates each
           ;; declaration against the real boxed/captured-var context, so an
           ;; over-eager declaration on a captured var is harmlessly ignored.
           (analysis-context-vals
            (let ((*fixnum-locals* (append (extract-fixnum-locals body) *fixnum-locals*))
                  (*small-int-locals* (append (extract-small-int-locals body) *small-int-locals*))
                  (*double-float-locals* (append (extract-double-float-locals body)
                                                 *double-float-locals*))
                  (*single-float-locals* (append (extract-single-float-locals body)
                                                 *single-float-locals*))
                  (*decimal-locals* (append (extract-decimal-locals body)
                                            *decimal-locals*))
                  (*locals* (append (mapcar (lambda (p) (cons p (var-name p)))
                                            all-param-vars)
                                    *locals*)))
              (cons
               (or ;; Detect (return-from block-name …) anywhere in the body,
                   ;; expanding global macro calls at any depth (a macro nested in
                   ;; a progn/let/etc. can expand to one — dotcl/dotcl issue 51).
                   (some (lambda (f) (form-macroexpands-to-return-from-p block-name f 0)) body)
                   ;; A MACROLET / SYMBOL-MACROLET local macro can also expand to
                   ;; (return-from block-name …) via its quasiquoted template, and
                   ;; its expander is lexical (not in the global *macros* table), so
                   ;; the scan above cannot expand it. Keep the implicit block
                   ;; wrapper conservatively when present.
                   (some #'form-has-macrolet-p body))
               (find-free-vars-with-defaults params body))))
           (has-literal-return-from (car analysis-context-vals))
           ;; Check for free variables from original body (block wrapper doesn't add free vars)
           (free-vars (cdr analysis-context-vals))
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
      ;; Also handle (setf gensym): set SetfFunction on the uninterned gensym.
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
          (%maybe-dump-defun-sil name
          (cond
            ;; Closure defun (free vars captured), OR a NON-top-level defun (nested
            ;; inside a conditional / other form): register the function at RUNTIME
            ;; via compile-lambda + RegisterFunctionOnSymbol, executed only when the
            ;; form is actually reached. The :defmethod path below registers at
            ;; ASSEMBLY time (even inside an untaken if-branch — CLAUDE.md), so a
            ;; guarded defun like (unless (fboundp 'x) (defun x …)) or (if nil
            ;; (defun x …)) would define X unconditionally, breaking cross-file
            ;; defdfun defaults on fresh compile.
            ((or free-vars (not *compile-was-toplevel*))
             (if (symbolp name)
                 (if (symbol-package name)
                     `(,@(compile-sym-lookup name)
                       ,@(compile-lambda params wrapped-body "" (mangle-name name))
                       (:castclass "LispFunction")
                       (:call "CilAssembler.RegisterFunctionOnSymbol")
                       ,@(compile-sym-lookup name))
                     ;; Uninterned (gensym) name: compile-sym-lookup would intern a
                     ;; FRESH symbol by name (registering the fn on the wrong object,
                     ;; and the mangled-name uninterned-fixup does not apply on the
                     ;; runtime-registration path). Load the actual gensym object and
                     ;; register directly on it. (fix fallout; ANSI DEFUN.ERROR.4
                     ;; eval's a prog2-nested (defun #:g …).)
                     `((:load-const ,name) (:castclass "Symbol")
                       ,@(compile-lambda params wrapped-body "" (mangle-name name))
                       (:castclass "LispFunction")
                       (:call "CilAssembler.RegisterFunctionOnSymbol")
                       (:load-const ,name)))
                 ;; (setf target): register on the target symbol's SetfFunction
                 ;; slot, package-aware. RegisterFunction resolved the name via
                 ;; Startup.Sym on the mangled string, which interns TARGET in the
                 ;; wrong package (not the reader's symbol), so #'(setf target) /
                 ;; (fboundp '(setf target)) could not find it — e.g. a non-top-level
                 ;; (defun (setf acc) …) inside report-and-ignore-errors (fix
                 ;; fallout; ANSI FBOUNDP.6 / FUNCTION.7). An uninterned (setf gensym)
                 ;; target still goes through uninterned-fixup below.
                 (if (and (consp name) (eq (car name) 'setf) (symbolp (cadr name))
                          (symbol-package (cadr name)))
                     `(,@(compile-sym-lookup (cadr name))
                       (:castclass "Symbol")
                       ,@(compile-lambda params wrapped-body "" (mangle-name name))
                       (:castclass "LispFunction")
                       (:call "CilAssembler.RegisterSetfFunctionOnSymbol")
                       ,@uninterned-fixup
                       ,@(compile-quoted name))
                     `((:ldstr ,(mangle-name name))
                       ,@(compile-lambda params wrapped-body "" (mangle-name name))
                       (:castclass "LispFunction")
                       (:call "CilAssembler.RegisterFunction")
                       ,@uninterned-fixup
                       (:ldstr ,(mangle-name name)) (:call "Startup.Sym")))))
            ;; Direct params: simple required-only functions
            (use-direct
             (let* ((mangled (mangle-name name))
                    (pkg-name (cadr pkg-spec))
                    ;; Native eligibility: all fixnum params, fixnum return, ≤4 params,
                    ;; no special-declared params
                    (native-eligible
                      (and (symbolp name)
                           (<= (length required) 4)
                           (all-params-fixnum-p params wrapped-body)
                           (eq 'fixnum (gethash name *function-return-types*))
                           (null (fn-body-special-params wrapped-body
                                                         (mapcar #'var-name required)))
                           (null (remove-if-not #'global-special-p required)))))
               (multiple-value-bind (direct-body direct-self-p)
                   (compile-function-body-direct params wrapped-body mangled pkg-name name)
                 `(,(if native-eligible
                        `(:defmethod-native ,mangled
                           ,@pkg-spec
                           :params ,param-names
                           :body ,direct-body)
                        ;; :self t — self-recursive non-native direct fn: the
                        ;; backend gives the direct method a leading LispFunction self
                        ;; param (threaded for non-tail self-calls, no per-entry lookup).
                        `(:defmethod-direct ,mangled
                           ,@pkg-spec
                           ,@(when direct-self-p '(:self t))
                           :params ,param-names
                           :body ,direct-body))
                   ,@uninterned-fixup
                   ,@(if (symbolp name)
                         (compile-sym-lookup name)
                         ;; A non-symbol function name — e.g. (SETF foo) — is
                         ;; returned by DEFUN as the name itself (the list
                         ;; (SETF foo)), NOT a symbol interned from its mangled
                         ;; string. Emitting the mangled symbol broke e.g.
                         ;; (multiple-value-list (eval `(defun (setf ,g) ...)))
                         ;; (ANSI DEFINE-COMPILER-MACRO.4).
                         (compile-quoted name))))))
            ;; Standard defmethod
            (t
              `((:defmethod ,(mangle-name name)
                 ,@pkg-spec
                 :params ,param-names
                 :body ,(compile-function-body params wrapped-body (mangle-name name)))
                ,@uninterned-fixup
                ,@(if (symbolp name)
                      (compile-sym-lookup name)
                      (compile-quoted name)))))))))))

(defun compile-defmacro (name lambda-list body)
  "Compile (defmacro name lambda-list body...).
   Uses SBCL's eval to create macro function at compile time.
   At runtime, just returns the macro name as a symbol."
  ;; Package lock check: only fire when the macro isn't already defined.
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
         ;; 1-arg (compile-time eval): the compiler expands via this expander, so
         ;; bind &environment to a REIFIED environment reflecting the current
         ;; lexical macro / symbol-macro scope — a (macros-ht . symbol-macros-ht)
         ;; cons that macroexpand-1/macroexpand read (mirrors the macrolet env
         ;; builder, cil-compiler.lisp). Previously this was NIL, so a macro doing
         ;; (macroexpand-1 'sym env) inside a symbol-macrolet saw no binding —
         ;; e.g. serapeum with-boolean's %all-branches% channel. *macros*/
         ;; *symbol-macros* are read at EXPANSION time, so they carry the scope
         ;; active when the macro call is compiled.
         (wrapped-body-1arg (if env-var
                                `((let ((,env-var (%reify-macro-environment)))
                                    ,@body))
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
         ;; 2-arg (runtime): env-var bound to the actual env argument (CLHS 3.4.4).
         ;; %register-macro-function-rt overwrites the *macros* entry with a wrapper
         ;; that calls this 2-arg expander with a NIL env (Runtime.Misc.cs), so at
         ;; same-session compile the macro would see no lexical scope. When the
         ;; passed env is NIL, reify the compiler's current macro / symbol-macro
         ;; scope so (macroexpand-1 'sym env) inside a symbol-macrolet expands —
         ;; e.g. serapeum with-boolean's %all-branches% channel.
         (env-sym (gensym "ENV"))
         (wrapped-body-2arg (if env-var
                                `((let ((,env-var (or ,env-sym (%reify-macro-environment))))
                                    ,@body))
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
             ;; Skip %register-struct-class: dotcl runtime function absent in SBCL.
             ;; During compile-file, the cross-package bridge resolves it correctly.
             (unless (and (consp form)
                          (symbolp (car form))
                          (string= (symbol-name (car form)) "%REGISTER-STRUCT-CLASS"))
               (eval form)))))
        (*compile-file-mode*
         (when ct-p
           (let ((*compile-file-mode* nil))
             (dolist (form body)
               (eval form)))))
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
    ;; Per CLHS 3.2.3.1 (Figure 3-7):
    ;; - compile-file at top level: emit for load iff :load-toplevel is present.
    ;;   A bare {:execute} eval-when at top level of a compiled file is DISCARDED
    ;;   (its body is neither evaluated at compile time nor placed in the fasl),
    ;;   so emitting on ex-p here was wrong (leaked :execute-only bodies into the
    ;;   fasl load code — see EVAL-WHEN.1).
    ;; - Cross-compiling (dotcl bootstrap convention): :load-toplevel OR :execute
    ;;   at top level → emit CIL. The core build relies on this; keep it.
    ;; - Load/eval (not cross-compiling, not compile-file): only :execute → emit CIL.
    ;; - Not at top level: only :execute → emit body as implicit progn.
    (if (cond
          (*cross-compiling*
           (if *at-toplevel* (or lt-p ex-p) ex-p))
          (*compile-file-mode*
           (if *at-toplevel* lt-p ex-p))
          (t ex-p))
        (compile-progn body)
        (emit-nil))))

(defun unwrap-body-wrappers (body)
  "Peel nested single-form (block name ...) and (let* bindings ...) wrappers to
   reach the leading declarations. &aux + a defun block produce
   (block name (let* aux ...)), so both levels must be unwrapped."
  (loop while (and (= (length body) 1)
                   (consp (car body))
                   (member (caar body) '(block let*)))
        do (setf body (cddar body)))
  body)

(defun fn-body-special-params (body all-param-names)
  "Find function parameters declared special in the function body.
   Handles body possibly wrapped in (block name ...).
   ALL-PARAM-NAMES is a list of symbol-name strings."
  (let ((inner-forms (unwrap-body-wrappers body)))
    (multiple-value-bind (specials _rest) (extract-specials inner-forms)
      (declare (ignore _rest))
      (remove-if-not (lambda (s)
                       (member (var-name s) all-param-names :test #'string=))
                     specials))))

(defun special-param-name-set (body all-params)
  "Names (strings) of ALL-PARAMS that are special — either declared special in
   BODY or globally proclaimed (defvar/defparameter/proclaim/defmvar).
   Special variables are accessed through the dynamic-binding stack, never
   through a lexical box, so they must be excluded from capture-driven boxing
   (a boxed special would store the LispObject[] box where a value is expected)."
  (mapcar #'var-name
          (union (fn-body-special-params body (mapcar #'var-name all-params))
                 (remove-if-not #'global-special-p all-params))))

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
   Used to determine native-eligibility for the native self-call path."
  (multiple-value-bind (required optional key rest-param) (parse-lambda-list params)
    (and (null optional) (null key) (null rest-param)
         (not (null required))
         (let ((fxlocals (extract-fixnum-locals body)))
           (every (lambda (p) (member (var-name p) fxlocals :test #'string=))
                  required)))))

(defun %sil-references-local-p (tree key)
  "Return T if the SIL TREE contains an instruction (:ldloc KEY).
   Used to detect whether a non-native direct body actually performs a
   non-tail self-call (the only producer of (:ldloc *self-fn-local*)).
   Walks via a local recursion that visits car and cdr as separate
   statements (NOT `(or (f car) (f cdr))`): the latter mixes a non-tail
   self-call with a tail self-call in one form and the tail call's TCO
   loop drops the non-tail call's result on the long body spine."
  (labels ((walk (x)
             (when (consp x)
               (cond
                 ((and (eq (car x) :ldloc) (consp (cdr x)) (eq (cadr x) key))
                  (return-from %sil-references-local-p t))
                 ;; (:load-const OBJ) embeds a quoted data literal that may be
                 ;; circular (e.g. '#1=(1 2 3 . #1#)); it is data, never SIL,
                 ;; so it can't contain (:ldloc KEY). Don't descend — walking a
                 ;; cyclic constant loops forever.
                 ((eq (car x) :load-const) nil)
                 (t (walk (car x)) (walk (cdr x)))))))
    (walk tree)
    nil))

(defun %sil-subst-self-arg0 (tree key)
  "Replace every (:ldloc KEY) leaf in SIL TREE with (:ldarg 0). KEY is the
   unique self-fn gen-local, so this rewrites exactly the non-tail self-call
   receivers to read the self LispFunction threaded in as arg0."
  (cond ((atom tree) tree)
        ((and (eq (car tree) :ldloc) (consp (cdr tree))
              (eq (cadr tree) key) (null (cddr tree)))
         '(:ldarg 0))
        ;; (:load-const OBJ) holds an opaque (possibly circular) data literal —
        ;; never a self-call receiver. Leave it untouched, don't recurse.
        ((eq (car tree) :load-const) tree)
        (t (let ((a (%sil-subst-self-arg0 (car tree) key))
                 (d (%sil-subst-self-arg0 (cdr tree) key)))
             (if (and (eq a (car tree)) (eq d (cdr tree))) tree (cons a d))))))

;;; --- Shared context-construction helpers for the function-body compilers ---
;;; (verbatim extractions of blocks that were identical in
;;; compile-function-body-direct and compile-function-body-inner)

(defun gen-param-local-keys (all-params)
  "Fresh gen-local key per param: alist (param . key)."
  (mapcar (lambda (p) (cons p (gen-local (var-name p)))) all-params))

(defun params-needs-boxing (body all-params)
  "Names of params that must be boxed: mutated AND captured in BODY (single
   walk), minus special params — those are bound on the dynamic
   stack, not in a box."
  (let* ((mc (multiple-value-list
              (find-mutated-and-captured-vars body (mapcar #'var-name all-params))))
         (mutated (first mc))
         (captured (second mc)))
    (set-difference
     (intersection mutated captured :test #'string=)
     (special-param-name-set body all-params) :test #'string=)))

(defun params-shadowed-symbol-macros (all-params)
  "Value for rebinding *symbol-macros* in a function body: lambda-list
   parameters shadow an enclosing symbol-macro of the same name (CLHS 3.4.2),
   including inside nested lambdas. Without this a self-referential
   symbol-macro whose name is a param — e.g. serapeum define-env-method's
   (self (slot-value self 'self)) — is still live when a NESTED lambda's
   free-var scan reaches the name, expanding it forever (compile hang)."
  (let ((param-names (mapcar #'var-name all-params)))
    (remove-if (lambda (entry)
                 (let ((k (car entry)))
                   (member (if (symbolp k) (var-name k) "")
                           param-names :test #'string=)))
               *symbol-macros*)))

(defun params-boxed-vars (all-params needs-boxing)
  "Value for rebinding *boxed-vars*: the param objects (from ALL-PARAMS)
   whose names are in NEEDS-BOXING."
  (mapcar (lambda (name) (find name all-params :key #'var-name :test #'string=))
          needs-boxing))

(defun compile-args-param-instrs (required optional key-specs rest-param
                                  locals-alist n-required key-start
                                  arg-elem-fn args-array-instrs
                                  defaults-locals-fn)
  "Parameter-binding instructions for an args-array function body. Shared by
   compile-function-body-inner and compile-closure-body, which differ only in
   three context points:
   ARG-ELEM-FN — (lambda (i) instrs) pushing args[i] (inner: ldarg
     args-arg-idx + ldelem; closure: :load-arg, which the assembler maps per
     closure mode).
   ARGS-ARRAY-INSTRS — instrs pushing the args array itself (for ldlen /
     FindKeyArg / CollectRestArgs).
   DEFAULTS-LOCALS-FN — (lambda (remaining-param-names) locals-value): the
     *locals* in effect while compiling an &optional/&key default form. Per
     CL, the current and all not-yet-initialized params must not be visible
     (a default referencing a later param name as a special variable must
     read the dynamic binding); the closure variant also keeps env captures
     visible so defaults see the outer scope."
  (append
   ;; Required params: load from args[i]
   (loop for p in required
         for key = (cdr (assoc p locals-alist))
         for i from 0
         if (boxed-var-p p)
           append `((:declare-local ,key "LispObject[]")
                    (:ldc-i4 1) (:newarr "LispObject") (:dup)
                    (:ldc-i4 0) ,@(funcall arg-elem-fn i)
                    (:stelem-ref) (:stloc ,key))
         else
           append `((:declare-local ,key "LispObject")
                    ,@(funcall arg-elem-fn i)
                    (:stloc ,key)))
   ;; Optional params: check args.Length (with boxing & supplied-p support)
   (let ((opt-instrs nil)
         (remaining-opt-names (mapcar #'car optional)))
     (loop for (opt-name opt-default sp-var) in optional
           for i from n-required
           for key = (cdr (assoc opt-name locals-alist))
           do ;; Mask current+later opt params while compiling the default
              (let* ((*locals* (funcall defaults-locals-fn
                                        (mapcar #'var-name remaining-opt-names)))
                     (default-label (gen-label "OPTDEF"))
                     (done-label (gen-label "OPTDONE")))
                (setq opt-instrs
                      (append opt-instrs
                              (if (boxed-var-p opt-name)
                                  (let ((tmp (gen-local "OPTTMP")))
                                    `((:declare-local ,tmp "LispObject")
                                      ,@args-array-instrs (:ldlen) (:conv-i4)
                                      (:ldc-i4 ,(1+ i))
                                      (:blt ,default-label)
                                      ,@(funcall arg-elem-fn i)
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
                                    ,@args-array-instrs (:ldlen) (:conv-i4)
                                    (:ldc-i4 ,(1+ i))
                                    (:blt ,default-label)
                                    ,@(funcall arg-elem-fn i)
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
                  (let ((sp-key (cdr (assoc sp-var locals-alist)))
                        (sp-found-label (gen-label "OPTSPF"))
                        (sp-done-label (gen-label "OPTSPD")))
                    (setq opt-instrs
                          (append opt-instrs
                                  `((:declare-local ,sp-key "LispObject")
                                    ,@args-array-instrs (:ldlen) (:conv-i4)
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
         (remaining-key-vars (mapcar #'second key-specs)))
     (dolist (key-spec key-specs)
       (let* ((key-name (first key-spec))
              (var-name (second key-spec))
              (key-default (third key-spec))
              (sp-var (fourth key-spec))
              (explicit-key-pkg (fifth key-spec))
              (find-key-fn (if explicit-key-pkg "Runtime.FindKeyArgByName" "Runtime.FindKeyArg"))
              (key (cdr (assoc var-name locals-alist)))
              (found-label (gen-label "KEYFOUND"))
              (done-label (gen-label "KEYDONE")))
         ;; Key param binding: mask current+later key params while compiling
         ;; the default (see DEFAULTS-LOCALS-FN above)
         (let ((default-instrs
                 (if key-default
                     (let ((*locals* (funcall defaults-locals-fn
                                              (mapcar #'var-name remaining-key-vars))))
                       (compile-expr key-default))
                     (emit-nil))))
           (setq key-instrs
                 (append key-instrs
                         (if (boxed-var-p var-name)
                             (let ((tmp (gen-local "KEYTMP")))
                               `((:declare-local ,tmp "LispObject")
                                 ,@args-array-instrs (:ldc-i4 ,key-start)
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
                               ,@args-array-instrs (:ldc-i4 ,key-start)
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
           (let ((sp-key (cdr (assoc sp-var locals-alist)))
                 (sp-found-label (gen-label "SPFOUND"))
                 (sp-done-label (gen-label "SPDONE")))
             (setq key-instrs
                   (append key-instrs
                           `((:declare-local ,sp-key "LispObject")
                             ,@args-array-instrs (:ldc-i4 ,key-start)
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
     (let ((key (cdr (assoc rest-param locals-alist)))
           (n key-start))
       (if (boxed-var-p rest-param)
           `((:declare-local ,key "LispObject[]")
             (:ldc-i4 1) (:newarr "LispObject") (:dup)
             (:ldc-i4 0) ,@args-array-instrs (:ldc-i4 ,n) (:call "Runtime.CollectRestArgs")
             (:stelem-ref) (:stloc ,key))
           `((:declare-local ,key "LispObject")
             ,@args-array-instrs (:ldc-i4 ,n) (:call "Runtime.CollectRestArgs")
             (:stloc ,key)))))))

(defun compile-args-arity-instrs (fn-name optional key-specs rest-param has-key-p
                                  n-required args-array-instrs)
  "Arity-check prefix for an args-array function body (shared inner/closure).
   The :direct closure case (arity check omitted) is handled by the caller."
  (cond
    ((and (null optional) (null key-specs) (null rest-param) (not has-key-p))
     ;; Required-only: exact arity check
     `((:ldstr ,fn-name) ,@args-array-instrs (:ldc-i4 ,n-required)
       (:call "Runtime.CheckArityExact")))
    ((and optional (null key-specs) (null rest-param) (not has-key-p))
     ;; Optional-only: min + max check
     (let ((n-max (+ n-required (length optional))))
       `((:ldstr ,fn-name) ,@args-array-instrs (:ldc-i4 ,n-required)
         (:call "Runtime.CheckArityMin")
         (:ldstr ,fn-name) ,@args-array-instrs (:ldc-i4 ,n-max)
         (:call "Runtime.CheckArityMax"))))
    (t
     ;; Has key/rest: minimum arity check only
     `((:ldstr ,fn-name) ,@args-array-instrs (:ldc-i4 ,n-required)
       (:call "Runtime.CheckArityMin")))))

(defun compile-args-key-check-instrs (fn-name key-specs has-key-p allow-other-keys-p
                                      key-start args-array-instrs)
  "Unknown-keyword check for an args-array function body (shared inner/
   closure). Fires for any &key lambda list (even a bare &key with no key
   params — hence HAS-KEY-P) without &allow-other-keys; explicit-package
   keyword names compare via the package-aware CheckNoUnknownKeys2."
  (when (and has-key-p (not allow-other-keys-p))
    (let* ((n-keys (length key-specs))
           (has-explicit (some #'fifth key-specs)))
      `((:ldstr ,fn-name)
        ,@args-array-instrs (:ldc-i4 ,key-start)
        (:ldc-i4 ,n-keys) (:newarr "String")
        ,@(loop for i from 0
                for key-spec in key-specs
                append `((:dup) (:ldc-i4 ,i) (:ldstr ,(first key-spec)) (:stelem-ref)))
        ,@(if has-explicit
              ;; Pass package array for explicit key matching
              `((:ldc-i4 ,n-keys) (:newarr "String")
                ,@(loop for i from 0
                        for key-spec in key-specs
                        append (let ((pkg (fifth key-spec)))
                                 (if pkg
                                     `((:dup) (:ldc-i4 ,i) (:ldstr ,pkg) (:stelem-ref))
                                     `())))
                (:call "Runtime.CheckNoUnknownKeys2"))
              `((:call "Runtime.CheckNoUnknownKeys")))))))

(defun compile-function-body-direct (params body &optional (fn-name "") fn-pkg fn-symbol)
  "Compile function body with direct parameter passing (no args array).
   Only for functions with exactly required params, no optional/key/rest.
   Params are accessed via (:ldarg 0), (:ldarg 1), ... directly.
   FN-PKG, if given, is the defining package name — used by the self-call
   symbol-lookup cache.
   FN-SYMBOL, if given, is the defun symbol — used for native eligibility check."
  (multiple-value-bind (required optional key rest-param aux) (parse-lambda-list params)
    (declare (ignore optional key rest-param aux))
    (let* ((all-params required)
           (*locals* '())
           (*boxed-vars* '())
           (*block-tags* '())
           (*go-tags* '())
           ;; Normally empty (a defun body sees no lexical local functions). During
           ;; speculative labels-self-TCO compilation, inject THIS function's own box
           ;; as a local-function: a self-reference that becomes a TCO branch never
           ;; consults it, but one that does NOT (non-tail, arity mismatch, inside a
           ;; try region, #'g, an optimizer gate) falls to the local-fn path and emits
           ;; a (:ldloc box-key) the acceptance scan detects — forcing the safe
           ;; closure-path fallback. Keyed off fn-name so nested inner lambdas
           ;; (fn-name "") never pick it up.
           (*local-functions*
             (let ((spec *labels-direct-speculation*))
               (if (and spec (string= fn-name (car spec)))
                   (list (list (car spec) (cdr spec) t))
                   '())))
           ;; NOTINLINE in this body disables matching compiler macros (CLHS 3.2.2.1.1).
           (*notinline-functions* (extract-notinline body))
           (local-keys (gen-param-local-keys all-params))
           (needs-boxing (params-needs-boxing body all-params))
           ;; Pre-check native eligibility: all fixnum params, fixnum return, no captures
           ;; Full check (including no specials) happens after special-param-syms is computed,
           ;; but we need this early for param-instrs type selection.
           (pre-special-syms
             (when (and fn-symbol (null needs-boxing) (all-params-fixnum-p params body))
               (union (fn-body-special-params body (mapcar #'var-name all-params))
                      (remove-if-not #'global-special-p all-params))))
           (pre-native-eligible
             (and fn-symbol
                  (null needs-boxing)
                  (all-params-fixnum-p params body)
                  (null pre-special-syms)
                  (eq 'fixnum (gethash fn-symbol *function-return-types*)))))
      (let ((*locals* local-keys)
            (*symbol-macros* (params-shadowed-symbol-macros all-params))
            (*boxed-vars* (params-boxed-vars all-params needs-boxing)))
        (let ((param-instrs
                (if pre-native-eligible
                    ;; Native body: arg0 is the self LispFunction (threaded for self-calls),
                    ;; so the long params start at ldarg 1. Store into Int64 locals.
                    (loop for p in required
                          for key = (cdr (assoc p local-keys))
                          for i from 1
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
                   (union (fn-body-special-params body (mapcar #'var-name all-params))
                          (remove-if-not #'global-special-p all-params)))
                 (sp-names (mapcar #'var-name special-param-syms))
                 (special-push-instrs
                   (loop for p in special-param-syms
                         for pkey = (cdr (assoc p local-keys))
                         append `(,@(compile-sym-lookup p)
                                  (:castclass "Symbol")
                                  (:ldloc ,pkey)
                                  (:call "DynamicBindings.Push"))))
                 (*locals* (remove-if (lambda (entry)
                                        (member (let ((k (car entry)))
                                                  (if (symbolp k) (var-name k) ""))
                                                sp-names :test #'string=))
                                      *locals*)))
            ;; TCO scope: enabled for named functions without special params.
            ;; Special params require a try/finally block (DynamicBindings.Pop) and
            ;; CIL's plain Br can't escape it (needs Leave). Boxed params are fine —
            ;; compile-named-call's TCO branch writes through the box via stelem-ref
            ;; (relaxed the prior (null needs-boxing) restriction; *tco-param-entries*
            ;; now carries per-param boxed-p so the self-call code chooses the right path).
            ;; Always reset TCO state so inner lambdas don't inherit outer TCO context.
            (let* ((use-tco (and (string/= fn-name "")
                                 (null special-param-syms)))
                   (tco-loop-label (when use-tco (gen-label "TCOLOOP")))
                   (*tco-self-name* (if use-tco fn-name nil))
                   (*tco-loop-label* (if use-tco tco-loop-label nil))
                   ;; Reset mutual-TCO: closures compiled within labels group must not
                   ;; emit br-to-outer-TCOLOOP.
                   (*labels-mutual-tco* nil)
                   ;; Self-fn local: holds the LispFunction used by non-tail self-calls.
                   ;; - Native bodies: the self LispFunction arrives as arg0, so use
                   ;;   the sentinel :ARG0 — self-call sites load (:ldarg 0) and NO prelude
                   ;;   symbol-lookup runs per recursive entry.
                   ;; - Labels functions are stored in boxes, not symbols — skip.
                   (*self-fn-local* (cond ((and pre-native-eligible use-tco) :arg0)
                                          ((and use-tco (null *tco-local-fn-key*))
                                           (gen-local "SELF-FN"))))
                   ;; Self-fn caching: for (SETF NAME) functions, look up SetfFunction
                   ;; on the target NAME symbol rather than Function on "(SETF NAME)"
                   ;; (fix broken GetFunctionBySymbol call for setf functions).
                   (setf-fn-p (and *self-fn-local* (not (eq *self-fn-local* :arg0))
                                   (> (length fn-name) 7)
                                   (string= fn-name "(SETF " :end1 6)))
                   (setf-target-name (when setf-fn-p
                                       (subseq fn-name 6 (1- (length fn-name)))))
                   (self-fn-prelude
                     (when (and *self-fn-local* (not (eq *self-fn-local* :arg0)))
                       (if setf-fn-p
                           ;; (SETF NAME): look up SetfFunction on the target symbol
                           `((:declare-local ,*self-fn-local* "LispFunction")
                             ,@(if fn-pkg
                                   `((:load-sym-pkg ,setf-target-name ,fn-pkg))
                                   `((:load-sym-fn ,setf-target-name ,(package-name *package*))))
                             (:castclass "Symbol")
                             (:call "CilAssembler.GetSetfFunctionBySymbol")
                             (:stloc ,*self-fn-local*))
                           ;; Normal function: look up Function on the symbol
                           `((:declare-local ,*self-fn-local* "LispFunction")
                             ,@(if fn-pkg
                                   `((:load-sym-pkg ,fn-name ,fn-pkg))
                                   `((:load-sym-fn ,fn-name ,(package-name *package*))))
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
                   ;; - MV return propagation: tail form doesn't unwrap MvReturn
                   (*in-tail-position* t)
                   ;; Fixnum type declarations on params — consulted by fixnum-typed-p
                   ;; and compile-as-long for native int64 paths.
                   (*fixnum-locals* (append (extract-fixnum-locals body) *fixnum-locals*))
                   ;; Bounded-integer type declarations on params (signed-byte/
                   ;; unsigned-byte/bit) → tight range gating native int64 arith.
                   (*small-int-locals* (append (extract-small-int-locals body)
                                               *small-int-locals*))
                   ;; Double-float type declarations on params.
                   (*double-float-locals* (append (extract-double-float-locals body)
                                                  *double-float-locals*))
                   (*single-float-locals* (append (extract-single-float-locals body)
                                                  *single-float-locals*))
                   ;; Decimal type declarations on params: native
                   ;; System.Decimal arithmetic in the body, scale preserved.
                   (*decimal-locals* (append (extract-decimal-locals body)
                                             *decimal-locals*))
                   ;; Float-array type declarations on params/locals (e.g.
                   ;; (simple-array double-float (*))) → aref rides native r8.
                   (*numeric-array-locals* (append (extract-float-array-locals body)
                                                   *numeric-array-locals*))
                   ;; Native body: params are Int64, enabling compile-as-long without
                   ;; unbox and native self-calls via InvokeNativeN.
                   (*long-locals* (if (and pre-native-eligible use-tco)
                                      (mapcar #'var-name all-params)
                                      *long-locals*))
                   (*native-self-name* (if (and pre-native-eligible use-tco)
                                           fn-name
                                           *native-self-name*))
                   ;; If we'll wrap in try/finally for DynamicBindings.Pop, body
                   ;; compilation must know — so tail-position calls don't emit
                   ;; the `.tail` prefix (illegal in CIL inside try) and TCO
                   ;; branches don't try to cross the try boundary.
                   (*in-try-block* (or *in-try-block* (not (null special-param-syms))))
                   (body-instrs (compile-progn body))
                   ;; Extend the native self-as-arg0 threading to NON-native direct
                   ;; functions. The per-entry self-fn-prelude (load-sym→castclass→
                   ;; GetFunctionBySymbol) only survives JIT DCE when the body does a
                   ;; non-tail self-call (= references *self-fn-local*). For exactly those
                   ;; functions, thread the self LispFunction in as a hidden arg0 (shift
                   ;; the LispObject params to ldarg 1..n, rewrite the self-call receiver
                   ;; to ldarg 0, drop the prelude). The caller emits :self t so the
                   ;; backend gives the direct method a leading LispFunction param and
                   ;; binds _funcN's target to fn. Non-recursive direct functions keep
                   ;; arg0 = first param so their apply/array path needs no symbol lookup.
                   (self-arg0-p (and (not pre-native-eligible)
                                     *self-fn-local*
                                     (not (eq *self-fn-local* :arg0))
                                     (null special-param-syms)
                                     (%sil-references-local-p body-instrs *self-fn-local*)))
                   (eff-param-instrs
                     (if self-arg0-p
                         (loop for p in required
                               for key = (cdr (assoc p local-keys))
                               for i from 1
                               if (boxed-var-p p)
                                 append `((:declare-local ,key "LispObject[]")
                                          (:ldc-i4 1) (:newarr "LispObject") (:dup)
                                          (:ldc-i4 0) (:ldarg ,i)
                                          (:stelem-ref) (:stloc ,key))
                               else
                                 append `((:declare-local ,key "LispObject")
                                          (:ldarg ,i) (:stloc ,key)))
                         param-instrs))
                   (eff-self-fn-prelude (if self-arg0-p '() self-fn-prelude))
                   (eff-body-instrs (if self-arg0-p
                                        (%sil-subst-self-arg0 body-instrs *self-fn-local*)
                                        body-instrs)))
              (values
               (merge-disjoint-locals
                (if special-param-syms
                    `(,@param-instrs
                      ,@self-fn-prelude
                      ,@(when use-tco `((:label ,tco-loop-label)))
                      ,@(compile-let-with-specials '() special-push-instrs body-instrs special-param-syms)
                      (:ret))
                    `(,@eff-param-instrs
                      ,@eff-self-fn-prelude
                      ,@(when use-tco `((:label ,tco-loop-label)))
                      ,@(maybe-tail-callvirt eff-body-instrs)
                      (:ret))))
               self-arg0-p))))))))

(defun wrap-aux-body (aux body)
  "Wrap BODY in (let* AUX ...) for &aux parameters. If BODY is a single
   (block name . forms), push the let* INSIDE the block so leading
   (declare (special x)) stays directly in the let* body — otherwise it gets
   buried under the block and compile-let* can't see it, so a body-level
   special declaration for a captured free var is lost."
  (if (null aux)
      body
      (if (and (= (length body) 1)
               (consp (car body))
               (eq (caar body) 'block))
          (let ((block-name (cadar body))
                (block-forms (cddar body)))
            `((block ,block-name (let* ,aux ,@block-forms))))
          `((let* ,aux ,@body)))))

(defun compile-function-body-inner (params body args-arg-idx &optional (fn-name ""))
  "Compile function body, loading args from (:ldarg ARGS-ARG-IDX).
   For normal functions args-arg-idx=0, for closures args-arg-idx=1."
  (multiple-value-bind (required optional key rest-param aux allow-other-keys-p has-key-p) (parse-lambda-list params)
    (let ((body (wrap-aux-body aux body)))
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
           ;; NOTINLINE in this body disables matching compiler macros for calls
           ;; within it (CLHS 3.2.2.1.1). Fresh function scope → this body only.
           (*notinline-functions* (extract-notinline body))
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
           ;; Reset native state: inner lambdas don't inherit outer native context
           (*long-locals* nil)
           (*numeric-array-locals* nil)
           (*native-self-name* nil)
           (local-keys (gen-param-local-keys all-params))
           (needs-boxing (params-needs-boxing body all-params))
           (n-required (length required))
           (key-start (+ n-required (length optional))))
      (let ((*locals* local-keys)
            (*symbol-macros* (params-shadowed-symbol-macros all-params))
            (*boxed-vars* (params-boxed-vars all-params needs-boxing)))
        ;; Generate parameter binding instructions via the shared args-array
        ;; machinery (compile-args-param-instrs); the args array lives at
        ;; ldarg ARGS-ARG-IDX here, and defaults mask current+later params
        ;; by filtering the full *locals*.
        (let ((param-instrs
                (compile-args-param-instrs
                 required optional key rest-param local-keys n-required key-start
                 (lambda (i) `((:ldarg ,args-arg-idx) (:ldc-i4 ,i) (:ldelem-ref)))
                 `((:ldarg ,args-arg-idx))
                 (lambda (names)
                   (remove-if (lambda (entry)
                                (member (var-name (car entry)) names :test #'string=))
                              *locals*)))))
          (let* ((arity-instrs
                  (compile-args-arity-instrs fn-name optional key rest-param has-key-p
                                             n-required `((:ldarg ,args-arg-idx))))
                (key-check-instrs
                  (compile-args-key-check-instrs fn-name key has-key-p allow-other-keys-p
                                                 key-start `((:ldarg ,args-arg-idx)))))
           ;; Handle special params: both (declare (special param)) and
           ;; globally special params (defvar/*name* convention) bind dynamically
           (let* ((special-param-syms
                    (union (fn-body-special-params body (mapcar #'var-name all-params))
                           (remove-if-not #'global-special-p all-params)))
                  (sp-names (mapcar #'var-name special-param-syms))
                  (special-push-instrs
                    (loop for p in special-param-syms
                          for pkey = (cdr (assoc p local-keys))
                          append `(,@(compile-sym-lookup p)
                                   (:castclass "Symbol")
                                   (:ldloc ,pkey)
                                   (:call "DynamicBindings.Push"))))
                  (*locals* (remove-if (lambda (entry)
                                         (member (let ((k (car entry)))
                                                   (if (symbolp k) (var-name k) ""))
                                                 sp-names :test #'string=))
                                       *locals*))
                  ;; Tail position preserves MvReturn for multi-value callers
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
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))
            ;; (type fixnum x y ...)
            ((and (eq (car decl) 'type) (eq (cadr decl) 'fixnum))
             (dolist (v (cddr decl))
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))))))
    result))

(defun extract-small-int-locals (body)
  "Scan head (declare ...) forms for bounded integer type hints —
   (signed-byte N) / (unsigned-byte N) / bit / (integer LO HI) — on locals.
   Returns an alist (name-string . (LO . HI)) for those whose range fits int64.
   Unlike extract-fixnum-locals these carry a TIGHT range, so expr-int-range can
   prove a product stays in int64 (native) or, when it can't, fall back to the
   promoting path (bignum). This is the CL-compliant generalization of the case
   extract-fixnum-locals deliberately punted on before the range gate."
  (let ((result '()))
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare))
        (return))
      (dolist (decl (cdr form))
        (when (consp decl)
          (multiple-value-bind (type vars)
              (if (eq (car decl) 'type)
                  (values (cadr decl) (cddr decl))
                  (values (car decl) (cdr decl)))
            (let ((range (integer-type-range type)))
              (when (and range (range-fits-int64-p range))
                (dolist (v vars)
                  (when (symbolp v)
                    (pushnew (cons (var-name v) range) result
                             :key #'car :test #'string=)))))))))
    result))

(defun infer-small-int-bindings (binding-info needs-boxing mutated)
  "For plain lexical (non-special, non-boxed, non-mutated) let bindings whose
   init has a statically provable int64 range (expr-int-range), return an alist
   (name-string . (LO . HI)) to extend *small-int-locals* for the body. This is
   what makes an undeclared let var like crc's new-rmdr = (logior bit (* rmdr 2))
   participate in native int64 arithmetic. Mutated bindings are excluded: a setf
   could move the value outside the init range, breaking the range gate's
   soundness (and the inline unbox-fixnum, which assumes a Fixnum). Init ranges
   are evaluated against the enclosing *small-int-locals* (callers leave the
   prior binding in effect), so the result is sound for both let and let*."
  (let ((result '()))
    (dolist (b binding-info)
      (let ((var (first b)) (init (second b)) (is-special (third b)))
        (when (and (not is-special)
                   init
                   (not (member (var-name var) needs-boxing :test #'string=))
                   (not (member (var-name var) mutated :test #'string=)))
          (let ((r (expr-int-range init)))
            (when (and r (range-fits-int64-p r))
              (push (cons (var-name var) r) result))))))
    result))

(defun %dotnet-sym-p (x name)
  "True when X is a symbol named NAME in the DOTNET package. Cross-compile safe:
   on the SBCL host there is no DOTNET package, so this is simply always NIL and
   no .NET type inference happens during cross-compile."
  (and (symbolp x) (string= (symbol-name x) name)
       (let ((p (symbol-package x)))
         (and p (string= (package-name p) "DOTNET")))))

(defun %dotnet-init-type (init)
  "If the let/let* init form INIT statically denotes a typed .NET value, return
   its .NET type-name string, else NIL. Mirrors the receiver/arg type sources the
   DOTNET:INVOKE compiler macro already understands:
     (the (dotnet \"T\") E)  -> \"T\"   (E is asserted to be a .NET object of T)
     (dotnet:new \"T\" ...)   -> \"T\"
     (dotnet:box E \"T\")     -> \"T\"
   The variable bound to such an init holds a .NET object of T, so a later
   (dotnet:invoke var ...) can be lowered to a typed direct callvirt."
  (and (consp init)
       (let ((head (car init)))
         (cond
           ((and (symbolp head) (string= (symbol-name head) "THE")
                 (consp (cdr init)) (consp (cadr init)) (consp (cddr init)))
            (let ((spec (cadr init)))
              (and (symbolp (car spec)) (string= (symbol-name (car spec)) "DOTNET")
                   (consp (cdr spec)) (stringp (cadr spec))
                   (cadr spec))))
           ((%dotnet-sym-p head "NEW")
            (and (consp (cdr init)) (stringp (cadr init)) (cadr init)))
           ((%dotnet-sym-p head "BOX")
            (and (consp (cdr init)) (consp (cddr init)) (stringp (caddr init))
                 (caddr init)))
           (t nil)))))

(defun infer-dotnet-typed-bindings (binding-info mutated outer)
  "Compute the .NET-typed-locals environment for a let/let* body. Start from OUTER
   (the enclosing environment), drop every name this let binds (so a rebinding
   shadows an outer typed local even when the new init is untyped), then add an
   entry for each non-special, non-mutated binding whose init denotes a typed .NET
   value. Each new entry is (name-string type-string . local-key); the key lets
   the consumer verify the binding is still the live one (see
   DOTNET-VALID-TYPED-LOCALS). Mutated bindings are excluded because a setf could
   replace the value with a different type, which would miscompile the direct
   callvirt."
  (let* ((bound-names (mapcar (lambda (b) (var-name (first b))) binding-info))
         (result (remove-if (lambda (e) (member (car e) bound-names :test #'string=))
                            outer)))
    (dolist (b binding-info)
      (let ((var (first b)) (init (second b)) (is-special (third b)) (key (fourth b)))
        (unless (or is-special
                    (member (var-name var) mutated :test #'string=))
          (let ((type (%dotnet-init-type init)))
            (when type
              (push (list* (var-name var) type key) result))))))
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
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))
            ((and (eq (car decl) 'type) (eq (cadr decl) 'double-float))
             (dolist (v (cddr decl))
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))))))
    result))

(defun decimal-type-name-p (sym)
  "T iff SYM is specifically DOTCL:DECIMAL, keyed on symbol-NAME *and* home
   PACKAGE-NAME. Both are cross-compile-safe strings (dotcl:decimal need not exist on
   the SBCL host, so EQ is unusable), yet the package check keeps a bare or foreign
   DECIMAL (CL-USER::DECIMAL, my-lib::DECIMAL) from being mistaken for the CLR decimal
   type and mis-triggering native decimal codegen. Under (use-package :dotcl) an
   unqualified DECIMAL resolves to dotcl:decimal and still matches."
  (and (symbolp sym)
       (string= (symbol-name sym) "DECIMAL")
       (let ((p (symbol-package sym)))
         (and p (string= (package-name p) "DOTCL")))))

(defun extract-decimal-locals (body)
  "Parallel to extract-double-float-locals for decimal. Only the
   (type decimal ...) / (decimal ...) forms — decimal is non-standard, so there is
   no bare shorthand class name to also accept beyond these."
  (let ((result '()))
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare))
        (return))
      (dolist (decl (cdr form))
        (when (consp decl)
          (cond
            ((decimal-type-name-p (car decl))
             (dolist (v (cdr decl))
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))
            ((and (symbolp (car decl)) (string= (symbol-name (car decl)) "TYPE")
                  (decimal-type-name-p (cadr decl)))
             (dolist (v (cddr decl))
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))))))
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
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))
            ((and (eq (car decl) 'type) (eq (cadr decl) 'single-float))
             (dolist (v (cddr decl))
               (when (symbolp v) (pushnew (var-name v) result :test #'string=))))))))
    result))

(defun extract-float-array-locals (body)
  "Scan the head declare forms of BODY for variables declared with a float
   simple-array/array/vector type of statically-known rank 1-3, returning
   *numeric-array-locals* entries (NAME KEY RANK . :single/:double). This lets
   aref on a float-array-typed PARAMETER or declared local (e.g. the fft/
   mandelbrot kernels' (simple-array single-float (1025)) / (simple-array
   double-float (6))) ride the native r8 path (Runtime.ArefNum*D), not just
   make-array let-locals. Must be called with *locals* bound (KEY = the var's
   slot via lookup-local, which numeric-array-aref-entry re-checks so a shadow
   or a re-key self-invalidates). The runtime fast path re-checks _numKind, so a
   caller that passes a non-float-backed array falls back to the boxed coerce
   path rather than corrupting data — the declaration is a soundness-safe hint."
  (let ((result '()))
    (dolist (form body)
      (unless (and (consp form) (eq (car form) 'declare)) (return))
      (dolist (decl (cdr form))
        (when (and (consp decl) (eq (car decl) 'type))
          (let ((kr (%array-type-float-kind-and-rank (cadr decl))))
            (when kr
              (dolist (v (cddr decl))
                (when (and (symbolp v) (lookup-local v) (not (boxed-var-p v)))
                  (pushnew (list* (var-name v) (lookup-local v) (cdr kr) (car kr))
                           result :test #'string= :key #'car))))))))
    result))

;;; ============================================================
;;; Return type inference
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
          (member (car expr) '(+ - * 1+ 1- logand logior logxor min max abs))
          (every (lambda (a)
                   (eq 'fixnum (infer-expr-return-type a var-types self-name)))
                 (cdr expr)))
     'fixnum)
    ;; ash: a non-negative shift can overflow int64, so only infer fixnum when
    ;; the value provably fits — negative shift (right shift), or a constant base
    ;; and shift whose folded result is in int64 range. Must match fixnum-typed-p
    ;; so the native-long return ABI never compiles an overflowing SHL.
    ((and (consp expr) (= (length expr) 3) (eq (car expr) 'ash)
          (eq 'fixnum (infer-expr-return-type (cadr expr) var-types self-name))
          (integerp (caddr expr))
          (let ((n (caddr expr)))
            (or (< n 0)
                (and (integerp (cadr expr))
                     (typep (ash (cadr expr) n) '(signed-byte 64))))))
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
                         (list var init is-special (gen-local (var-name var)))))
                     parsed))
           ;; Pre-scan for mutable captures in body (and init forms for let*)
           (var-names (mapcar #'first parsed))
           ;; For let*, lambdas in init forms can capture+mutate earlier vars
           (scan-forms (if sequential-p
                           (append (remove nil (mapcar #'second parsed)) real-body)
                           real-body))
           (mc (multiple-value-list
                (find-mutated-and-captured-vars scan-forms (mapcar #'var-name var-names))))
           (mutated (first mc))
           (captured (second mc))
           (needs-boxing (intersection mutated captured :test #'string=))
           ;; Separate
           (special-bindings (remove-if-not #'third binding-info))
           (lexical-bindings (remove-if #'third binding-info))
           ;; Long-rep candidates: fixnum-declared plain lexicals whose slot can
           ;; hold a raw Int64 instead of a boxed Fixnum. Requires: not special,
           ;; not captured by any closure (env capture loads the slot as an
           ;; object reference), and a fixnum-typed init so the initial store is
           ;; well-typed. Mutation is fine — compile-setq has an Int64-slot
           ;; path. References in boxed contexts re-box via Fixnum.Make; native
           ;; contexts (compare/arith/aref index) read the slot directly, which
           ;; is what removes the per-iteration box/unbox from loop counters.
           (fx-decl-names (extract-fixnum-locals body))
           (long-rep-names
             (when fx-decl-names
               (loop for b in binding-info
                     for nm = (var-name (first b))
                     when (and (not (third b))
                               (second b)
                               (member nm fx-decl-names :test #'string=)
                               (not (member nm captured :test #'string=))
                               (fixnum-typed-p (second b)))
                       collect nm)))
           ;; Native-float-slot candidates: the r8/r4 analog of long-rep-names.
           ;; A double/single-float-declared plain lexical, not special, not
           ;; captured, with a float-typed init (so the initial native store is
           ;; well-typed). Its slot holds a raw double/float; the per-setq box in
           ;; numeric loops is eliminated (fft/mandelbrot accumulators).
           (dbl-decl-names (extract-double-float-locals body))
           (sgl-decl-names (extract-single-float-locals body))
           (double-rep-names
             (when dbl-decl-names
               (loop for b in binding-info
                     for nm = (var-name (first b))
                     when (and (not (third b)) (second b)
                               (member nm dbl-decl-names :test #'string=)
                               (not (member nm captured :test #'string=))
                               (double-float-typed-p (second b)))
                       collect nm)))
           (single-rep-names
             (when sgl-decl-names
               (loop for b in binding-info
                     for nm = (var-name (first b))
                     when (and (not (third b)) (second b)
                               (member nm sgl-decl-names :test #'string=)
                               (not (member nm captured :test #'string=))
                               (single-float-typed-p (second b)))
                       collect nm))))
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
            ;; into the final binding key — no temp needed.
            (let ((temp-keys
                    (mapcar (lambda (b)
                              (let ((var (first b))
                                    (is-special (third b)))
                                (if (or is-special
                                        (member (var-name var) needs-boxing
                                                :test #'string=))
                                    (gen-local "INIT")
                                    nil)))
                            binding-info)))
              ;; Evaluate inits in old scope — inits bind a single value:
              ;; never in tail position and never in MV context.
              (loop for b in binding-info
                    for tk in temp-keys
                    do (let* ((init-form (second b))
                              (key (fourth b))
                              (nm (var-name (first b)))
                              (nfk (cond ((member nm double-rep-names :test #'string=) :double)
                                         ((member nm single-rep-names :test #'string=) :single)
                                         (t nil))))
                         (cond
                           ((member nm long-rep-names :test #'string=)
                             ;; Long-rep lexical: raw Int64 slot, init lowered
                             ;; to a raw long (still in the OLD scope).
                             (setf init-instrs
                                   (append init-instrs
                                           `((:declare-local ,key "Int64")
                                             ,@(compile-expr-to-long init-form)
                                             (:stloc ,key)))))
                           (nfk
                             ;; Native float rep: raw r8/r4 slot, init lowered
                             ;; to a native float (still in the OLD scope).
                             (setf init-instrs
                                   (append init-instrs
                                           `((:declare-local ,key ,(ecase nfk (:double "Double") (:single "Single")))
                                             ,@(compile-float-native-value init-form nfk)
                                             (:stloc ,key)))))
                           (t
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
                                                   (:stloc ,key))))))))))
              ;; Now bind in new scope
              ;; Filter *boxed-vars* to remove names being rebound as non-boxed
              (let* ((non-boxed-names (set-difference
                                       (mapcar #'var-name var-names)
                                       needs-boxing :test #'string=))
                     (filtered-boxed (remove-if
                                      (lambda (x)
                                        (member (if (symbolp x) (var-name x) x)
                                                non-boxed-names :test #'string=))
                                      *boxed-vars*))
                     ;; Remove declared-specials from *locals* so references use dynamic binding
                     (ds-names (mapcar #'var-name declared-specials))
                     (*locals* (remove-if
                                (lambda (entry)
                                  (member (let ((k (car entry)))
                                            (if (symbolp k) (var-name k) ""))
                                          ds-names :test #'string=))
                                (append new-local-entries *locals*)))
                     (*specials* all-specials)
                     ;; Shadow symbol-macros for variables being bound by this let
                     (*symbol-macros* (remove-if
                                       (lambda (entry)
                                         (member (var-name (car entry))
                                                 (mapcar #'var-name var-names)
                                                 :test #'string=))
                                       *symbol-macros*))
                     (*boxed-vars* (append
                                    (mapcar (lambda (name)
                                              (find name var-names
                                                    :key #'var-name :test #'string=))
                                            needs-boxing)
                                    filtered-boxed))
                     ;; Long-rep bindings are Int64 slots for the body; names
                     ;; rebound here as ordinary LispObject slots must shadow
                     ;; (drop) any outer Int64-slot entry of the same name.
                     (*long-locals*
                       (let ((bound-names (mapcar #'var-name var-names)))
                         (append long-rep-names
                                 (remove-if (lambda (n)
                                              (member n bound-names :test #'string=))
                                            *long-locals*))))
                     ;; Native-float-slot bindings (r8/r4) for the body; a name
                     ;; rebound here as an ordinary slot shadows any outer
                     ;; native-float entry of the same name.
                     (bound-names-nf (mapcar #'var-name var-names))
                     (*native-double-locals*
                       (append double-rep-names
                               (remove-if (lambda (n) (member n bound-names-nf :test #'string=))
                                          *native-double-locals*)))
                     (*native-single-locals*
                       (append single-rep-names
                               (remove-if (lambda (n) (member n bound-names-nf :test #'string=))
                                          *native-single-locals*))))
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
                             ((member (var-name var) needs-boxing :test #'string=)
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
                                         (*small-int-locals*
                                          (append (extract-small-int-locals body)
                                                  (infer-small-int-bindings
                                                   binding-info needs-boxing mutated)
                                                  *small-int-locals*))
                                         (*numeric-array-locals*
                                          (append
                                           (extract-float-array-locals body)
                                           (infer-numeric-array-bindings
                                            binding-info mutated *numeric-array-locals*)))
                                         (*double-float-locals*
                                          (append (extract-double-float-locals body)
                                                  *double-float-locals*))
                                         (*single-float-locals*
                                          (append (extract-single-float-locals body)
                                                  *single-float-locals*))
                                         (*decimal-locals*
                                          (append (extract-decimal-locals body)
                                                  *decimal-locals*))
                                         (*dotnet-typed-locals*
                                          (infer-dotnet-typed-bindings
                                           binding-info mutated *dotnet-typed-locals*)))
                                     (compile-progn real-body))))
                  (compile-let-with-specials
                   init-instrs bind-instrs body-instrs
                   (reverse special-syms)))))
            ;; Let*: sequential binding — incrementally extend *locals*
            (let ((*locals* *locals*)
                  (*specials* all-specials)
                  (*symbol-macros* *symbol-macros*)
                  ;; Extended/shadowed incrementally as bindings are processed
                  ;; so each init sees exactly the earlier siblings' slots.
                  (*long-locals* *long-locals*)
                  (*native-double-locals* *native-double-locals*)
                  (*native-single-locals* *native-single-locals*)
                  (*boxed-vars* (append
                                  (mapcar (lambda (name)
                                            (find name var-names
                                                  :key #'var-name :test #'string=))
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
                                                 ;; Init forms bind a single value
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
                               (let* ((long-rep-p (member (var-name var) long-rep-names
                                                          :test #'string=))
                                      (nfk (cond ((member (var-name var) double-rep-names :test #'string=) :double)
                                                 ((member (var-name var) single-rep-names :test #'string=) :single)
                                                 (t nil)))
                                      (init-code (cond
                                                   (long-rep-p (compile-expr-to-long init-form))
                                                   (nfk (compile-float-native-value init-form nfk))
                                                   (t (let ((*in-tail-position* nil)
                                                            (*in-mv-context* nil))
                                                        (if init-form
                                                            (compile-expr init-form)
                                                            (emit-nil)))))))
                                 ;; Extend scope for subsequent bindings and body
                                 (push (cons var key) *locals*)
                                 ;; Long-rep slot for subsequent inits/body; a
                                 ;; non-long rebinding shadows an outer or
                                 ;; earlier-sibling Int64 slot of the same name.
                                 (setf *long-locals*
                                       (if long-rep-p
                                           (cons (var-name var) *long-locals*)
                                           (remove (var-name var) *long-locals*
                                                   :test #'string=)))
                                 ;; Native-float slot for subsequent inits/body,
                                 ;; shadowing likewise on a non-native rebinding.
                                 (setf *native-double-locals*
                                       (if (eq nfk :double)
                                           (cons (var-name var) *native-double-locals*)
                                           (remove (var-name var) *native-double-locals* :test #'string=)))
                                 (setf *native-single-locals*
                                       (if (eq nfk :single)
                                           (cons (var-name var) *native-single-locals*)
                                           (remove (var-name var) *native-single-locals* :test #'string=)))
                                 ;; Shadow any symbol-macro with this name
                                 (setf *symbol-macros*
                                       (remove var *symbol-macros*
                                               :key #'car :test #'eq))
                                 (if (member (var-name var) needs-boxing :test #'string=)
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
                                               (remove (var-name var) *boxed-vars*
                                                       :key (lambda (x) (if (symbolp x) (var-name x) x))
                                                       :test #'string=)))
                                       (setf bind-instrs
                                             (append bind-instrs
                                                     `((:declare-local ,key
                                                        ,(cond (long-rep-p "Int64")
                                                               ((eq nfk :double) "Double")
                                                               ((eq nfk :single) "Single")
                                                               (t "LispObject")))
                                                       ,@init-code
                                                       (:stloc ,key))))))))))
              ;; Remove declared-specials from *locals* so body references use dynamic binding
              (let* ((ds-names (mapcar #'var-name declared-specials))
                     ;; Body inherits outer *in-tail-position*; TCO branches
                     ;; are suppressed via *in-try-block* when special-syms
                     ;; introduce a try/finally (see parallel-let branch).
                     (body-instrs (let ((*locals* (remove-if
                                                   (lambda (entry)
                                                     (member (let ((k (car entry)))
                                                               (if (symbolp k) (var-name k) ""))
                                                             ds-names :test #'string=))
                                                   *locals*))
                                        (*in-try-block*
                                         (or *in-try-block* special-syms))
                                        (*fixnum-locals*
                                         (append (extract-fixnum-locals body)
                                                 *fixnum-locals*))
                                        (*small-int-locals*
                                         (append (extract-small-int-locals body)
                                                 (infer-small-int-bindings
                                                  binding-info needs-boxing mutated)
                                                 *small-int-locals*))
                                        (*numeric-array-locals*
                                         (append
                                          (extract-float-array-locals body)
                                          (infer-numeric-array-bindings
                                           binding-info mutated *numeric-array-locals*)))
                                        (*double-float-locals*
                                         (append (extract-double-float-locals body)
                                                 *double-float-locals*))
                                        (*single-float-locals*
                                         (append (extract-single-float-locals body)
                                                 *single-float-locals*))
                                        (*decimal-locals*
                                         (append (extract-decimal-locals body)
                                                 *decimal-locals*))
                                        (*dotnet-typed-locals*
                                         (infer-dotnet-typed-bindings
                                          binding-info mutated *dotnet-typed-locals*)))
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
          ;; Pop in REVERSE of push order (LIFO). All call sites pass SPECIAL-SYMS
          ;; in push (source) order, so the naive in-order loop emitted FIFO Pops
          ;; for a multi-special frame. The current by-symbol DynamicBindings.Pop
          ;; tolerates any order, so this is behavior-neutral today — but it lets a
          ;; shallow-binding Pop take its O(1) top-of-stack fast path instead of the
          ;; out-of-order search fallback.
          ,@(loop for sym in (reverse special-syms)
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
    ;; such NAME elide Runtime.UnwrapMv.
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
  ;; If var is a symbol-macro, delegate to SETF of the expansion (branch on found-p,
  ;; not the expansion's truth value, so a NIL-expanding symbol-macro still delegates )
  (multiple-value-bind (sm-exp found) (lookup-symbol-macro var)
    (when found
      (return-from compile-setq (compile-expr `(setf ,sm-exp ,val-expr)))))
  ;; Int64-slot local (long-rep let binding or native body param): store the
  ;; raw long, then box once for the expression value. In statement position
  ;; the peephole pass deletes the box+pop, leaving a pure native store.
  ;; Checked before the generic path because the slot type differs.
  (let ((key (lookup-local var)))
    (when (and key
               (boundp '*long-locals*) *long-locals*
               (member (var-name var) *long-locals* :test #'string=)
               (not (boxed-var-p var)))
      (return-from compile-setq
        `(,@(compile-expr-to-long val-expr)
          (:dup)
          (:stloc ,key)
          (:call "Fixnum.Make")))))
  ;; Native float slot (double/single-rep local): store the raw r8/r4, then box
  ;; once for the expression value. The peephole (P6/P7) deletes the box+pop in
  ;; statement position, leaving a pure native store — this is what removes the
  ;; per-setq DoubleFloat/SingleFloat box from numeric accumulator loops.
  (let ((key (lookup-local var))
        (nk (float-native-local-kind var)))
    (when (and key nk)
      (return-from compile-setq
        `(,@(compile-float-native-value val-expr nk)
          (:dup)
          (:stloc ,key)
          (:newobj ,(ecase nk (:double "DoubleFloat") (:single "SingleFloat")))))))
  ;; Variable assignment binds a single value. Force *in-mv-context* nil
  ;; and *in-tail-position* nil when compiling val-expr so MvReturn is
  ;; unwrapped before storage (mirrors compile-let).
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
  (multiple-value-bind (required optional key rest-param aux) (parse-lambda-list params)
    (declare (ignore rest-param))
    (let* ((all-params (append required
                               (mapcar #'car optional)
                               (remove nil (mapcar #'third optional))
                               (mapcar #'second key)
                               (remove nil (mapcar #'fourth key))
                               ;; &aux vars are bound in the function body scope,
                               ;; so body references to them are not free (D-fix).
                               (mapcar (lambda (a) (if (consp a) (car a) a)) aux)))
           (bound-names (mapcar #'var-name all-params))
           (free-ht (make-hash-table :test #'equal)))
      ;; Scan body
      (dolist (form body)
        (find-free-vars-expr form bound-names free-ht))
      ;; Scan default forms in &optional/&key — pass nil for bound
      ;; since scan-lambda-list-defaults handles progressive scoping
      ;; (each default only sees params to its left, not all params)
      (scan-lambda-list-defaults params nil free-ht)
      (let ((keys '())) (maphash (lambda (k v) (declare (ignore v)) (push k keys)) free-ht) keys))))

(defun lambda-list-shape-tag (params)
  "Digit-free lambda-list shape tag for the make-function DM name (diagnostic
   only — InvokeSlow statistics can then break \"<anon:lambda>\" down by
   shape). Counts encode as repeated letters, capped at 9, because the
   statistics origin tag strips digits: \"oo\" = 2 &optional, \"kkk\" = 3
   &key, \"r\" = &rest, \"a\" = &aux; a bare &key with no key params = \"k\"."
  (multiple-value-bind (required optional key rest-param aux allow-other-keys-p has-key-p)
      (parse-lambda-list params)
    (declare (ignore required allow-other-keys-p))
    (concatenate 'string
                 (make-string (min (length optional) 9) :initial-element #\o)
                 (if (and has-key-p (null key))
                     "k"
                     (make-string (min (length key) 9) :initial-element #\k))
                 (if rest-param "r" "")
                 (if aux "a" ""))))

(defun compile-lambda (params body &optional (fn-name "") (fn-label ""))
  "Compile (lambda (params) body...). FN-NAME enables self-TCO when non-empty.
   FN-LABEL, when non-empty, is wired to the :name plist of
   :make-function/-direct so the runtime LispFunction carries the name
   (backtrace / statistics / print) WITHOUT enabling TCO — used by the
   runtime-registration defun paths, whose defuns were previously anonymous
   (e.g. &rest stdlib defuns in chunked progn compiles)."
  (multiple-value-bind (required optional key rest-param) (parse-lambda-list params)
    (declare (ignore optional key))
    (let* ((free-vars (remove-if #'global-special-name-p
                                 (find-free-vars-with-defaults params body)))
           ;; Speculative labels-self-TCO: when this named, required-only fn's
           ;; ONLY free var is its own labels box, compile it through the
           ;; direct+TCO path instead of a closure. compile-labels-boxed scans the
           ;; result and falls back to the closure path unless it is provably
           ;; self-contained (no box-key reference, no orphan post-TCO code), so a
           ;; wrong guess only forgoes the optimization — it never miscompiles.
           (spec *labels-direct-speculation*)
           (speculate-direct
             (and spec
                  (string= fn-name (car spec))
                  (simple-required-only-p params)
                  (equal free-vars
                         (list (concatenate 'string "__LABELFN_" (car spec)))))))
      (if (or (null free-vars) speculate-direct)
          ;; No captures (or speculating self-TCO) — :make-function or :make-function-direct
          (if (simple-required-only-p params)
              `((:make-function-direct
                 :param-count ,(length required)
                 ,@(when (string/= fn-label "") `(:name ,fn-label))
                 :body ,(compile-function-body-direct params body fn-name)))
              `((:make-function
                 :param-count ,(length required)
                 :ll-shape ,(lambda-list-shape-tag params)
                 ,@(when (string/= fn-label "") `(:name ,fn-label))
                 :body ,(compile-function-body params body))))
          ;; Closure: build env array, then :make-closure
          (compile-closure params body free-vars)))))

(defun compile-closure (params body free-vars)
  "Compile a closure lambda with captured variables."
  (multiple-value-bind (required optional key rest-param) (parse-lambda-list params)
    (declare (ignore optional key))
  (let* ((n-free (length free-vars))
         ;; Direct-params eligibility: required-only lambda list (no &optional/
         ;; &key/&rest/&aux — simple-required-only-p) with <= 6 params. Such a
         ;; body reads params exclusively via :load-arg, so the assembler can
         ;; emit it with real per-arity CLR params (:load-arg i -> ldarg i+1).
         ;; Its arity-check prefix is omitted (see compile-closure-body): the
         ;; runtime wrapper performs the identical CheckArityExact for
         ;; args-array callers, and the delegate signature enforces argc
         ;; structurally on the direct path.
         (direct-p (and (simple-required-only-p params)
                        (<= (length required) 6)))
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
                            (let ((entry (assoc fv *locals*
                                               :key (lambda (k) (var-name k))
                                               :test #'string=)))
                              (if entry
                                  (boxed-var-p (car entry))
                                  ;; Only check *local-functions* when not in *locals*
                                  (let ((lf (find fv *local-functions*
                                                  :key #'first :test #'string=)))
                                    (and lf (third lf))))))
                          free-vars))
         ;; Compile inner body with env slots
         (inner-body (compile-closure-body params body free-vars outer-boxed-fvs
                                           "" direct-p)))
    `(,@env-build-instrs
      (:make-closure
       :param-count ,(length required)
       ;; fn-name "" matches the name the omitted arity check would have
       ;; carried (compile-closure never passes a name to compile-closure-body)
       ,@(when direct-p '(:direct t :fn-name ""))
       :env-size ,n-free
       :env-map ,(loop for fv in free-vars
                       for i from 0
                       collect (let ((entry (assoc fv *locals*
                                                   :key (lambda (k) (var-name k))
                                                   :test #'string=)))
                                 (if entry
                                     (if (boxed-var-p (car entry))
                                         (list fv i "boxed")
                                         (list fv i "value"))
                                     ;; Only check *local-functions* when not in *locals*
                                     (let ((lf (find fv *local-functions*
                                                     :key #'first :test #'string=)))
                                       (if (and lf (third lf))
                                           (list fv i "boxed")
                                           (list fv i "value"))))))
       :body ,inner-body)))))

(defun compile-env-capture (fv-name idx)
  "Generate instructions to capture a free variable into env[idx]."
  (let ((entry (assoc fv-name *locals*
                      :key (lambda (k) (var-name k)) :test #'string=)))
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

(defun compile-closure-body (params body free-vars &optional outer-boxed-fvs (fn-name "")
                                                             direct-p)
  "Compile function body for closure. Free vars access env (arg 0), params from args (arg 1).
   OUTER-BOXED-FVS is a list of free var name strings that were boxed in the outer scope.
   Handles &rest/&optional/&key parameters.
   DIRECT-P: the caller will mark this closure :direct (required-only, <= 6
   params, assembled with per-arity CLR params) — omit the arity-check prefix;
   the runtime-side args-array wrapper performs the identical CheckArityExact."
  (multiple-value-bind (required optional key rest-param aux allow-other-keys-p has-key-p) (parse-lambda-list params)
    (let ((body (wrap-aux-body aux body)))
    (let* ((all-params (append required
                               (mapcar #'car optional)
                               (remove nil (mapcar #'third optional))
                               (mapcar #'second key)
                               (remove nil (mapcar #'fourth key))
                               (if rest-param (list rest-param) nil)))
           ;; Outer-context captures — evaluated BEFORE the closure-boundary
           ;; reset below, exactly as in the previous let* ordering.
           ;; Save outer local-functions for captured flet/labels functions
           (outer-local-fns *local-functions*)
           ;; Save outer block-tags for captured return-from
           (outer-block-tags *block-tags*)
           ;; Save outer go-tags for captured non-local go
           (outer-go-tags *go-tags*))
      ;; Closure-boundary reset: rebind every registered per-compilation state
      ;; variable (define-compile-state, cil-compiler.lisp) to its fresh value
      ;; so inner closures don't inherit the enclosing body's compile context
      ;; (TCO state / *self-fn-local* especially — it refers to a local
      ;; declared in the OUTER method). Overrides carry the entries whose
      ;; fresh value is not a registry constant: *notinline-functions* is
      ;; computed from this body (CLHS 3.2.2.1.1); *in-tail-position* T
      ;; (closure body's last form is in tail position: MV propagation)
      ;; duplicates the registered fresh-init for explicitness. Everything
      ;; that was previously bound AFTER the reset bindings in the old let*
      ;; is evaluated inside the thunk — the original evaluation order and
      ;; environment are preserved exactly.
      (call-with-fresh-closure-state
       (list (cons '*notinline-functions* (extract-notinline body))
             (cons '*in-tail-position* t))
       (lambda ()
    (let* ((n-required (length required))
           (key-start (+ n-required (length optional)))
           ;; Compute boxing needs for params within this closure (single walk)
           (all-var-names (append (mapcar #'var-name all-params)
                                  free-vars))
           (mc (multiple-value-list
                (find-mutated-and-captured-vars body all-var-names)))
           (mutated (first mc))
           (captured-inner (second mc))
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
                     collect (let ((key (gen-local (var-name p))))
                               (cons p key))))
             (*locals* (append param-locals env-locals))
             ;; Shadow symbol-macros whose names match captured env variables or params.
             ;; When a variable like X is both a symbol-macro (x → (svref #:inst 0)) AND
             ;; captured in the env, accessing X inside the closure must use the local
             ;; variable, not the expansion. Without this shadow, the expansion would be
             ;; compiled but its free variables (e.g. #:inst) wouldn't be in *locals*.
             (*symbol-macros*
              (let ((all-local-names
                     (append (mapcar (lambda (p) (var-name p)) all-params)
                             free-vars)))
                (remove-if (lambda (entry)
                             (let ((k (car entry)))
                               (member (if (symbolp k) (var-name k) "")
                                       all-local-names :test #'string=)))
                           *symbol-macros*)))
             (*boxed-vars* (append
                            (mapcar (lambda (name)
                                      (or (find name all-params :key #'var-name :test #'string=)
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
                                                   :key (lambda (k) (var-name k))
                                                   :test #'string=)))
                              (list fn-name (cdr env-entry) fn-boxed-p))))
             ;; Re-establish *block-tags* for captured block tags
             (*block-tags*
              (loop for (bname . binfo) in outer-block-tags
                    for tag-var = (block-tag-var-name bname)
                    for env-entry = (assoc tag-var env-locals
                                           :key (lambda (k) (var-name k))
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
                                          :key (lambda (k) (var-name k))
                                          :test #'string=)
                    when env-entry
                    ;; 5th/6th nil (closure go → throw path); 7th preserves the
                    ;; outer tagbody's needs-catch cell so the throw flags it.
                    collect (list tag-name tb-var-name (cdr env-entry) label-idx
                                  nil nil (seventh gt-entry))))
             (param-instrs
               ;; Shared args-array machinery (compile-args-param-instrs):
               ;; args elements load via :load-arg (mapped per closure mode by
               ;; the assembler), the args array itself is ldarg 1, and
               ;; defaults mask current+later params while keeping env
               ;; captures visible (so defaults see the outer scope).
               (compile-args-param-instrs
                required optional key rest-param param-locals n-required key-start
                (lambda (i) `((:load-arg ,i)))
                '((:ldarg 1))
                (lambda (names)
                  (append (remove-if (lambda (entry)
                                       (member (var-name (car entry)) names
                                               :test #'string=))
                                     param-locals)
                          env-locals)))))
        (let* ((arity-instrs
                 (if direct-p
                     ;; :direct closure: arity check moves to the runtime-side
                     ;; args-array wrapper (MakeDirectClosure); the per-arity
                     ;; delegate signature enforces argc on the direct path.
                     ;; Structural guard: :direct must imply the required-only
                     ;; shape — a mismatch means the ignition predicate in
                     ;; compile-closure diverged from this one.
                     (progn
                       (unless (and (null optional) (null key) (null rest-param)
                                    (not has-key-p))
                         (error ":direct closure with non-required-only lambda list: ~s"
                                params))
                       '())
                     (compile-args-arity-instrs fn-name optional key rest-param
                                                has-key-p n-required '((:ldarg 1)))))
               (key-check-instrs
                 ;; Shared with compile-function-body-inner (semantics unified
                 ;; in the fix): bare &key rejects unknown keywords,
                 ;; explicit-package keys use CheckNoUnknownKeys2.
                 (compile-args-key-check-instrs fn-name key has-key-p allow-other-keys-p
                                                key-start '((:ldarg 1)))))
          ;; Handle special params in closure body (declare + globally special)
          (let* ((special-param-syms
                   (union (fn-body-special-params body (mapcar #'var-name all-params))
                          (remove-if-not #'global-special-p all-params)))
                 (sp-names (mapcar #'var-name special-param-syms))
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
                 ;; Do NOT unwrap let*: a special declared inside an &aux-introduced
                 ;; (let* aux ...) only governs the let* BODY, not its init forms, and
                 ;; compile-let* already handles that scoping. Removing it here
                 ;; would wrongly make the aux init forms read the dynamic value.
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
                   (mapcar #'var-name
                           (remove-if (lambda (s)
                                        (member (var-name s)
                                                (mapcar #'var-name all-params)
                                                :test #'string=))
                                      body-declared-specials)))
                 (all-special-names (append sp-names free-special-names))
                 (*locals* (remove-if (lambda (entry)
                                        (member (let ((k (car entry)))
                                                  (if (symbolp k) (var-name k) ""))
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
                   (:ret))))))))))))))



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
                  (:callvirt ,(invoke-name n-call-args))))
              `(,@(let ((*in-mv-context* nil) (*in-tail-position* nil))
                    (compile-expr fn-expr))
                (:call "Runtime.CoerceToFunction")
                ,@(compile-args-array call-args)
                (:callvirt "LispFunction.Invoke")))))))

;;; ============================================================
;;; function special form + flet / labels
;;; ============================================================

;;; (TCO defvars moved to top of file for proper SBCL special-var recognition)

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
    ;; (function (setf name)) — a lexical (flet/labels) (setf name) shadows the
    ;; global one (CLHS 5.1.2.9 / the FUNCTION special form consults the lexical
    ;; environment), so check *local-functions* first, exactly like the symbol
    ;; case below. Without this, #'(setf f) always took the global SetfFunction
    ;; path and the setf-function fallback expansion — (funcall #'(setf f) …) —
    ;; failed with "Undefined function: (SETF F)" inside an flet that binds it
    ;; (eclector's set-standard-syntax-types).
    ((and (consp thing) (eq (car thing) 'setf) (symbolp (cadr thing)))
     (let ((local-fn (assoc (mangle-name thing) *local-functions* :test #'string=)))
       (if local-fn
           (let ((key (second local-fn))
                 (boxed-p (third local-fn)))
             (if boxed-p
                 `((:ldloc ,key) (:ldc-i4 0) (:ldelem-ref))
                 `((:ldloc ,key))))
           `(,@(compile-sym-lookup (cadr thing))
             (:castclass "Symbol")
             (:call "CilAssembler.GetSetfFunctionBySymbol")))))
    ((symbolp thing)
     ;; Check local functions first
     (let ((local-fn (assoc (symbol-name thing) *local-functions* :test #'string=)))
       (if local-fn
           (let ((key (second local-fn))
                 (boxed-p (third local-fn)))
             (if boxed-p
                 `((:ldloc ,key) (:ldc-i4 0) (:ldelem-ref))
                 `((:ldloc ,key))))
           ;; Global function (built-in or user-defined) — use symbol-based lookup
           ;; so that #'sym returns the SAME stable object as (symbol-function sym),
           ;; i.e. (eq #'car #'car) and (eq #'car (symbol-function 'car)) are T.
           ;; A fresh arity-checking wrapper per #' would break eq-on-builtin code
           ;; (memoization, function tables, cl-store's fdefinition round-trip).
           ;; Package-qualified names (e.g. #'bt2:current-thread) resolve to the
           ;; correct package's symbol via compile-fn-sym-lookup. GetFunctionBySymbol
           ;; is authoritative (no cross-package fallback): unqualified #'sym resolves
           ;; via Startup.Sym's bare-name bridge at symbol-resolution time.
           `(,@(compile-fn-sym-lookup thing)
             (:castclass "Symbol")
             (:call "CilAssembler.GetFunctionBySymbol")))))
    (t (error "FUNCTION: unsupported argument ~s" thing))))

;;;; Mini S-expression interpreter for compile-macrolet
;;;; Interprets the expander lambda without Reflection.Emit (NativeAOT/IL2CPP safe).

;; puthash is a compiler intrinsic (no CL standard equivalent). Define a Lisp
;; wrapper here (in dotcl.cil-compiler package) so %mini-eval can call it via
;; symbol-function. The body compiles to (:call "Runtime.Puthash") directly.
(defun puthash (key ht val) (puthash key ht val))

(defun %mini-bind-params (params args env)
  "Bind a lambda list PARAMS to ARGS, extending the ENV alist. Handles required,
   &optional (with default forms and supplied-p), &rest, &key (with default
   forms, supplied-p, and ((:keyword var) ...) form), and &aux. Default and aux
   init forms are evaluated left-to-right in the partial environment."
  (let ((state :required) (new-env env) (rest-args args))
    (dolist (p params)
      (cond
        ((eq p '&optional) (setq state :optional))
        ((eq p '&rest)     (setq state :rest))
        ((eq p '&key)      (setq state :key))
        ((eq p '&aux)      (setq state :aux))
        ((eq p '&allow-other-keys) nil)
        ((eq state :required)
         (setq new-env (acons p (car rest-args) new-env)
               rest-args (cdr rest-args)))
        ((eq state :optional)
         (let* ((var (if (consp p) (car p) p))
                (default (when (consp p) (cadr p)))
                (supp (when (consp p) (caddr p)))
                (present rest-args)
                (val (if present (pop rest-args)
                         (when default (%mini-eval default new-env)))))
           (setq new-env (acons var val new-env))
           (when supp (setq new-env (acons supp (if present t nil) new-env)))))
        ((eq state :rest)
         (setq new-env (acons p rest-args new-env)))
        ((eq state :key)
         ;; p may be VAR, (VAR DEFAULT), (VAR DEFAULT SUPPLIED-P), or
         ;; ((:KEYWORD VAR) DEFAULT [SUPPLIED-P]).
         (let* ((head (if (consp p) (car p) p))
                (var (if (consp head) (cadr head) head))
                (kw (if (consp head) (car head)
                        (intern (symbol-name var) :keyword)))
                (default (when (consp p) (cadr p)))
                (supp (when (consp p) (caddr p)))
                (cell (do ((a rest-args (cddr a))) ((null a) nil)
                        (when (eq (car a) kw) (return a))))
                (val (if cell (cadr cell)
                         (when default (%mini-eval default new-env)))))
           (setq new-env (acons var val new-env))
           (when supp (setq new-env (acons supp (if cell t nil) new-env)))))
        ((eq state :aux)
         (let* ((var (if (consp p) (car p) p))
                (init (when (consp p) (cadr p)))
                (val (when init (%mini-eval init new-env))))
           (setq new-env (acons var val new-env))))))
    new-env))

(defun %mini-eval-progn (forms env)
  ;; Collect vars from (declare (special ...)) that are in env alist,
  ;; then bind them dynamically so closures can see their values.
  (let ((dyn-vars '()) (dyn-vals '()))
    (dolist (f forms)
      (when (and (consp f) (eq (car f) 'declare))
        (dolist (d (cdr f))
          (when (and (consp d) (eq (car d) 'special))
            (dolist (v (cdr d))
              (let ((b (assoc v env)))
                (when b
                  (push v dyn-vars)
                  (push (cdr b) dyn-vals))))))))
    ;; Evaluate all-but-last for effect; return the LAST form's values via a
    ;; tail %mini-eval so multiple values propagate (CL progn semantics).
    (labels ((run (fs)
               (cond ((null fs) nil)
                     ((null (cdr fs)) (%mini-eval (car fs) env))
                     (t (%mini-eval (car fs) env) (run (cdr fs))))))
      (if dyn-vars
          (progv dyn-vars dyn-vals (run forms))
          (run forms)))))

(defun %mini-make-closure (lambda-form env)
  "Return a Lisp function that interprets LAMBDA-FORM in captured ENV."
  (let ((params (cadr lambda-form))
        (body   (cddr lambda-form)))
    (lambda (&rest call-args)
      (%mini-eval-progn body (%mini-bind-params params call-args env)))))

(defun %mini-expand-handler-case (form)
  "Rewrite (handler-case body (type (var) h...) ... [(:no-error ll forms...)])
   into block + handler-bind (+ multiple-value-call for :no-error), reusing
   primitives %mini-eval already supports. The condition clauses become
   handler-bind handlers that non-locally exit the block with their result; a
   :no-error clause receives the body's values when no condition is signalled."
  (let* ((body (cadr form))
         (all-clauses (cddr form))
         (no-error (find :no-error all-clauses :key (lambda (c) (and (consp c) (car c)))))
         (clauses (remove no-error all-clauses))
         (blk (gensym "HC-BLK"))
         (handler-bindings
          (mapcar (lambda (clause)
                    (let* ((type (car clause))
                           (vars (cadr clause))
                           (hbody (cddr clause))
                           (cvar (if vars (car vars) (gensym "HC-C"))))
                      (list type
                            `(lambda (,cvar)
                               ,@(unless vars `((declare (ignore ,cvar))))
                               (return-from ,blk (progn ,@hbody))))))
                  clauses)))
    (if no-error
        `(block ,blk
           (multiple-value-call
               (lambda ,(cadr no-error) ,@(cddr no-error))
             (handler-bind ,handler-bindings ,body)))
        `(block ,blk
           (handler-bind ,handler-bindings ,body)))))

(defvar *%mini-eval-depth* 0)

(defun %mini-eval (form env)
  "Interpret FORM in ENV (alist of (sym . val)). No Reflection.Emit needed."
  (incf *%mini-eval-depth*)
  (unwind-protect
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
             (if (and (consp v) (eq (car v) 'SYMBOL-MACRO))
                 (%mini-eval (cadr v) env)
                 v))
           (multiple-value-bind (sm found) (lookup-symbol-macro form)
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
     ;; First try macroexpand-1: handles destructuring-bind, when, cond, etc.
     (multiple-value-bind (expanded expandedp) (macroexpand-1 form)
       (if expandedp
           (%mini-eval expanded env)
           ;; Dispatch on special form operators
           (let ((op (car form)))
             (case op
               (quote   (cadr form))
               (if      (if (%mini-eval (cadr form) env)
                            (%mini-eval (caddr form) env)
                            (when (cdddr form) (%mini-eval (cadddr form) env))))
               (progn   (%mini-eval-progn (cdr form) env))
               (let
                ;; Special (dynamic) vars bind via progv; lexical vars via alist.
                ;; All init forms are evaluated in the outer env (parallel).
                (let ((spec-vars '()) (spec-vals '()) (lex '()))
                  (dolist (b (cadr form))
                    (let ((var (if (consp b) (car b) b))
                          (val (when (consp b) (%mini-eval (cadr b) env))))
                      (if (%runtime-special-p var)
                          (progn (push var spec-vars) (push val spec-vals))
                          (push (cons var val) lex))))
                  (let ((new-env (append lex env)))
                    (if spec-vars
                        (progv (nreverse spec-vars) (nreverse spec-vals)
                          (%mini-eval-progn (cddr form) new-env))
                        (%mini-eval-progn (cddr form) new-env)))))
               (let*
                ;; Sequential: lexical vars accumulate in new-env; special vars
                ;; are progv'd around the body (their init forms still see prior
                ;; lexical bindings).
                (let ((new-env env) (spec-vars '()) (spec-vals '()))
                  (dolist (b (cadr form))
                    (let ((var (if (consp b) (car b) b))
                          (val (when (consp b) (%mini-eval (cadr b) new-env))))
                      (if (%runtime-special-p var)
                          (progn (push var spec-vars) (push val spec-vals))
                          (push (cons var val) new-env))))
                  (if spec-vars
                      (progv (nreverse spec-vars) (nreverse spec-vals)
                        (%mini-eval-progn (cddr form) new-env))
                      (%mini-eval-progn (cddr form) new-env))))
               (setq
                (let (result)
                  (let ((pairs (cdr form)))
                    (loop while pairs do
                      (let* ((var (car pairs))
                             (val (%mini-eval (cadr pairs) env))
                             (b   (assoc var env)))
                        (if b (setf (cdr b) val) (set var val))
                        (setq result val)
                        (setq pairs (cddr pairs)))))
                  result))
               (function
                (let ((fn (cadr form)))
                  (if (symbolp fn)
                      (symbol-function fn)
                      (if (and (consp fn) (eq (car fn) 'setf))
                          (fdefinition fn)
                          (%mini-make-closure fn env)))))
               (lambda
                (%mini-make-closure form env))
               (flet
                (let ((new-env env))
                  (dolist (def (cadr form))
                    ;; Named local functions get an implicit block named after the
                    ;; function ((setf f) -> block f), so (return-from f ...) works.
                    (let ((bname (if (consp (car def)) (cadr (car def)) (car def))))
                      (push (cons (car def)
                                  (%mini-make-closure
                                   `(lambda ,(cadr def) (block ,bname ,@(cddr def))) env))
                            new-env)))
                  (%mini-eval-progn (cddr form) new-env)))
               (labels
                (let ((cells '()) (new-env env))
                  ;; Pre-allocate cells so closures can mutually reference each other
                  (dolist (def (cadr form))
                    (let ((cell (cons (car def) nil)))
                      (push cell cells)
                      (push cell new-env)))
                  ;; Fill in closures (they capture new-env which already has all names)
                  (dolist (def (cadr form))
                    (let ((cell (assoc (car def) cells))
                          (bname (if (consp (car def)) (cadr (car def)) (car def))))
                      (setf (cdr cell)
                            (%mini-make-closure
                             `(lambda ,(cadr def) (block ,bname ,@(cddr def))) new-env))))
                  (%mini-eval-progn (cddr form) new-env)))
               (block
                ;; Use the block name as catch tag (correct for non-escaped returns)
                (catch (cadr form)
                  (%mini-eval-progn (cddr form) env)))
               (return-from
                (throw (cadr form)
                       (when (cddr form) (%mini-eval (caddr form) env))))
               (the    (%mini-eval (caddr form) env))
               (locally (%mini-eval-progn (cdr form) env))
               (eval-when
                ;; In an evaluator (not compile-file), eval the body iff :execute
                ;; is among the situations (CLHS 3.2.3.1).
                (let ((situations (cadr form)))
                  (when (or (member :execute situations)
                            (member 'cl:eval situations)) ; legacy EVAL keyword
                    (%mini-eval-progn (cddr form) env))))
               (symbol-macrolet
                ;; Extend env with symbol macro bindings
                (let ((new-env env))
                  (dolist (binding (cadr form))
                    (push (cons (car binding) (list 'SYMBOL-MACRO (cadr binding))) new-env))
                  (%mini-eval-progn (cddr form) new-env)))
               (macrolet
                ;; Temporarily extend *macros* with local macros, then eval body.
                ;; Handles the case where a macro (e.g. collect) expands into macrolet
                ;; inside a %mini-eval closure (e.g. in a compile-macrolet expander).
                (let ((saved-macros '()))
                  (unwind-protect
                      (progn
                        (dolist (def (cadr form))
                          (let* ((name (car def))
                                 (params (cadr def))
                                 (mbody (cddr def))
                                 (old (gethash name *macros*))
                                 (expander-fn
                                  (%mini-eval (%macrolet-expander-form params mbody) env)))
                            (push (cons name old) saved-macros)
                            (setf (gethash name *macros*) expander-fn)))
                        (%mini-eval-progn (cddr form) env))
                    (dolist (entry saved-macros)
                      (if (cdr entry)
                          (setf (gethash (car entry) *macros*) (cdr entry))
                          (remhash (car entry) *macros*))))))
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
                       ;; Extend env with (tag . (GO-TARGET tb-id idx)) for each tag
                       (tagged-env
                        (let ((e env) (idx 0))
                          (dolist (seg segs)
                            (when (car seg)
                              (push (cons (car seg) (list 'GO-TARGET tb-id idx)) e))
                            (incf idx))
                          e))
                       (done-marker (list 'done))
                       (start-idx 0))
                  (loop
                    (let ((result (catch tb-id
                                    (let ((idx 0))
                                      (dolist (seg segs)
                                        (when (>= idx start-idx)
                                          (dolist (f (cdr seg))
                                            (%mini-eval f tagged-env)))
                                        (incf idx)))
                                    done-marker)))
                      (if (eq result done-marker)
                          (return nil)
                          (setq start-idx result))))))
               (go
                (let* ((tag (cadr form))
                       (b (assoc tag env)))
                  (if (and b (consp (cdr b)) (eq (car (cdr b)) 'GO-TARGET))
                      (throw (cadr (cdr b)) (caddr (cdr b)))
                      (error "%mini-eval: go tag ~S not found" tag))))
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
                ;; (m-v-call fn form*) — splice all values of each form as args.
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
                ;; implicit block) as the function. No compilation — works on
                ;; emit-free targets. &key/&aux in the lambda list are not yet
                ;; bound by %mini-bind-params (required/&optional/&rest only).
                (let* ((name (cadr form))
                       (params (caddr form))
                       (fbody (cdddr form))
                       (bname (if (consp name) (cadr name) name))
                       (fn (%mini-make-closure
                            `(lambda ,params (block ,bname ,@fbody))
                            env)))
                  (setf (fdefinition name) fn)
                  name))
               (defvar
                (let ((name (cadr form)))
                  (proclaim (list 'special name))
                  (when (and (cddr form) (not (boundp name)))
                    (set name (%mini-eval (caddr form) env)))
                  name))
               (defparameter
                (let ((name (cadr form)))
                  (proclaim (list 'special name))
                  (set name (%mini-eval (caddr form) env))
                  name))
               (defconstant
                (let ((name (cadr form)))
                  (proclaim (list 'special name))
                  (set name (%mini-eval (caddr form) env))
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
                  name))
               (handler-bind
                ;; (handler-bind ((type handler-form) ...) body...)
                ;; Establish a cluster via the runtime primitive, then run body
                ;; as a 0-arg interpreted closure under it.
                (%call-with-handler-cluster
                 (mapcar (lambda (b)
                           (cons (car b) (%mini-eval (cadr b) env)))
                         (cadr form))
                 (%mini-make-closure (list* 'lambda '() (cddr form)) env)))
               (handler-case
                (%mini-eval (%mini-expand-handler-case form) env))
               (restart-case
                ;; (restart-case body (name (params) [:report/:interactive/:test x]* h...) ...)
                (%call-with-restart-cluster
                 (mapcar (lambda (clause)
                           (let ((name (car clause))
                                 (params (cadr clause))
                                 (rest (cddr clause)))
                             (loop while (and rest (member (car rest)
                                                           '(:report :interactive :test)))
                                   do (setq rest (cddr rest)))
                             (cons name
                                   (%mini-make-closure `(lambda ,params ,@rest) env))))
                         (cddr form))
                 (%mini-make-closure (list 'lambda '() (cadr form)) env)))
               (restart-bind
                ;; (restart-bind ((name function [:report/:interactive/:test x]*) ...) body...)
                (%call-with-restart-bind
                 (mapcar (lambda (binding)
                           (cons (car binding) (%mini-eval (cadr binding) env)))
                         (cadr form))
                 (%mini-make-closure (list* 'lambda '() (cddr form)) env)))
               (multiple-value-list
                (multiple-value-list (%mini-eval (cadr form) env)))
               (%make-instance-with-initargs
                ;; Compiler intrinsic for (make-instance 'class ...): delegate to GF.
                (apply (symbol-function 'make-instance)
                       (mapcar (lambda (a) (%mini-eval a env)) (cdr form))))
               (t
                ;; Function call: a local binding, a symbol's function, a
                ;; (setf name) function designator in operator position (e.g.
                ;; the ((setf foo) v place) form setf expands to), or a
                ;; ((lambda ...) ...) / computed-function operator.
                (let* ((fn (cond
                             ((symbolp op)
                              (let ((b (assoc op env)))
                                (if (and b (functionp (cdr b)))
                                    (cdr b)
                                    (symbol-function op))))
                             ((and (consp op) (eq (car op) 'setf))
                              (fdefinition op))
                             (t (%mini-eval op env))))
                       (args (mapcar (lambda (a) (%mini-eval a env)) (cdr form))))
                  (apply fn args))))))))
    (t form))
  (decf *%mini-eval-depth*)))

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
        ;; Build a macro expander lambda and eval it. &whole / &environment are
        ;; handled by the shared %macrolet-expander-form (same builder the
        ;; %mini-eval MACROLET case uses) so the compiled and interpreted paths
        ;; never diverge.
        (progn
          (progn
            (let* ((expander-form (%macrolet-expander-form params mbody))
                   ;; Wrap with surrounding flet so macrolet body can call
                   ;; enclosing locally-defined functions at expansion time.
                   ;; Use %mini-eval instead of eval: no Reflection.Emit needed.
                   (eval-form (if *compile-time-flet-defs*
                                  `(flet ,*compile-time-flet-defs* ,expander-form)
                                  expander-form))
                   (expander-fn (%mini-eval eval-form nil)))
              (setf (gethash name *macros*) expander-fn))))))
    ;; Compile body with local macros active
    ;; Handle (declare (special ...)) in body — remove those vars from *locals*
    ;; so they use dynamic binding (like locally does)
    (multiple-value-bind (declared-specials real-body) (extract-specials body)
    (let* ((*locals* (if declared-specials
                         (remove-if (lambda (entry)
                                      (member (var-name (car entry))
                                              (mapcar #'var-name declared-specials)
                                              :test #'string=))
                                    *locals*)
                         *locals*)))
      (unwind-protect
          ;; Push this macrolet onto *macroexpand-scope* so a form shared (by a
          ;; splicing macro) between this shadowing scope and an outer scope is
          ;; cached separately per scope. MACRO-DEFS is the source
          ;; cons the analysis walk pushes too, keeping the two passes in sync.
          (let ((*macroexpand-scope* (cons macro-defs *macroexpand-scope*)))
            (compile-progn real-body))
        (dolist (entry saved)
          (if (cdr entry)
              (setf (gethash (car entry) *macros*) (cdr entry))
              (remhash (car entry) *macros*))))))))

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
  (let* ((sm-names (mapcar (lambda (b) (var-name (car b))) bindings))
         (*symbol-macros* (append (mapcar (lambda (b) (cons (car b) (cadr b)))
                                          bindings)
                                  *symbol-macros*))
         ;; A symbol-macro shadows an enclosing lexical variable of the same name
         ;; inside the body (CLHS 5.1.2.1: inner binding wins). Drop such locals so
         ;; compile-var-ref falls through lookup-local to the symbol-macro instead of
         ;; resolving the outer variable. Without this, e.g. (with-accessors ((x acc))
         ;; x x) — where the accessor var name equals the instance-form variable —
         ;; read the instance itself, not (acc instance).
         (*locals* (remove-if
                    (lambda (entry)
                      (let ((k (car entry)))
                        (member (if (symbolp k) (var-name k) k)
                                sm-names :test #'string=)))
                    *locals*))
         ;; Push this symbol-macrolet onto *macroexpand-scope* (like compile-macrolet)
         ;; so a macro call form shared by a splicing macro between this scope and an
         ;; outer/sibling scope is cached separately per scope. Without this, a
         ;; recursive macro that nests symbol-macrolet and splices its body into both
         ;; if-arms (serapeum %with-boolean) reuses the outer-scope expansion for the
         ;; inner-scope form — the inner symbol-macro binding is lost. BINDINGS
         ;; is the source cons the analysis walk pushes too, keeping the passes in sync.
         (*macroexpand-scope* (cons bindings *macroexpand-scope*)))
    ;; Process free special declarations (like locally does)
    (multiple-value-bind (declared-specials real-body) (extract-specials body)
      (if (null declared-specials)
          (compile-progn real-body)
          (let* ((*specials* (append declared-specials *specials*))
                 (special-names (mapcar #'var-name declared-specials))
                 (*locals* (remove-if
                            (lambda (entry)
                              (let ((k (car entry)))
                                (member (if (symbolp k) (var-name k) "")
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
          ;; Track in *locals* so closures can capture flet functions.
          ;; Use BOTH the plain name and the __LABELFN_ prefix (same key):
          ;; - Plain name: backward compat (#'flet-fn value capture)
          ;; - __LABELFN_ prefix: capture in function call position without
          ;;   conflicting with a same-named let/let* variable (Lisp-2 separation).
          ;;   The free var analysis detects __LABELFN_ entries in function position
          ;;   (see find-free-vars-expr), and compile-closure-body prefers it.
          (when (symbolp name)
            (push (cons (intern (symbol-name name) :dotcl.cil-compiler) key) new-locals)
            (push (cons (intern (concatenate 'string "__LABELFN_" (symbol-name name))
                                :dotcl.cil-compiler)
                        key)
                  new-locals)))))
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

;;; --- Speculative labels-self-TCO acceptance scan ---
;;; compile-labels-boxed speculatively compiles a single self-recursive labels
;;; function through the direct+TCO path (see *labels-direct-speculation*). The
;;; generated instruction tree is accepted ONLY when both scans pass, so the
;;; failure mode is always "fall back to the closure path", never a miscompile.

(defun %instr-tree-contains-symbol-p (tree sym)
  "T if the interned symbol SYM (a labels box-key from gen-local) appears
   anywhere in the instruction TREE. Any occurrence means the box was actually
   referenced (as a value / non-tail call / #'g), so the direct compile is not
   self-contained — scan (a)."
  (cond ((eq tree sym) t)
        ((consp tree)
         (or (%instr-tree-contains-symbol-p (car tree) sym)
             (%instr-tree-contains-symbol-p (cdr tree) sym)))
        (t nil)))

(defun %tco-loop-target (instr)
  "If INSTR is a (:br L) / (:leave L) to a TCO loop label (TCOLOOP* or LMTCO*),
   return L, else NIL."
  (and (consp instr)
       (member (car instr) '(:br :leave))
       (let ((tgt (cadr instr)))
         (and (stringp tgt)
              (or (and (>= (length tgt) 7) (string= tgt "TCOLOOP" :end1 7))
                  (and (>= (length tgt) 5) (string= tgt "LMTCO" :end1 5)))
              tgt))))

(defun %instr-list-tco-orphan-free-p (instrs)
  "Scan ONE instruction list: after a TCO-loop branch, until the next
   (:label ...), only :br / :leave / :ret may appear. Anything else is dead code
   stranded by a TCO rewrite (e.g. an intrinsic call whose result was discarded
   because its argument tail-recursed) — reject. Scan (b)."
  (let ((in-dead nil))
    (dolist (instr instrs t)
      (cond
        ((and (consp instr) (eq (car instr) :label))
         (setq in-dead nil))
        (in-dead
         (unless (and (consp instr) (member (car instr) '(:br :leave :ret)))
           (return nil)))
        ((%tco-loop-target instr)
         (setq in-dead t))))))

(defun %instr-nested-body (instr)
  "The nested :body instruction list of a make-function/-direct/closure op, or
   NIL. Restricted to those ops so GETF never touches a non-plist instruction
   like (:ldc-i4 0)."
  (when (and (consp instr)
             (member (car instr)
                     '(:make-function :make-function-direct :make-closure)))
    (getf (cdr instr) :body)))

(defun %instr-tree-tco-orphan-free-p (instrs)
  "Apply %instr-list-tco-orphan-free-p to INSTRS and recursively to every nested
   :body instruction list (each nested make-function/-direct/closure is its own
   method)."
  (and (%instr-list-tco-orphan-free-p instrs)
       (every (lambda (instr)
                (let ((body (%instr-nested-body instr)))
                  (or (not (consp body))
                      (%instr-tree-tco-orphan-free-p body))))
              instrs)))

(defun labels-direct-speculation-acceptable-p (instrs box-key)
  "T iff INSTRS is a single :make-function-direct whose body is provably safe for
   labels self-TCO: it took the direct path, references BOX-KEY nowhere (scan a),
   and strands no orphan code after a TCO branch (scan b)."
  (and (consp instrs)
       (null (cdr instrs))
       (consp (car instrs))
       (eq (caar instrs) :make-function-direct)
       (not (%instr-tree-contains-symbol-p instrs box-key))
       (%instr-tree-tco-orphan-free-p instrs)))

(defun compile-labels (fn-defs body)
  "Compile (labels ((name (params) body...) ...) body...).
   When all functions have the same required-only arity (>= 2 fns), uses a dispatch
   loop for mutual tail call optimization. Otherwise uses boxed closures."
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
           ;; Always use __LABELFN_ prefix so labels boxes and let variables with
           ;; the same name can coexist in *locals*. find-free-vars-expr detects the
           ;; __LABELFN_ entry when a labels function appears in function position.
           (new-locals (compile-labels-build-new-locals new-local-fns))
           (*locals* (append new-locals *locals*))
           (*boxed-vars* (append (mapcar #'car new-locals)
                                 *boxed-vars*))
           (store-instrs '()))
      ;; Compile each function and store into its box.
      ;; CL spec: labels creates an implicit block named after the function.
      ;; For (setf sym) names, use progn instead of block (block requires a symbol).
      ;; Pass name-str + set *tco-local-fn-key* so compile-lambda enables self-TCO.
      (dolist (entry fn-compile-list)
        (let ((name (first entry))
              (params (second entry))
              (fn-body (third entry))
              (key (fourth entry)))
          (let* ((name-str (mangle-name name))
                 ;; Rebind *tco-self-symbol* to THIS labels function's symbol:
                 ;; the self-call identity check must match g, not a stale
                 ;; enclosing defun (whose symbol would make a tail call to
                 ;; that outer defun falsely TCO-branch to g's loop).
                 (compile-thunk
                   (lambda ()
                     (let ((*tco-local-fn-key* key)
                           (*tco-self-symbol* (if (symbolp name) name nil)))
                       (if (and (symbolp name)
                                (some (lambda (f) (form-has-return-from-p name f)) fn-body))
                           (compile-lambda params `((block ,name ,@fn-body)) name-str)
                           (compile-lambda params fn-body name-str)))))
                 ;; Speculative direct+TCO: try compiling this labels fn
                 ;; through the direct path so a single self-tail-recursion is
                 ;; TCO'd instead of overflowing. Accept only when the generated
                 ;; body is provably self-contained; otherwise recompile via the
                 ;; unchanged closure path. Symbol-named fns only (the box key is
                 ;; matched by name in compile-lambda / -function-body-direct).
                 (lambda-instrs
                   (if (symbolp name)
                       (let ((spec-instrs
                               (let ((*labels-direct-speculation* (cons name-str key)))
                                 (funcall compile-thunk))))
                         (if (labels-direct-speculation-acceptable-p spec-instrs key)
                             spec-instrs
                             (funcall compile-thunk)))
                       (funcall compile-thunk))))
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
                  (let* ((local-sym (intern (concatenate 'string "__LABELFN_" name-str)
                                            :dotcl.cil-compiler)))
                    (cons local-sym (second lf))))))
            new-local-fns)))

(defun compile-labels-mutual-tco (fn-defs body arity)
  "Compile (labels ...) where all functions have the same required ARITY.
   Emits closures in boxes for non-tail calls, plus a dispatch loop for
   mutual tail call optimization.
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
                 ;; Rebind *tco-self-symbol* per labels function (see
                 ;; compile-labels-boxed) — a stale enclosing defun symbol
                 ;; falsely matches tail calls to that outer defun.
                 (lambda-instrs (let ((*tco-local-fn-key* key)
                                      (*tco-self-symbol* (if (symbolp name) name nil)))
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
                                  ;; The section binds each param to a plain shared
                                  ;; LispObject local. An outer variable of the SAME
                                  ;; name may be boxed or numerically typed; those
                                  ;; per-name context lists must be shadowed here or
                                  ;; a param reference compiles as a box-deref /
                                  ;; native unbox of a raw value.
                                  (param-names (mapcar #'var-name params))
                                  (shadowed-p (lambda (x)
                                                (member (if (symbolp x) (var-name x) x)
                                                        param-names :test #'string=)))
                                  (shadowed-key-p (lambda (e)
                                                    (member (if (consp e) (car e) e)
                                                            param-names :test #'string=)))
                                  (fn-instrs
                                    (let ((*locals*
                                            (append (loop for p in params
                                                          for key in shared-param-keys
                                                          collect (cons p key))
                                                    *locals*))
                                          (*boxed-vars* (remove-if shadowed-p *boxed-vars*))
                                          (*fixnum-locals* (remove-if shadowed-key-p *fixnum-locals*))
                                          (*small-int-locals* (remove-if shadowed-key-p *small-int-locals*))
                                          (*double-float-locals* (remove-if shadowed-key-p *double-float-locals*))
                                          (*single-float-locals* (remove-if shadowed-key-p *single-float-locals*))
                                          (*decimal-locals* (remove-if shadowed-key-p *decimal-locals*))
                                          (*long-locals* (remove-if shadowed-key-p *long-locals*))
                                          (*numeric-array-locals* (remove-if shadowed-key-p *numeric-array-locals*))
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
   correctly to an outer (catch ...) even across the eval boundary."
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
      ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
          (compile-expr tag-expr))
      (:stloc ,tag-key)
      ;; result-expr's multiple values become the catch's return values
      ;; regardless of where the throw form sits (CLHS 5.2); preserve
      ;; MvReturn wrapping like return-from does. This must not depend on
      ;; the ambient MV context of the position the throw appears in.
      ,@(let ((*in-tail-position* nil) (*in-mv-context* t))
          (compile-expr result-expr))
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

(defun sil-references-symbol-p (instrs sym)
  "Deep scan: does SYM appear anywhere in the SIL tree INSTRS (including nested
   :body lists for hoisted closures)? Used by compile-tagbody to decide whether
   the tagbody id local is referenced — by a non-local go's GoException throw
   OR by a closure capturing the id into its env — in which case the GoException
   try/catch must be kept. A structural check, immune to compile ordering."
  (labels ((walk (x)
             (cond ((eq x sym) t)
                   ((consp x) (or (walk (car x)) (walk (cdr x))))
                   (t nil))))
    (walk instrs)))

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
         ;; needs-catch: shared cell flagged by compile-go when it emits a
         ;; NON-LOCAL (GoException throw) go targeting this tagbody. If none is
         ;; emitted while compiling the segments, the catch is dead and omitted
         ;; (mirrors compile-block). Eliding the per-iteration try is a large
         ;; win for hot loops (dotimes/do/loop → tagbody).
         (needs-catch (list nil))
         ;; Extended format: (tag-name tb-var-name tb-id-key label-idx index-key
         ;;                    leave-label needs-catch). 5th/6th enable local go;
         ;; 7th lets a captured non-local go flag this tagbody's needs-catch.
         (*go-tags* (append
                     (mapcar (lambda (ti) (list (car ti) tb-var-name tb-id-key (cdr ti)
                                                index-key loop-label needs-catch))
                             tag-indices)
                     *go-tags*))
         ;; Compile segments NOW so compile-go runs (and may flag needs-catch)
         ;; before we decide whether the GoException try/catch is needed.
         (seg-instrs (let ((*in-tail-position* nil) (*in-mv-context* nil))
                       (loop for seg in segments
                             for label in seg-labels
                             append `((:label ,label)
                                      ,@(loop for form in (cdr seg)
                                              append (compile-and-pop form)))))))
    (if (or (car needs-catch)
            ;; tb-id-key referenced in the body ⇒ a non-local go's throw or a
            ;; closure capturing the tagbody id ⇒ the catch is required. This
            ;; structural check backs up the needs-catch side-effect flag.
            (sil-references-symbol-p seg-instrs tb-id-key))
        ;; A non-local go can land here: keep the GoException try/catch.
        `((:declare-local ,tb-id-key "LispObject")
          (:ldsfld "Nil.Instance") (:ldsfld "Nil.Instance") (:call "Runtime.MakeCons")
          (:stloc ,tb-id-key)
          (:declare-local ,index-key "Int32")
          (:ldc-i4 0) (:stloc ,index-key)
          (:declare-local ,done-key "Boolean")
          (:ldc-i4 0) (:stloc ,done-key)
          (:label ,loop-label)
          (:ldloc ,done-key) (:brtrue ,end-label)
          (:begin-exception-block)
          (:ldloc ,index-key)
          (:switch ,seg-labels)
          (:br ,leave-label)
          ,@seg-instrs
          (:label ,leave-label)
          (:ldc-i4 1) (:stloc ,done-key)
          (:leave ,loop-label)
          (:begin-catch-block "GoException")
          (:declare-local ,ex-key "GoException")
          (:stloc ,ex-key)
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
          (:label ,end-label)
          (:call "MultipleValues.Reset")  ; tagbody return is always single NIL
          ,@(emit-nil))
        ;; All go's are local: no GoException can target this tagbody. Skip the
        ;; try/catch entirely. tb-id-key is unreferenced and needs no slot.
        `((:declare-local ,index-key "Int32")
          (:ldc-i4 0) (:stloc ,index-key)
          (:declare-local ,done-key "Boolean")
          (:ldc-i4 0) (:stloc ,done-key)
          (:label ,loop-label)
          (:ldloc ,done-key) (:brtrue ,end-label)
          (:ldloc ,index-key)
          (:switch ,seg-labels)
          (:br ,leave-label)
          ,@seg-instrs
          (:label ,leave-label)
          (:ldc-i4 1) (:stloc ,done-key)
          (:leave ,loop-label)
          (:label ,end-label)
          (:call "MultipleValues.Reset")
          ,@(emit-nil)))))

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
          (local-loop-label (sixth entry))
          (needs-catch (seventh entry)))
      (if (and local-index-key (not *in-finally-block*))
          ;; Local go: set index and leave try block — no exception
          `((:ldc-i4 ,label-idx)
            (:stloc ,local-index-key)
            (:leave ,local-loop-label))
          ;; Non-local go (or go from finally block): throw GoException. Flag the
          ;; target tagbody so it keeps its GoException catch.
          (progn
            (when needs-catch (setf (car needs-catch) t))
            `((:ldloc ,tb-id-key)
              (:ldc-i4 ,label-idx)
              (:newobj "GoException")
              (:throw)))))))

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

(defun %handler-type-spec-load (type-spec)
  "Instructions that push the handler clause's type specifier as a LispObject for
   Runtime.Typep. A bare symbol / class metaobject loads its name symbol (Typep
   matches CL condition types by name); a compound specifier — (or ...), (and ...),
   (member ...), a deftype, etc. — is loaded as its literal list so Typep evaluates
   it (was previously stringified into one bogus symbol, so (or ...) never matched)."
  (cond
    ((symbolp type-spec) `((:ldstr ,(symbol-name type-spec)) (:call "Startup.Sym")))
    ((and (typep type-spec 'standard-object) (class-name type-spec))
     `((:ldstr ,(symbol-name (class-name type-spec))) (:call "Startup.Sym")))
    (t (compile-quoted type-spec))))

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
                                  (lambda-list (cadr clause))
                                  (var (if (and lambda-list (car lambda-list))
                                           (car lambda-list)
                                           nil))
                                  (handler-body (cddr clause)))
                             (list type-spec var handler-body)))
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
      ,@(loop for (type-spec var handler-body) in parsed
              for i from 0
              append `((:dup) (:ldc-i4 ,i)
                        ;; Type specifier (symbol/class name, or compound list literal)
                        ,@(%handler-type-spec-load type-spec)
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
      ;; PopCluster so each iteration has a clean handler stack.
      ,@(let ((*in-try-block* t)         ; protect: :ret invalid in try/catch region
              (*tco-in-try-catch* (if *tco-self-name* t *tco-in-try-catch*))
              (*in-mv-context* t)
              ;; When TCO is active, prepend PopCluster to *tco-leave-instrs* so the
              ;; self-call emits PopCluster before `leave TCOLOOP`.
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
      ,@(loop for (type-spec var handler-body) in parsed
              for label in clause-labels
              for skip in le-skip-labels
              append `((:ldloc ,cond-key)
                       ,@(%handler-type-spec-load type-spec)
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
      (:ldloc ,dotnet-ex-key)
      (:call "Runtime.WrapDotNetExceptionObj")
      (:stloc ,cond-key)
      ,@(loop for (type-spec var handler-body) in parsed
              for label in clause-labels
              for skip in dn-skip-labels
              append `((:ldloc ,cond-key)
                       ,@(%handler-type-spec-load type-spec)
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
      ,@(loop for (type-spec var handler-body) in parsed
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
                                            *locals*))
                              ;; The clause var binds the condition into a plain
                              ;; LispObject slot; it must SHADOW an enclosing boxed
                              ;; var of the same name (e.g. a boxed LOOP variable
                              ;; captured by this same-named handler), else its
                              ;; reference compiles to a boxed `slot[0]` ldelem-ref
                              ;; on the condition object → ArrayTypeMismatch.
                              (*boxed-vars* (if (and var (not var-is-special))
                                                (remove-if (lambda (x)
                                                             (string= (if (symbolp x) (var-name x) x)
                                                                      (var-name var)))
                                                           *boxed-vars*)
                                                *boxed-vars*)))
                         (let ((var-key (if (and var (not var-is-special))
                                            (lookup-local var) nil)))
                           `((:label ,label)
                             (:call "HandlerClusterStack.PopCluster")
                             ,@(cond
                                 (var-is-special
                                  ;; Bind as special (dynamic) variable with try/finally.
                                  ;; Set *in-try-block* before compiling body so TCO hooks
                                  ;; emit `leave` instead of `br` (mirrors compile-let).
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
                     ;; Intermediate arg: NOT in tail position — its value is
                     ;; consumed by the IsTruthy test, not returned. Binding
                     ;; *in-tail-position* nil prevents a self-call here from
                     ;; being TCO-rewritten into a value-discarding loop-back
                     ;; (symmetric with compile-or).
                     append `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr arg))
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
                     ;; Intermediate arg: NOT in tail position — its value is
                     ;; consumed by the IsTruthy test, not returned. Binding
                     ;; *in-tail-position* nil prevents a self-call here from
                     ;; being TCO-rewritten into a value-discarding loop-back,
                     ;; which made e.g. (or (f (car x)) (f (cdr x))) return the
                     ;; wrong value on deep inputs. The last arg
                     ;; below keeps tail position so genuine tail self-calls
                     ;; still TCO.
                     append `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr arg))
                              (:call "MultipleValues.Primary")
                              (:stloc ,result-key)
                              (:ldloc ,result-key)
                              (:call "Runtime.IsTruthy")
                              (:brtrue ,end-label))
                   else
                     ;; Last arg: pass through all values (tail position kept)
                     append `(,@(compile-expr arg)
                              (:stloc ,result-key)))
           (:label ,end-label)
           (:ldloc ,result-key))))))

(defun compile-cond (clauses &optional shared-tmp)
  "Compile (cond (test1 body1...) ...) to nested conditional.
   shared-tmp: shared LispObject local for no-body arms."
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
                        ;; Default: normalize MV state, then IsTruthy.
                        ;; The test is never in tail position (only the body is);
                        ;; binding *in-tail-position* nil prevents a self-recursive
                        ;; call in the test from being TCO'd into a jump.
                        `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr test))
                          (:call "MultipleValues.Primary")
                          (:call "Runtime.IsTruthy")
                          (:brfalse ,else-label)
                          ,@(compile-progn body)
                          (:br ,end-label)
                          (:label ,else-label)
                          ,@(compile-cond (cdr clauses) shared-tmp)
                          (:label ,end-label))))
                    ;; No body: all no-body arms share one CTMP slot
                    (let ((tmp (or shared-tmp (gen-local "CTMP"))))
                      ;; Test-only arm: the test value becomes the result, but the
                      ;; test itself is not in tail position — a self-recursive call
                      ;; here must compute a value, not TCO-jump.
                      `(,@(unless shared-tmp `((:declare-local ,tmp "LispObject")))
                        ,@(let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr test))
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

(defun compile-value-arg (arg)
  "Compile ARG as a known-function intrinsic argument: never in tail position (the
   consuming intrinsic call is), never in MV context (a single value is taken).
   Without this, a self-tail-call in an intrinsic argument inherits the tail
   context, fires TCO (a br/leave to the TCO loop), and the intrinsic call that
   follows becomes unreachable dead code — its side effect silently lost
   (e.g. (defun f (n) (if (zerop n) 0 (print (f (1- n))))) never printing). Mirrors
   compile-unary-call / compile-args-array, which already bind both. NOT for
   tail/MV-transparent intrinsics (the/when/unless/min/max/values/…): those use a
   bare (compile-expr …) as their whole result and must keep the caller's context."
  (let ((*in-tail-position* nil) (*in-mv-context* nil))
    (compile-expr arg)))

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
              (1 (let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args))))
              ;; 2-arg: direct Runtime.Nconc2 (semantics match the stdlib &rest
              ;; defun exactly) — the dominant call shape, e.g. the liveness
              ;; analysis' (nconc writes reads). Unconditional inline, same
              ;; precedent as the rplacd/append handlers here (builtin handlers
              ;; don't consult notinline; only compiler macros do, CLHS 3.2.2.1.1).
              ;; NOTE: Runtime.Nconc2 is a different, COPYING helper (CLOS
              ;; method combination) — NconcDestructive2 is the CL-semantics one.
              (2 (compile-binary-call args "Runtime.NconcDestructive2" "NCONC"))
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

  ;; Array ops. When every index is statically fixnum-typed (e.g. an
  ;; Int64-slot loop counter), lower the indices as raw longs and call the
  ;; *L runtime variants — no per-access Fixnum boxing on either side.
  (setf (gethash 'svref h)
        (lambda (expr)
          (let ((args (cdr expr)))
            (if (and (= (length args) 2) (fixnum-typed-p (second args)))
                (compile-aref-native-index-call
                 (first args) (list (second args)) "Runtime.ArefL")
                (compile-binary-call args "Runtime.Aref")))))
  (setf (gethash 'aref h)
        (lambda (expr)
          (let* ((args (cdr expr))
                 (idxs (cdr args))
                 (num-info (numeric-array-aref-info expr))
                 (float-kind (numeric-array-aref-float-kind expr))
                 (native (and (<= 2 (length args) 4)
                              (every #'fixnum-typed-p idxs))))
            (cond
              ;; Numeric-backed array local: raw long element read, boxed once
              ;; here for the value context (native consumers go through
              ;; compile-as-long and never see the box).
              (num-info
               `(,@(compile-numeric-aref-as-long (first args) idxs (car num-info))
                 (:call "Fixnum.Make")))
              ;; Float-backed array local: raw r8 element read, boxed once here
              ;; (native consumers go through compile-as-double/single).
              (float-kind
               `(,@(compile-numeric-aref-float
                    (first args) idxs (car (numeric-array-aref-entry expr)))
                 ,@(ecase float-kind
                     (:double '((:newobj "DoubleFloat")))
                     (:single '((:conv-r4) (:newobj "SingleFloat"))))))
              (t
               (case (length args)
                 (2 (if native
                        (compile-aref-native-index-call (first args) idxs "Runtime.ArefL")
                        (compile-binary-call args "Runtime.Aref")))
                 (3 (if native
                        (compile-aref-native-index-call (first args) idxs "Runtime.Aref2DL")
                        (compile-ternary-call args "Runtime.Aref2D")))
                 (4 (if native
                        (compile-aref-native-index-call (first args) idxs "Runtime.Aref3DL")
                        (compile-quaternary-call args "Runtime.Aref3D")))
                 (t `(,@(compile-args-array args) (:call "Runtime.ArefMulti")))))))))
  (setf (gethash '%aref-set h)
        (lambda (expr)
          (let* ((args (cdr expr))
                 (idxs (butlast (cdr args)))
                 (val (car (last args)))
                 (aref-form `(aref ,(first args) ,@idxs))
                 (num-info (and (fixnum-typed-p val)
                                (numeric-array-aref-info aref-form)))
                 (float-kind (numeric-array-aref-float-kind aref-form))
                 (native (and (<= 3 (length args) 5)
                              (every #'fixnum-typed-p idxs))))
            (cond
              ;; Numeric-backed array local with an integer-typed value: raw
              ;; long store, no boxing on either the indices or the value.
              (num-info
               (compile-numeric-aref-set (first args) idxs val (car num-info)))
              ;; Float-backed array local with a matching float-typed value: raw
              ;; r8 store, no boxing. A mismatched value type (e.g. storing a
              ;; single into a double array, or an int) falls to the general
              ;; path, which coerces on store.
              ((and float-kind
                    (ecase float-kind
                      (:double (double-float-typed-p val))
                      (:single (single-float-typed-p val))))
               (compile-numeric-aref-set-float
                (first args) idxs val
                (car (numeric-array-aref-entry aref-form)) float-kind))
              (t
               (case (length args)
                 (3 (if native
                        (compile-aref-native-index-call (first args) idxs "Runtime.ArefSetL" val)
                        (compile-ternary-call args "Runtime.ArefSet")))
                 (4 (if native
                        (compile-aref-native-index-call (first args) idxs "Runtime.ArefSet2DL" val)
                        (compile-quaternary-call args "Runtime.ArefSet2D")))
                 (5 (if native
                        (compile-aref-native-index-call (first args) idxs "Runtime.ArefSet3DL" val)
                        (compile-quinary-call args "Runtime.ArefSet3D")))
                 (t `(,@(compile-args-array args) (:call "Runtime.ArefSetMulti")))))))))
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
          (let ((*at-toplevel* *compile-was-toplevel*)
                ;; (locally (declare (notinline f)) ...) extends the notinline set
                ;; for the body (CLHS 3.2.2.1.1).
                (*notinline-functions* (append (extract-notinline (cdr expr)) *notinline-functions*)))
            (multiple-value-bind (declared-specials real-body) (extract-specials (cdr expr))
              (if (null declared-specials)
                  (compile-progn real-body)
                  (let* ((*specials* (append declared-specials *specials*))
                         (special-names (mapcar #'var-name declared-specials))
                         (*locals* (remove-if
                                    (lambda (entry)
                                      (let ((k (car entry)))
                                        (member (if (symbolp k) (var-name k) "")
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
          `(,@(compile-value-arg (cadr expr))
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
          `(,@(compile-value-arg (cadr expr))
            ,@(compile-args-array (cddr expr))
            (:call "Runtime.MakeStruct"))))
  (setf (gethash '%struct-ref h)
        (lambda (expr)
          (let ((obj (first (cdr expr))) (idx (second (cdr expr))))
            (if (integerp idx)
                `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                      (compile-expr obj))
                  (:ldc-i4 ,idx) (:call "Runtime.StructRefI"))
                (compile-binary-call (cdr expr) "Runtime.StructRef")))))
  (setf (gethash '%struct-set h)
        (lambda (expr)
          (let ((obj (first (cdr expr))) (idx (second (cdr expr))) (val (third (cdr expr))))
            (if (integerp idx)
                `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                      (compile-expr obj))
                  (:ldc-i4 ,idx)
                  ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                      (compile-expr val))
                  (:call "Runtime.StructSetI"))
                `(,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                      `(,@(compile-value-arg obj)
                        ,@(compile-value-arg idx)
                        ,@(compile-value-arg val)))
                  (:call "Runtime.StructSet"))))))
  (setf (gethash '%struct-typep h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.StructTypep")))
  (setf (gethash '%copy-struct h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.CopyStruct")))

  ;; CLOS primitives
  (setf (gethash '%make-class h)
        (lambda (expr)
          `(,@(compile-value-arg (second expr))
            ,@(compile-value-arg (third expr))
            ,@(compile-value-arg (fourth expr))
            (:call "Runtime.MakeClass"))))
  (setf (gethash '%make-class-full h)
        (lambda (expr)
          `(,@(compile-value-arg (second expr))
            ,@(compile-value-arg (third expr))
            ,@(compile-value-arg (fourth expr))
            ,@(compile-value-arg (fifth expr))
            (:call "Runtime.MakeClassFull"))))
  (setf (gethash '%make-slot-def h)
        (lambda (expr)
          `(,@(compile-value-arg (second expr))
            ,@(compile-value-arg (third expr))
            ,@(compile-value-arg (fourth expr))
            (:call "Runtime.MakeSlotDef"))))
  (setf (gethash '%make-slot-def-with-allocation h)
        (lambda (expr)
          `(,@(compile-value-arg (second expr))
            ,@(compile-value-arg (third expr))
            ,@(compile-value-arg (fourth expr))
            ,@(compile-value-arg (fifth expr))
            (:call "Runtime.MakeSlotDefWithAllocation"))))
  ;; (%slot-def-raw-options slotd options-plist) → slotd
  ;; Attaches the canonical slot-option plist for DIRECT-SLOT-DEFINITION-CLASS dispatch
  ;; under a custom metaclass; returns the slotd so it wraps %make-slot-def transparently.
  (setf (gethash '%slot-def-raw-options h)
        (lambda (expr)
          `(,@(compile-value-arg (second expr))
            ,@(compile-value-arg (third expr))
            (:call "Runtime.SetSlotDefRawOptions"))))
  (setf (gethash '%register-class h) (lambda (expr) (compile-unary-call (cdr expr) "Runtime.RegisterClass")))
  (setf (gethash '%set-class-default-initargs h)
        (lambda (expr)
          `(,@(compile-value-arg (second expr))
            ,@(compile-value-arg (third expr))
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
          ;; Pre-evaluate obj/slot/value into temps so the stack is empty while
          ;; each is compiled. Pushing them inline would leave obj+slot on the
          ;; stack during the value's evaluation, which is invalid CIL if the
          ;; value contains a try block (e.g. (setf (slot-value o 's)
          ;; (loop ... being each hash-value ...)) or a handler-case value).
          (let* ((da (compile-direct-call-args (cdr expr)))
                 (temps (car da))
                 (eval-instrs (cdr da)))
            `(,@eval-instrs
              ,@(loop for tmp in temps append `((:ldloc ,tmp)))
              (:call "Runtime.SetSlotValue")))))
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
          `(,@(compile-value-arg (second expr))
            ,@(compile-value-arg (third expr))
            ,@(compile-value-arg (fourth expr))
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
  ;; defmethod-body capture intrinsics: return THIS invocation's cnm/nmp closures
  ;; from thread-local storage. Used in place of (symbol-function 'call-next-method).
  (setf (gethash '%captured-call-next-method h)
        (lambda (expr) (declare (ignore expr)) '((:call "Runtime.CapturedCnm"))))
  (setf (gethash '%captured-next-method-p h)
        (lambda (expr) (declare (ignore expr)) '((:call "Runtime.CapturedNmp"))))
  (setf (gethash '%make-instance-with-initargs h)
        (lambda (expr)
          ;; Evaluate the class into a temp FIRST so the stack is empty while
          ;; compile-args-array pre-evaluates the initarg values. An initarg value
          ;; containing a tagbody (loop/dolist — which emit :LEAVE and labels that
          ;; require an empty CIL stack) was otherwise compiled with the class still
          ;; on the stack, producing an unbalanced (stack-underflow) method — invalid
          ;; CIL, e.g. cl-ppcre's (make-instance 'alternation :choices (loop … collect
          ;; …)). A plain call (list/mapcar) never hit this because it loads nothing
          ;; before its args.
          (let ((class-tmp (gen-local "MI-CLASS"))
                (args-tmp (gen-local "MI-ARGS")))
            `((:declare-local ,class-tmp "LispObject")
              ,@(compile-value-arg (second expr))
              (:stloc ,class-tmp)
              (:declare-local ,args-tmp "LispObject[]")
              ,@(compile-args-array (cddr expr))
              (:stloc ,args-tmp)
              (:ldloc ,class-tmp)
              (:ldloc ,args-tmp)
              (:call "Runtime.MakeInstanceWithInitargs")))))

  ;; Derived predicates
  (setf (gethash 'zerop h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              `(,@(compile-value-arg (cadr expr)) ,@(emit-fixnum 0) (:call "Runtime.NumEqual"))
              (compile-named-call 'zerop (cdr expr)))))
  (setf (gethash 'plusp h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              `(,@(compile-value-arg (cadr expr)) ,@(emit-fixnum 0) (:call "Runtime.GreaterThan"))
              (compile-named-call 'plusp (cdr expr)))))
  (setf (gethash 'minusp h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              `(,@(compile-value-arg (cadr expr)) ,@(emit-fixnum 0) (:call "Runtime.LessThan"))
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
                  (1 (let ((*in-tail-position* nil) (*in-mv-context* nil)) (compile-expr (first args))))
                  (2 (if (and (fixnum-typed-p (first args))
                              (fixnum-typed-p (second args)))
                         (compile-fixbit-binop args cil-op)
                         (compile-binary-call args method2)))
                  (t `(,@(compile-args-array args) (:call ,methodN)))))))))

  ;; I/O
  (setf (gethash 'print h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond ((= nargs 1) `(,@(compile-value-arg (cadr expr)) (:call "Runtime.Print")))
                  ((= nargs 2) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) (:call "Runtime.Print2")))
                  (t (compile-static-program-error (format nil "PRINT: wrong number of arguments: ~a (expected 1-2)" nargs)))))))
  (setf (gethash 'prin1 h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond ((= nargs 1) `(,@(compile-value-arg (cadr expr)) (:call "Runtime.Prin1")))
                  ((= nargs 2) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) (:call "Runtime.Prin12")))
                  (t (compile-static-program-error (format nil "PRIN1: wrong number of arguments: ~a (expected 1-2)" nargs)))))))
  (setf (gethash 'princ h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond ((= nargs 1) `(,@(compile-value-arg (cadr expr)) (:call "Runtime.Princ")))
                  ((= nargs 2) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) (:call "Runtime.Princ2")))
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
              ,@(compile-value-arg (cadr expr)) (:stloc ,stream-tmp)
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
                  ,@(compile-value-arg (cadr expr)) (:stloc ,path-tmp)
                  ,@(compile-args-array (cddr expr)) (:stloc ,args-tmp)
                  (:ldloc ,path-tmp) (:ldloc ,args-tmp)
                  (:call "Runtime.OpenFile"))))))
  ;; open-stream-p goes through the named call (symbol-function), not a direct
  ;; Runtime.OpenStreamP inline, so a user-defined method — e.g. flexi-streams,
  ;; whose open-stream-p delegates to the underlying stream — is dispatched
  ;; instead of the builtin gray-stream default (which always returns T). When no
  ;; user method exists the symbol-function is still the builtin, so the default
  ;; behavior is unchanged.
  (setf (gethash 'open-stream-p h) (lambda (expr) (compile-named-call 'open-stream-p (cdr expr))))
  (setf (gethash 'read-line h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((= nargs 4) (compile-named-call 'read-line (cdr expr)))
              ((= nargs 0) `(,@(compile-value-arg '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadLine")))
              ((= nargs 1) `(,@(compile-value-arg (cadr expr)) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadLine")))
              ((= nargs 2) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(emit-nil) (:call "Runtime.ReadLine")))
              ((= nargs 3) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(compile-value-arg (cadddr expr)) (:call "Runtime.ReadLine")))
              (t (compile-static-program-error (format nil "READ-LINE: wrong number of arguments: ~a (expected 0-4)" nargs)))))))
  (setf (gethash 'read-char h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((= nargs 4) (compile-named-call 'read-char (cdr expr)))
              ((= nargs 0) `(,@(compile-value-arg '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadChar")))
              ((= nargs 1) `(,@(compile-value-arg (cadr expr)) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadChar")))
              ((= nargs 2) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(emit-nil) (:call "Runtime.ReadChar")))
              ((= nargs 3) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(compile-value-arg (cadddr expr)) (:call "Runtime.ReadChar")))
              (t (compile-static-program-error (format nil "READ-CHAR: wrong number of arguments: ~a (expected 0-4)" nargs)))))))
  (setf (gethash 'read-char-no-hang h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((= nargs 4) (compile-named-call 'read-char-no-hang (cdr expr)))
              ((= nargs 0) `(,@(compile-value-arg '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadCharNoHang")))
              ((= nargs 1) `(,@(compile-value-arg (cadr expr)) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadCharNoHang")))
              ((= nargs 2) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(emit-nil) (:call "Runtime.ReadCharNoHang")))
              ((= nargs 3) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(compile-value-arg (cadddr expr)) (:call "Runtime.ReadCharNoHang")))
              (t (compile-static-program-error (format nil "READ-CHAR-NO-HANG: wrong number of arguments: ~a (expected 0-4)" nargs)))))))
  (setf (gethash 'listen h)
        (lambda (expr)
          (if (null (cdr expr))
              `(,@(compile-value-arg '*standard-input*) (:call "Runtime.Listen"))
              (compile-unary-call (cdr expr) "Runtime.Listen"))))
  (setf (gethash 'clear-input h)
        (lambda (expr)
          (if (null (cdr expr))
              `(,@(compile-value-arg '*standard-input*) (:call "Runtime.ClearInput"))
              (compile-unary-call (cdr expr) "Runtime.ClearInput"))))
  (setf (gethash 'write-byte h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.WriteByte")))
  (setf (gethash 'peek-char h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((= nargs 5) (compile-named-call 'peek-char (cdr expr)))
              ((= nargs 0) `(,@(emit-nil) ,@(compile-value-arg '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.PeekChar")))
              ((= nargs 1) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.PeekChar")))
              ((= nargs 2) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(emit-t) ,@(emit-nil) (:call "Runtime.PeekChar")))
              ((= nargs 3) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(compile-value-arg (cadddr expr)) ,@(emit-nil) (:call "Runtime.PeekChar")))
              ((= nargs 4) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(compile-value-arg (cadddr expr)) ,@(compile-value-arg (car (cddddr expr))) (:call "Runtime.PeekChar")))
              (t (compile-static-program-error (format nil "PEEK-CHAR: wrong number of arguments: ~a (expected 0-5)" nargs)))))))
  (setf (gethash 'unread-char h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg '*standard-input*) (:call "Runtime.UnreadChar"))
              (compile-binary-call (cdr expr) "Runtime.UnreadChar"))))
  (setf (gethash 'write-char h)
        (lambda (expr)
          (if (= (length (cdr expr)) 1)
              `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg '*standard-output*) (:call "Runtime.WriteChar"))
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
              ;; With recursive-p: fall through to generic dispatch (registered LispFunction handles it)
              ((= nargs 4) (compile-named-call 'read (cdr expr)))
              ((= nargs 0) `(,@(compile-value-arg '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadFromStream")))
              ((= nargs 1) `(,@(compile-value-arg (cadr expr)) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadFromStream")))
              ((= nargs 3) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(compile-value-arg (cadddr expr)) (:call "Runtime.ReadFromStream")))
              (t `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(emit-nil) (:call "Runtime.ReadFromStream")))))))
  (setf (gethash 'read-from-string h) (lambda (expr) `(,@(compile-args-array (cdr expr)) (:call "Runtime.ReadFromString"))))
  (setf (gethash 'read-preserving-whitespace h)
        (lambda (expr)
          (let ((nargs (length (cdr expr))))
            (cond
              ((> nargs 4) (compile-static-program-error (format nil "READ-PRESERVING-WHITESPACE: too many arguments: ~D (expected at most 4)" nargs)))
              ;; With recursive-p: fall through to generic dispatch
              ((= nargs 4) (compile-named-call 'read-preserving-whitespace (cdr expr)))
              ((= nargs 0) `(,@(compile-value-arg '*standard-input*) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadPreservingWhitespace")))
              ((= nargs 1) `(,@(compile-value-arg (cadr expr)) ,@(emit-t) ,@(emit-nil) (:call "Runtime.ReadPreservingWhitespace")))
              ((= nargs 3) `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(compile-value-arg (cadddr expr)) (:call "Runtime.ReadPreservingWhitespace")))
              (t `(,@(compile-value-arg (cadr expr)) ,@(compile-value-arg (caddr expr)) ,@(emit-nil) (:call "Runtime.ReadPreservingWhitespace")))))))

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
              `(,@(compile-value-arg (cadr expr)) ,@(compile-quoted 'stream) (:call "Runtime.Typep"))
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
              ((= nargs 1) `(,@(compile-value-arg (car args)) (:ldc-i4 10) (:call "Fixnum.Make") (:call "Runtime.DigitCharP")))
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
                ;; Multi-list MAPCAR. Evaluate the function into a temp FIRST so the
                ;; stack is empty before compile-args-array compiles the list args.
                ;; Leaving the function value on the stack while a list arg expands
                ;; to code that opens a try-block (e.g. a LOOP ... COLLECT) violated
                ;; CIL's "stack must be empty at try-block entry" rule and produced
                ;; invalid IL ("CLR detected an invalid program").
                (let ((fn-tmp (gen-local "MAPFN"))
                      (arr-tmp (gen-local "MAPARR")))
                  `((:declare-local ,fn-tmp "LispObject")
                    ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
                        (compile-expr (car args)))
                    (:stloc ,fn-tmp)
                    ;; Build the list-args array with an empty operand stack
                    ;; (compile-args-array already pre-evaluates each arg to a temp),
                    ;; then stash it so neither the function value nor a partial array
                    ;; sits on the stack while a LOOP arg opens its try-block.
                    (:declare-local ,arr-tmp "LispObject[]")
                    ,@(compile-args-array (cdr args))
                    (:stloc ,arr-tmp)
                    (:ldloc ,fn-tmp)
                    (:ldloc ,arr-tmp)
                    (:call "Runtime.MapcarN")))
                (compile-binary-call args "Runtime.Mapcar")))))

  ;; Property list
  (setf (gethash 'get h)
        (lambda (expr)
          (let* ((args (cdr expr)) (nargs (length args)))
            (cond
              ((< nargs 2) (compile-static-program-error (format nil "GET: too few arguments: ~D (expected at least 2)" nargs)))
              ((> nargs 3) (compile-static-program-error (format nil "GET: too many arguments: ~D (expected at most 3)" nargs)))
              (t `(,@(compile-value-arg (first args))
                   ,@(compile-value-arg (second args))
                   ,@(if (third args) (compile-value-arg (third args)) (emit-nil))
                   (:call "Runtime.GetProp")))))))
  (setf (gethash 'put-prop h)
        (lambda (expr)
          `(,@(compile-value-arg (cadr expr))
            ,@(compile-value-arg (caddr expr))
            ,@(compile-value-arg (cadddr expr))
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
                     `(,@(compile-value-arg test-expr) (:call "Runtime.MakeHashTable")))))
              (t '((:call "Runtime.MakeHashTable0")))))))
  (setf (gethash 'gethash h) (lambda (expr) (compile-gethash (cdr expr))))
  (setf (gethash 'puthash h) (lambda (expr) (compile-puthash (cdr expr))))
  (setf (gethash 'remhash h) (lambda (expr) (compile-binary-call (cdr expr) "Runtime.Remhash")))

  ;; Values
  (setf (gethash 'values h) (lambda (expr) (compile-values-call (cdr expr))))
  (setf (gethash 'multiple-value-list h)
        (lambda (expr)
          `((:call "MultipleValues.Reset")
            ;; Keep MV context (multiple-value-list needs its arg's values) but
            ;; block tail: a self-tail-call arg would otherwise fire TCO and
            ;; dead-code the MultipleValuesList1 call.
            ,@(let ((*in-tail-position* nil) (*in-mv-context* t))
                (compile-expr (cadr expr)))
            (:call "Runtime.MultipleValuesList1"))))

  ;; Shorthand
  (setf (gethash '1+ h)
        (lambda (expr)
          (cond
            ((and (= (length (cdr expr)) 1)
                  (fixnum-typed-p (cadr expr))
                  (fixnum-arith-unboxed-safe-p expr))
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
                  (fixnum-typed-p (cadr expr))
                  (fixnum-arith-unboxed-safe-p expr))
             `(,@(compile-as-long (cadr expr))
               (:ldc-i8 1) (:sub)
               (:call "Fixnum.Make")))
            ((= (length (cdr expr)) 1)
             (compile-unary-call (cdr expr) "Runtime.Decrement" "1-"))
            (t (compile-named-call '1- (cdr expr))))))
  ;; %dotimes-1+ — increment emitted by the dotimes expansion for a
  ;; fixnum-declared counter. The macro asserts (from loop structure: the
  ;; increment site is only reached while counter < limit <= int64-max) that
  ;; the result fits int64, so a fixnum-typed counter takes the raw add path
  ;; without needing a +1-widened range proof. A counter that is NOT
  ;; fixnum-typed here (e.g. captured by a closure, hence boxed) falls back to
  ;; the generic promoting increment.
  (setf (gethash '%dotimes-1+ h)
        (lambda (expr)
          (if (and (= (length (cdr expr)) 1)
                   (fixnum-typed-p (cadr expr)))
              `(,@(compile-as-long (cadr expr))
                (:ldc-i8 1) (:add)
                (:call "Fixnum.Make"))
              (compile-unary-call (cdr expr) "Runtime.Increment" "1+"))))

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
          ;; recurse on the same defun.
          (when (and *compile-was-toplevel* *compile-file-mode*
                     (not *cross-compiling*))
            (try-eval expr))
          ;; CLHS 3.4.11: extract docstring (first form if string AND more forms follow).
          ;; Skip during cross-compile because cil-stdlib's own defuns may precede
          ;; (setf documentation) GF definition; runtime user defuns are fine.
          (let* ((name (cadr expr))
                 (params (caddr expr))
                 (body (cdddr expr))
                 ;; Only attach docstring for plain symbol names; (setf foo) /
                 ;; other function-name forms are skipped because compile-sym-lookup
                 ;; expects a symbol (would error: SYMBOL-PACKAGE on cons).
                 (has-docstring (and (not *cross-compiling*)
                                     (symbolp name)
                                     (consp body) (stringp (car body)) (cdr body)))
                 (docstring (when has-docstring (car body)))
                 (real-body (if has-docstring (cdr body) body))
                 (defun-instrs (compile-defun name params real-body)))
            (if has-docstring
                ;; defun-instrs ends with sym on stack. Set documentation
                ;; via (funcall #'(setf documentation) docstring 'name 'function),
                ;; then leave sym on stack as the form's value.
                `(,@defun-instrs
                  (:pop)
                  ,@(compile-and-pop
                      `(funcall #'(setf documentation) ,docstring ',name 'function))
                  ,@(compile-sym-lookup name))
                defun-instrs))))
  ;; %inline-cs-spliced: dotcl-cs:inline-cs macro expansion target.
  ;; Form: (%inline-cs-spliced ((arg1 arg2 ...)) :returns long ((:LDARG-0) ...))
  ;;   ARGN are Lisp value forms (compiled to LispObject Fixnum).
  ;;   The instr-list comes from (dotcl-cs:disassemble-cs ...) at macro
  ;;   expansion time — opcodes like (:LDARG-0) refer to the corresponding
  ;;   ARGN. We unbox each arg to int64, store in a fresh local, translate
  ;;   LDARG-N to ldloc that local, drop the trailing :RET, and box the
  ;;   final stack-top via Fixnum.Make. Only fixnum bindings + fixnum
  ;;   return are supported for the MVP. Other types throw.
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
          (let ((ltv-id (incf *ltv-counter*))
                (mod-id *current-module-id*))
            (if mod-id
                ;; Per-module namespaced LTV — prevents cross-run slot ID collisions
                (compile-expr `(if (%has-ltv-slot-in ,mod-id ,ltv-id)
                                   (%get-ltv-slot-in ,mod-id ,ltv-id)
                                   (%set-ltv-slot-in ,mod-id ,ltv-id ,(cadr expr))))
                ;; Legacy path (eval, compile without compile-file context)
                (compile-expr `(if (%has-ltv-slot ,ltv-id)
                                   (%get-ltv-slot ,ltv-id)
                                   (%set-ltv-slot ,ltv-id ,(cadr expr))))))))
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
