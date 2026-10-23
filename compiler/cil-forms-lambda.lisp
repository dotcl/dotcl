;;; cil-forms-lambda.lisp: setq, lambda, compile-env, funcall, FUNCTION
;;; Part of the CIL compiler (A2 instruction list architecture)

(in-package :dotcl.cil-compiler)

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
    ((and (consp expr) (eq (car expr) 'load-time-value)) t)
    ((and (consp expr) (eq (car expr) 'the))
     (single-value-form-p (caddr expr)))
    ;; Non-local exits (return-from, throw, go) transfer control and
    ;; leave the stack unchanged from the compiler's perspective: adding
    ;; an UnwrapMv after would try to pop a non-existent value.
    ((and (consp expr)
          (member (car expr) '(return-from return throw go)))
     t)
    ;; Known single-value operators (direct calls, never produce MV)
    ((and (consp expr) (symbolp (car expr))
          (not (local-function-entry (car expr)))
          (member (car expr)
                  '(car cdr cons list list* first rest second third
                    cadr cddr caar cdar caddr
                    + - * / 1+ 1- mod rem
                    < > <= >= = /=
                    eq eql equal equalp
                    not null endp atom consp listp numberp integerp symbolp
                    stringp characterp functionp
                    length last nconc nreverse reverse
                    rplaca rplacd nth nthcdr
                    char-code code-char char
                    ash logand logior logxor lognot
                    typep type-of
                    get
                    aref vector-push-extend
                    ;; Element readers: each answers the one object stored at
                    ;; the index (CLHS gives each a single ELEMENT value), and
                    ;; every store into an element is a single-value position.
                    svref schar sbit bit row-major-aref elt
                    setq %absent-key %key-value
                    ;; The step of a counting DO / LOOP variable: a macro
                    ;; that expands to 1+, 1- or a two-argument +.
                    %induction-next)))
     t)
    ;; Functions with a declaimed ftype return type are single-value: atomic
    ;; types like fixnum / double-float / etc. cannot appear as multiple values.
    ;; Honors user `(declaim (ftype (function (...) fixnum) NAME))` so calls to
    ;; such NAME elide Runtime.UnwrapMv.
    ((and (consp expr) (symbolp (car expr))
          (not (local-function-entry (car expr)))
          (let ((ret (function-return-type (car expr))))
            (and ret
                 (or (member ret '(fixnum bit double-float single-float
                                          short-float long-float
                                          integer float real rational number
                                          character boolean))
                     (declared-integer-range ret)))))
     t)
    (t nil)))

(defun %induction-overflow-step (var key val-expr up)
  "The store of (%INDUCTION-NEXT VAR DELTA EXIT) into VAR's Int64 slot KEY when
   the sum may leave int64 in the direction the loop ends: the wrapped sum is
   behind VAR exactly when it did, and then EXIT runs instead of the store."
  (when (and (consp val-expr) (symbolp (car val-expr))
             (string= (symbol-name (car val-expr)) "%INDUCTION-NEXT")
             (eq (cadr val-expr) var)
             (consp (cdddr val-expr)))
    (let* ((delta (caddr val-expr))
           (exit (cadddr val-expr))
           (v (if (consp delta) (cadr delta) delta))
           (sum (gen-local "STEP"))
           (ok (gensym "STEPOK")))
      (when (or (integerp v) (fixnum-typed-p v))
        `((:declare-local ,sum "Int64")
          (:ldloc ,key)
          ,@(compile-expr-to-long v)
          (,(if (consp delta) :sub :add))
          (:stloc ,sum)
          (:ldloc ,sum)
          (:ldloc ,key)
          (,(if up :clt :cgt))
          (:brfalse ,ok)
          ,@(let ((*in-tail-position* nil) (*in-mv-context* nil))
              (compile-expr exit))
          (:pop)
          (:label ,ok)
          (:ldloc ,sum)
          (:dup)
          (:stloc ,key)
          ,@(%frame-set-instrs key var :long)
          (:call "Fixnum.Make"))))))

(defun compile-setq (var val-expr)
  "Compile (setq var value). Returns the assigned value on stack (single value)."
  ;; If var is a symbol-macro, delegate to SETF of the expansion (branch on found-p,
  ;; not the expansion's truth value, so a NIL-expanding symbol-macro still delegates )
  (multiple-value-bind (sm-exp found) (lookup-symbol-macro var)
    (when found
      (return-from compile-setq (compile-expr `(setf ,sm-exp ,val-expr)))))
  ;; An inline call that returns its value with VALUES, stored in a raw integer
  ;; slot: the store moves into each VALUES tail, so the value is not boxed
  ;; into the multiple-value record first.
  (when (and (boundp '*cstate*)
             (or (native-slot-p var (cstate-long-locals)) (u64-native-local-p var)))
    (let ((sunk (%mv-setq-sink-inline (list var) val-expr)))
      (when sunk
        (return-from compile-setq (compile-expr sunk)))))
  ;; Int64-slot local (long-rep let binding or native body param): store the
  ;; raw long, then box once for the expression value. In statement position
  ;; the peephole pass deletes the box+pop, leaving a pure native store.
  ;; Checked before the generic path because the slot type differs.
  (let ((key (and (boundp '*cstate*)
                  (native-slot-p var (cstate-long-locals)))))
    (let ((exit (and key (assoc key *induction-overflow-exits* :test #'equal))))
      (let ((step (and exit (%induction-overflow-step var key val-expr (cdr exit)))))
        (when step (return-from compile-setq step))))
    (when key
      (return-from compile-setq
        `(,@(compile-expr-to-long val-expr)
          (:dup)
          (:stloc ,key)
          ,@(%frame-set-instrs key var :long)
          (:call "Fixnum.Make")))))
  ;; Native float slot (double/single-rep local): store the raw r8/r4, then box
  ;; once for the expression value. The peephole (P6/P7) deletes the box+pop in
  ;; statement position, leaving a pure native store: this is what removes the
  ;; per-setq DoubleFloat/SingleFloat box from numeric accumulator loops.
  (let ((key (lookup-local var))
        (nk (float-native-local-kind var)))
    (when (and key nk)
      (return-from compile-setq
        `(,@(compile-float-native-value val-expr nk)
          (:dup)
          (:stloc ,key)
          ,@(%frame-set-instrs key var nk)
          (:newobj ,(ecase nk (:double "DoubleFloat") (:single "SingleFloat")))))))
  ;; Native decimal slot: same shape as the float slots: store raw, box once
  ;; for the expression value (the peephole drops the box in statement position).
  (let ((key (lookup-local var)))
    (when (and key (decimal-native-local-p var))
      (return-from compile-setq
        `(,@(compile-decimal-native-value val-expr)
          (:dup)
          (:stloc ,key)
          ,@(%frame-set-instrs key var :decimal)
          (:newobj "LispDecimal")))))
  ;; (unsigned-byte 64) slot: store the bits of the value (a value that is not
  ;; an (unsigned-byte 64) is a TYPE-ERROR here), box once for the expression
  ;; value (the peephole drops the box in statement position).
  (let ((key (u64-native-local-p var)))
    (when key
      (return-from compile-setq
        `(,@(compile-as-u64-bits val-expr)
          (:dup)
          (:stloc ,key)
          (:call "Runtime.BoxU64")))))
  (let ((e (simd-native-local var)))
    (when e
      (return-from compile-setq
        `(,@(compile-simd-local-value val-expr (cdr e))
          (:dup)
          (:stloc ,(car e))
          ,(%simd-call "Box" (cdr e))))))
  ;; Native character slot: store the raw code, rebuild the character once for
  ;; the expression value (the peephole drops it in statement position, which is
  ;; where a scanner's (SETQ C (SCHAR S I)) sits).
  (let ((key (lookup-local var)))
    (when (and key (char-native-local-p var))
      (return-from compile-setq
        `(,@(compile-char-native-value val-expr)
          (:dup)
          (:stloc ,key)
          ,@(%frame-set-instrs key var :char)
          (:conv-i4)
          (:call "LispChar.Make")))))
  ;; Variable assignment binds a single value. Force *in-mv-context* nil
  ;; and *in-tail-position* nil when compiling val-expr so MvReturn is
  ;; unwrapped before storage (mirrors compile-let).
  (let ((val-instrs (let ((*in-tail-position* nil)
                          (*in-mv-context* nil))
                      (compile-expr val-expr))))
    ;; Check lexical binding BEFORE special: mirrors compile-var-ref ordering
    (let ((key (lookup-local var)))
      (if key
          (if (boxed-var-p var)
              ;; Boxed: box[0] = value, return value
              (let ((tmp (gen-local "SETQ")))
                `((:declare-local ,tmp "LispObject")
                  ,@val-instrs
                  (:stloc ,tmp)
                  ,@(if (and *emit-source-lines* (not (labels-cell-var-p var)))
                        `((:ldloc ,key) (:ldloc ,tmp) (:stfld "LispBox.Value"))
                        `((:ldloc ,key) (:ldc-i4 0) (:ldloc ,tmp) (:stelem-ref)))
                  (:ldloc ,tmp)))
              ;; Simple local: store and return value
              `(,@val-instrs
                (:dup)
                (:stloc ,key)
                ,@(%frame-set-instrs key var)))
          ;; No lexical binding: use special/dynamic assignment.
          ;; SetChecked, not Set: assigning a constant variable is an error
          ;; (CLHS 3.1.2.1.1.3), and DEFCONSTANT is the only thing that may write
          ;; one. The check is at run time because the symbol may not be a
          ;; constant yet when this form is compiled.
          (let ((tmp (gen-local "SETQSPL")))
            `((:declare-local ,tmp "LispObject")
              ,@val-instrs
              (:stloc ,tmp)
              ,@(compile-sym-lookup var)
              (:castclass "Symbol")
              (:ldloc ,tmp)
              (:call "DynamicBindings.SetChecked")))))))

;;; ============================================================
;;; lambda / closure
;;; ============================================================

(defun find-free-vars-with-defaults (params body)
  "The free variables of BODY and of the &optional/&key default forms, as the
   SYMBOLS that were referenced; not their names. FREE-HT is keyed by VAR-NAME
   (so same-named references collapse to one capture) but holds the symbol, and
   returning that keeps capture on symbol identity all the way to the closure
   body's *LOCALS*. Names are re-derived (VAR-NAME) only where a string is
   actually required: the emitted :env-map, and the string-keyed mutation/boxing
   analyses."
  (multiple-value-bind (required optional key rest-param aux) (parse-lambda-list params)
    (let* ((all-params (append required
                               (mapcar #'car optional)
                               (remove nil (mapcar #'third optional))
                               (mapcar #'second key)
                               (remove nil (mapcar #'fourth key))
                               ;; The &rest var is bound by the lambda list like
                               ;; every other parameter. Leaving it out made a body
                               ;; reference to it look free, which pushed DEFUN off
                               ;; the :defmethod path onto runtime registration and
                               ;; made a bare (lambda (&rest r) r) build an env array
                               ;; capturing an outer R that the parameter shadows.
                               (if rest-param (list rest-param) nil)
                               ;; &aux vars are bound in the function body scope,
                               ;; so body references to them are not free (D-fix).
                               (mapcar (lambda (a) (if (consp a) (car a) a)) aux)))
           (bound-names (mapcar #'var-name all-params))
           (free-ht (list nil)))
      ;; Scan body
      (dolist (form body)
        (find-free-vars-expr form bound-names free-ht))
      ;; Scan default forms in &optional/&key: pass nil for bound
      ;; since scan-lambda-list-defaults handles progressive scoping
      ;; (each default only sees params to its left, not all params)
      (scan-lambda-list-defaults params nil free-ht)
      (%ffv-collected free-ht))))

(defun lambda-list-shape-tag (params)
  "Digit-free lambda-list shape tag for the make-function DM name (diagnostic
   only; InvokeSlow statistics can then break \"<anon:lambda>\" down by
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

(defun %lambda-key-direct-specs (params body)
  "The typed entries of an anonymous &key lambda that captures nothing, as a
   DEFUN of the same lambda list gets them, or NIL."
  (multiple-value-bind (required optional key rest-param aux allow-other-keys-p)
      (parse-lambda-list params)
    (when (and (%key-direct-eligible-p required optional key rest-param aux)
               (not (%declares-special-p body (mapcar #'second key))))
      (let ((checked (insert-simple-decl-checks
                      body (append required (mapcar #'second key)))))
        (%build-key-direct-specs required key checked "" nil nil allow-other-keys-p
                                 (every (lambda (k) (null (fifth k))) key)
                                 checked)))))

(defun compile-lambda (params body &optional (fn-name "") (fn-label ""))
  "Compile (lambda (params) body...). FN-NAME enables self-TCO when non-empty.
   FN-LABEL, when non-empty, is wired to the :name plist of
   :make-function/-direct so the runtime LispFunction carries the name
   (backtrace / statistics / print) WITHOUT enabling TCO; used by the
   runtime-registration defun paths, whose defuns were previously anonymous
   (e.g. &rest stdlib defuns in chunked progn compiles)."
  (multiple-value-bind (required optional key rest-param) (parse-lambda-list params)
    (declare (ignore optional key))
    (let* ((free-vars (remove-if #'global-special-p
                                 (find-free-vars-with-defaults params body)))
           ;; Speculative labels-self-TCO: when this named, required-only fn's
           ;; ONLY free var is its own labels box, compile it through the
           ;; direct+TCO path instead of a closure. compile-labels-boxed scans the
           ;; result and falls back to the closure path unless it is provably
           ;; self-contained (no box-key reference, no orphan post-TCO code), so a
           ;; wrong guess only forgoes the optimization: it never miscompiles.
           (spec (cstate-labels-direct-speculation))
           (speculate-direct
             (and spec
                  (string= fn-name (car spec))
                  (simple-required-only-p params)
                  free-vars (null (cdr free-vars))
                  (string= (var-name (first free-vars))
                           (concatenate 'string "__LABELFN_" (car spec))))))
      (if (or (null free-vars) speculate-direct)
          ;; No captures (or speculating self-TCO): :make-function or :make-function-direct
          (if (simple-required-only-p params)
              `((:make-function-direct
                 :param-count ,(length required)
                 ;; Development information (DOTCL:FUNCTION-LAMBDA-LIST), as
                 ;; for a DEFUN's :defmethod.
                 :lambda-list ,(sil-portable-lambda-list params)
                 ,@(when (string/= fn-label "") `(:name ,fn-label))
                 ,@(when (debug-frames-off-p body) '(:no-frame t))
                 :body ,(compile-function-body-direct
                         params (%direct-body-with-aux params body) fn-name)))
              (let* ((label (if (string/= fn-label "") fn-label fn-name))
                     (direct-specs (and (string= label "")
                                        (%lambda-key-direct-specs params body))))
                `((:make-function
                   :param-count ,(length required)
                   :ll-shape ,(lambda-list-shape-tag params)
                   :lambda-list ,(sil-portable-lambda-list params)
                   ,@(when (string/= fn-label "") `(:name ,fn-label))
                   ,@(when (debug-frames-off-p body) '(:no-frame t))
                   ,@(when direct-specs `(:direct-delegates ,direct-specs))
                   ;; FN-LABEL, not FN-NAME: this is the name the arity error and
                   ;; the debug frame report, and the runtime-registration DEFUN
                   ;; paths carry the name there (FN-NAME stays "" so self-TCO,
                   ;; which only the direct path implements, is not enabled).
                   :body ,(if (eq (car (first direct-specs)) :shared)
                              (multiple-value-bind (required optional key rest-param aux
                                                    allow-other-keys-p)
                                  (parse-lambda-list params)
                                (declare (ignore optional rest-param aux))
                                (%compile-key-shared-xep required key allow-other-keys-p label))
                              (compile-function-body params body label))))))
          ;; Closure: build env array, then :make-closure
          (compile-closure params body free-vars fn-name)))))

(defvar *closure-inline-only* '()
  "The captured local functions of the closure being compiled that are not put
   in its environment, because every call of them in its body is to be
   substituted. Their entries there get keys no slot has (collected in
   *CLOSURE-INLINE-ONLY-KEYS*).")

(defvar *closure-inline-only-keys* nil
  "A list cell whose car collects the keys given to *CLOSURE-INLINE-ONLY*.")

(defun %inline-only-captures (free-vars)
  "The FREE-VARS that hold a local function whose inline plan holds here."
  (loop for e in (cstate-local-functions)
        for name = (first e)
        for fv = (find (concatenate 'string "__LABELFN_" name) free-vars
                       :key #'var-name :test #'string=)
        when (and fv (nth 5 e) (not (fourth e))
                  (%local-inline-env-matches-p (nth 5 (nth 5 e))))
          collect fv))

(defun %with-inline-only-fn-locals (cstate)
  "CSTATE with a local for each local function left out of this closure's
   environment (*CLOSURE-INLINE-ONLY*), so that a closure nested in the body
   still finds the function free: it captures the absent slot, and the
   enclosing COMPILE-CLOSURE sees that and captures the function after all."
  (let ((keys (and *closure-inline-only-keys* (car *closure-inline-only-keys*)))
        (added '()))
    (dolist (e (cstate-local-functions))
      (when (member (second e) keys :test #'eq)
        (push (cons (%labelfn-local-sym (first e)) (second e)) added)))
    (if added
        (cstate-with cstate +cs-locals+ (append (nreverse added) (cstate-locals)))
        cstate)))

(defun %tree-mentions-p (tree items)
  (let ((seen (make-hash-table :test 'eq)))
    (labels ((walk (x)
               (cond ((member x items :test #'eq) t)
                     ((not (consp x)) nil)
                     ((gethash x seen) nil)
                     (t (setf (gethash x seen) t)
                        (or (walk (car x)) (walk (cdr x)))))))
      (walk tree))))

(defun compile-closure (params body free-vars &optional (fn-name ""))
  "COMPILE-CLOSURE-1, first without capturing the local functions every call of
   which is substituted in BODY (%INLINE-ONLY-CAPTURES). When the result still
   refers to one of them (a call that was not substituted, #'F), BODY is
   compiled again capturing all of FREE-VARS."
  (let ((inline-only (%inline-only-captures free-vars)))
    (or (and inline-only
             (let* ((keys (list nil))
                    (code (let ((*closure-inline-only* inline-only)
                                (*closure-inline-only-keys* keys))
                            (compile-closure-1 params body
                                               (remove-if (lambda (fv) (member fv inline-only))
                                                          free-vars)
                                               fn-name))))
               (and (not (%tree-mentions-p code (car keys))) code)))
        (let ((*closure-inline-only* '()))
          (compile-closure-1 params body free-vars fn-name)))))

(defun compile-closure-1 (params body free-vars &optional (fn-name ""))
  "Compile a closure lambda with captured variables.
   FN-NAME is the name the caller compiles this lambda under (\"\" for an
   anonymous one). It is not emitted; it is what identifies a labels function
   that captures something, so its tail self-call can still be a loop."
  (multiple-value-bind (required optional key rest-param) (parse-lambda-list params)
    (declare (ignore optional key))
  (let* ((n-free (length free-vars))
         ;; Direct-params eligibility: required-only lambda list (no &optional/
         ;; &key/&rest/&aux: simple-required-only-p) with <= 6 params. Such a
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
                            (let ((entry (local-entry fv)))
                              (if entry
                                  (boxed-var-p (car entry))
                                  ;; Only check *local-functions* when not in *locals*
                                  (let ((lf (local-function-entry (var-name fv))))
                                    (and lf (third lf))))))
                          free-vars))
         ;; Under debug, the subset of boxed captures that are DATA variables
         ;; (a boxed lexical in *locals*, not a captured labels function cell)
         ;; use the LispBox representation and so must be loaded/declared as
         ;; LispBox in the body. Labels function cells stay LispObject[1].
         (outer-lispbox-fvs
           (when *emit-source-lines*
             (remove-if-not (lambda (fv)
                              (let ((entry (local-entry fv)))
                                (and entry (boxed-var-p (car entry))
                                     (not (labels-cell-var-p (car entry))))))
                            free-vars)))
         ;; Compile inner body with env slots. The enclosing context: the
         ;; tables the body may still consult plus the two classifications
         ;; above: is snapshotted here, where we are still outside the
         ;; closure-boundary reset.
         ;; Is this lambda the labels function currently being compiled? Read
         ;; here, outside the closure-boundary reset, where the handoff
         ;; compile-labels-boxed made is still in force. An inner lambda of that
         ;; body reaches this point with FN-NAME "" and gets nothing.
         (self-tco-name
           (and (string/= fn-name "")
                (cstate-tco-local-fn-key)
                (symbolp (cstate-tco-self-symbol))
                (cstate-tco-self-symbol)
                (string= fn-name (mangle-name (cstate-tco-self-symbol)))
                (simple-required-only-p params)
                (cstate-tco-self-symbol)))
         (inner-body (compile-closure-body params body free-vars
                                           (capture-compile-env outer-boxed-fvs
                                                                outer-lispbox-fvs
                                                                free-vars)
                                           "" direct-p self-tco-name)))
    `(,@env-build-instrs
      (:make-closure
       :param-count ,(length required)
       :lambda-list ,(sil-portable-lambda-list params)
       ;; fn-name "" matches the name the omitted arity check would have
       ;; carried (compile-closure never passes a name to compile-closure-body)
       ,@(when direct-p '(:direct t :fn-name ""))
       :env-size ,n-free
       ;; The map crosses into the assembler, so slots are named by string here.
       :env-map ,(loop for fv in free-vars
                       for name = (var-name fv)
                       for i from 0
                       collect (let ((entry (local-entry fv)))
                                 (if entry
                                     (if (boxed-var-p (car entry))
                                         ;; Data var: LispBox under debug (so the
                                         ;; body loads env[i] as LispBox), else
                                         ;; LispObject[1]. Labels function cells
                                         ;; stay LispObject[1] even under debug.
                                         (list name i
                                               (if (and *emit-source-lines*
                                                        (not (labels-cell-var-p (car entry))))
                                                   "lispbox" "boxed"))
                                         (list name i "value"))
                                     ;; Only check *local-functions* when not in *locals*
                                     (let ((lf (local-function-entry name)))
                                       (if (and lf (third lf))
                                           ;; Labels function cell: always LispObject[1].
                                           (list name i "boxed")
                                           (list name i "value"))))))
       :body ,inner-body)))))

(defun compile-env-capture (fv idx)
  "Generate instructions to capture free variable FV (the referenced symbol)
   into env[idx]."
  (let ((entry (local-entry fv)))
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
        ;; Not in *locals*: check *local-functions* for boxed labels functions
        (let ((lf-entry (local-function-entry (var-name fv))))
          (if (and lf-entry (third lf-entry))  ;; boxed-p
              `((:dup) (:ldc-i4 ,idx)
                (:ldloc ,(second lf-entry))  ;; the box key
                (:stelem-ref))
              ;; Not found: nil
              `((:dup) (:ldc-i4 ,idx) (:ldsfld "Nil.Instance") (:stelem-ref)))))))

;;; ------------------------------------------------------------
;;; compile-env: the enclosing compile context at a boundary
;;; ------------------------------------------------------------
;;; The per-compilation state variables describe the body being compiled. A
;;; closure body gets a fresh set of them (the closure-boundary reset, see
;;; DEFINE-COMPILE-STATE), but a few pieces of the ENCLOSING body stay relevant
;;; inside it: a captured flet/labels function, a return-from naming an outer
;;; block, a non-local go. Those were saved one variable at a time just before
;;; the reset. COMPILE-ENV names the concept and puts the capture in one place,
;;; so cross-boundary context is added here instead of as another ad-hoc save at
;;; the boundary.
;;;
;;; It is deliberately NOT the whole registry. Reset is an obligation; missing
;;; one variable there is a silent miscompile, which is why that side is
;;; registry-driven. Capture is a permission: state absent here simply cannot be
;;; consulted from inside, and that fails loudly at the call site.
;;;
;;; It lives in this file rather than beside the state variables because the
;;; cross-compile compiles cil-compiler.lisp first, before DEFSTRUCT's support
;;; functions exist.

(defstruct (compile-env
             (:constructor %make-compile-env (local-functions block-tags go-tags
                                              boxed-fvs lispbox-fvs inline-fns
                                              fv-types))
             (:copier nil)
             (:predicate nil))
  local-functions       ; as *local-functions*, from the enclosing body
  block-tags            ; as *block-tags*
  go-tags               ; as *go-tags*
  boxed-fvs             ; captured free vars that are boxed in the enclosing body
  lispbox-fvs           ; under debug, the subset of those held in a LispBox
  inline-fns            ; the local-functions entries whose inline plan holds here
  fv-types)             ; (FV . TYPE) of the captured values whose type is known
;; NB: COMPILE-ENV-CAPTURE (below) is NOT an accessor of this struct; it emits
;; the closure env-array capture for one free var. Do not name a slot CAPTURE.

(defun %captured-value-types (free-vars boxed-fvs)
  "(FV . TYPE) for each captured free variable of FREE-VARS that is captured by
   value (not in BOXED-FVS) and whose value is known to be an integer in a range
   ((:RANGE LO . HI)), a fixnum, a single-float or a double-float."
  (loop for fv in free-vars
        for ty = (and (symbolp fv) fv
                      (not (member fv boxed-fvs :test #'eq))
                      (lookup-local fv)
                      (not (boxed-var-p fv))
                      (let ((r (small-int-local-range fv)))
                        (cond (r (cons :range r))
                              ((fixnum-exact-p fv) '(:fixnum))
                              ((single-float-typed-p fv) '(:single))
                              ((double-float-typed-p fv) '(:double)))))
        when ty collect (cons fv ty)))

(defun capture-compile-env (boxed-fvs lispbox-fvs &optional free-vars)
  "Snapshot the enclosing compile context; the state an inner body may still
   consult after the closure-boundary reset. Call it BEFORE the reset.
   BOXED-FVS and LISPBOX-FVS are the caller's classification of the captured
   free variables; they are facts about the ENCLOSING bindings, so they belong
   to the same snapshot as the tables above."
  (%make-compile-env (cstate-local-functions) (cstate-block-tags) (cstate-go-tags)
                     boxed-fvs lispbox-fvs
                     (loop for e in (cstate-local-functions)
                           when (and (nth 5 e)
                                     (%local-inline-env-matches-p (nth 5 (nth 5 e))))
                             collect e)
                     (%captured-value-types free-vars boxed-fvs)))

(defun %carry-inline-plans (outer-env env-locals)
  "Give each local function this closure captured the inline plan it had
   outside, when every name of its body means here what it meant there: a
   variable is the captured one, the other bindings are there exactly when they
   were. The plan is set in the captured entry itself, which the snapshots of
   the other plans refer to."
  (let ((carried '()))
    (dolist (outer (compile-env-inline-fns outer-env))
      (let ((inner (find-if (lambda (e) (equal (seventh e) (seventh outer)))
                            (cstate-local-functions))))
        (when (and inner (eq (local-function-entry (seventh outer)) inner))
          (push (cons inner (nth 5 outer)) carried))))
    (dolist (c carried)
      (destructuring-bind (name params decls body blockp snapshot count) (cdr c)
        (let ((new-snapshot
                (loop for (sym . outer-bs) in snapshot
                      for inner-bs = (%local-inline-bindings sym)
                      unless (and (if (first outer-bs)
                                      (member (first inner-bs) env-locals :test #'eq)
                                      (null (first inner-bs)))
                                  (eq (null (second outer-bs)) (null (second inner-bs)))
                                  (eq (null (third outer-bs)) (null (third inner-bs)))
                                  (eq (null (fourth outer-bs)) (null (fourth inner-bs)))
                                  (eq (fifth outer-bs) (fifth inner-bs))
                                  (eq (sixth outer-bs) (sixth inner-bs)))
                        do (return :refuse)
                      collect (cons sym inner-bs))))
          (unless (eq new-snapshot :refuse)
            (setf (nth 5 (car c))
                  (list name params decls body blockp new-snapshot count))))))
    carried))

(defun closure-self-tco-key (self-tco-name special-param-syms optional key rest-param)
  "The closure-local key of the LABELS cell SELF-TCO-NAME is stored in, or NIL
   when this closure body gets no self-TCO loop.
   The entry is looked up exactly the way COMPILE-NAMED-CALL looks it up, so the
   loop can only be entered by a call that resolves to this same cell. Required
   parameters only: the loop rewrites the required locals, and an &optional or
   &key left at its previous iteration's value would be wrong. Special
   parameters are excluded for the reason the direct path excludes them -- their
   DynamicBindings.Pop needs try/finally, which a plain Br cannot leave."
  (when (and self-tco-name (null special-param-syms)
             (null optional) (null key) (null rest-param))
    (let ((entry (local-function-entry (mangle-name self-tco-name))))
      ;; THIRD = boxed-p: a labels cell. An flet entry is not one.
      (and entry (third entry) (second entry)))))

(defun closure-self-tco-cstate (self-tco-name self-tco-key tco-loop-label
                                required param-locals)
  "*CSTATE* for a closure body that is a LABELS function's own loop, or the
   current pack unchanged when SELF-TCO-KEY is NIL."
  (if self-tco-key
      (cstate-with *cstate*
        +cs-tco-self-name+ (mangle-name self-tco-name)
        +cs-tco-self-symbol+ self-tco-name
        +cs-tco-local-fn-key+ self-tco-key
        +cs-tco-loop-label+ tco-loop-label
        +cs-tco-checked+ nil
        +cs-tco-param-entries+ (loop for p in required
                                     collect (cons (cdr (assoc p param-locals))
                                                   (boxed-var-p p))))
      *cstate*))

(defun call-with-param-type-locals (params param-locals body thunk
                                    &optional fv-types env-locals)
  "Call THUNK with the type declarations BODY makes on PARAMS in the type
   tables, as a function body has them, and with the outer entries of the same
   names dropped. PARAM-LOCALS maps each parameter to its slot key. FV-TYPES
   (see %CAPTURED-VALUE-TYPES) are the known types of the captured values,
   whose slots ENV-LOCALS maps."
  (flet ((captured (kind)
           (loop for (fv . ty) in fv-types
                 when (eq (car ty) kind) collect (var-name fv))))
  (let* ((names (mapcar #'var-name params))
         (*fixnum-locals* (append (extract-fixnum-locals body)
                                  (drop-shadowed-type-locals
                                   names (append (captured :fixnum) *fixnum-locals*))))
         (*character-locals* (append (extract-character-locals body)
                                     (drop-shadowed-type-locals names *character-locals*)))
         (*double-float-locals* (append (extract-double-float-locals body)
                                        (drop-shadowed-type-locals
                                         names (append (captured :double)
                                                       *double-float-locals*))))
         (*single-float-locals* (append (extract-single-float-locals body)
                                        (drop-shadowed-type-locals
                                         names (append (captured :single)
                                                       *single-float-locals*))))
         (*decimal-locals* (append (extract-decimal-locals body)
                                   (drop-shadowed-type-locals names *decimal-locals*)))
         (*cstate* (cstate-with *cstate*
                     +cs-small-int-locals+
                     (append (rekey-by-param (extract-small-int-locals body)
                                             params param-locals)
                             (loop for (fv . ty) in fv-types
                                   for key = (cdr (assoc fv env-locals :test #'eq))
                                   when (and key (eq (car ty) :range))
                                     collect (cons key (cdr ty)))
                             (cstate-small-int-locals))
                     +cs-numeric-array-locals+
                     (append (extract-numeric-array-locals body)
                             (cstate-numeric-array-locals))
                     +cs-float-range-locals+
                     (append (rekey-by-param (extract-float-range-locals body)
                                             params param-locals)
                             (cstate-float-range-locals)))))
    (funcall thunk))))

(defun closure-mv-mode-key (direct-p special-param-syms)
  "The value-mode local for a closure body, or NIL: only a :direct closure is
   called through a typed entry (the one InvokeNM can pass a mode to), and a
   body that binds special parameters runs inside a try."
  (when (and *mv-mode-elision* direct-p (null special-param-syms))
    (gen-local "MVMODE")))

(defun compile-closure-body (params body free-vars outer-env &optional (fn-name "")
                                                                      direct-p
                                                                      self-tco-name)
  "Compile function body for closure. Free vars access env (arg 0), params from args (arg 1).
   FREE-VARS are the captured SYMBOLS (see FIND-FREE-VARS-WITH-DEFAULTS); each one
   keys its env slot in *LOCALS*, so body references resolve by identity.
   OUTER-ENV is the enclosing context (CAPTURE-COMPILE-ENV), taken by the caller
   before this body's closure-boundary reset: the tables a captured
   flet/labels / return-from / go still resolve against, and which captured free
   variables were boxed (and under debug, held in a LispBox) out there.
   Handles &rest/&optional/&key parameters.
   DIRECT-P: the caller will mark this closure :direct (required-only, <= 6
   params, assembled with per-arity CLR params); omit the arity-check prefix;
   the runtime-side args-array wrapper performs the identical CheckArityExact.
   SELF-TCO-NAME: the symbol of the LABELS function this closure IS, when the
   caller established that (compile-closure). A closure is normally never the
   self-call target of the body it is compiled in, which is why the closure
   boundary clears the TCO handoff; but a labels function that captures a free
   variable is compiled as a closure and IS its own call target, so its tail
   self-call has to become a loop here or the recursion keeps a frame per
   iteration. Only that one name is enabled: inner lambdas get no handoff."
  (multiple-value-bind (required optional key rest-param aux allow-other-keys-p has-key-p) (parse-lambda-list params)
    (let ((body (wrap-aux-body
                 aux
                 (insert-simple-decl-checks
                  body (append required (mapcar #'car optional) (mapcar #'second key)
                               (if rest-param (list rest-param) nil))))))
    (let* ((all-params (append required
                               (mapcar #'car optional)
                               (remove nil (mapcar #'third optional))
                               (mapcar #'second key)
                               (remove nil (mapcar #'fourth key))
                               (if rest-param (list rest-param) nil))))
      ;; Closure-boundary reset: rebind every registered per-compilation state
      ;; variable (define-compile-state, cil-compiler.lisp) to its fresh value
      ;; so inner closures don't inherit the enclosing body's compile context
      ;; (TCO state / *self-fn-local* especially: it refers to a local
      ;; declared in the OUTER method). Overrides carry the entries whose
      ;; fresh value is not a registry constant: *notinline-functions* is
      ;; this body's declarations on top of the enclosing ones, since a
      ;; NOTINLINE is lexical and reaches into nested lambdas (CLHS 3.2.2.1.1); *in-tail-position* T
      ;; (closure body's last form is in tail position: MV propagation)
      ;; duplicates the registered fresh-init for explicitness. Everything
      ;; that was previously bound AFTER the reset bindings in the old let*
      ;; is evaluated inside the thunk: the original evaluation order and
      ;; environment are preserved exactly.
      (call-with-fresh-closure-state
       (list (cons '*notinline-functions*
                   (append (extract-notinline body) *notinline-functions*))
             (cons '*lexical-decl-frames* (%push-decl-frame :lambda params body))
             (cons '*in-tail-position* t))
       (lambda ()
    (let* ((n-required (length required))
           (key-start (+ n-required (length optional)))
           ;; Compute boxing needs for params within this closure (single walk)
           (all-var-names (append (mapcar #'var-name all-params)
                                  (mapcar #'var-name free-vars)))
           (mc (multiple-value-list
                (find-mutated-and-captured-vars body all-var-names)))
           (mutated (first mc))
           (captured-inner (second mc))
           (needs-boxing (%strings-in-both mutated captured-inner))
           ;; Env slot locals
           (env-instrs '())
           (env-locals '()))
      ;; Set up free var locals from env
      (loop for fv in free-vars
            for i from 0
            do (let* ((key (gen-local (var-name fv)))
                      ;; FV itself keys the env local: the closure body's
                      ;; references resolve to the very symbol the enclosing
                      ;; scope bound, by identity. (Interning the name into
                      ;; DOTCL.CIL-COMPILER here used to break that, leaving the
                      ;; name fallback to reconnect them.)
                      (fv-sym fv)
                      (is-lispbox (member fv (compile-env-lispbox-fvs outer-env)))
                      (is-outer-boxed (member fv (compile-env-boxed-fvs outer-env)))
                      ;; Name the captured slot in the closure body's Locals, but
                      ;; only for real user variables (not labels cells / block /
                      ;; tagbody machinery): so a variable closed over shows by
                      ;; name when stopped inside the lambda. The box branches pass
                      ;; :BOX: the slot holds the cell, not the value, so it takes
                      ;; the PDB marker but no frame-locals store.
                      (user-var-p (not (%synthetic-capture-name-p (var-name fv))))
                      (marker (when user-var-p
                                (%local-var-marker key fv-sym)))
                      (box-marker (when user-var-p
                                    (%local-var-marker key fv-sym :box))))
                 (push (cons fv-sym key) env-locals)
                 (setf env-instrs
                       (append env-instrs
                               (cond
                                 ;; Debug data-var box: LispBox cell from env.
                                 (is-lispbox
                                  `((:declare-local ,key "LispBox")
                                    (:load-env ,i)
                                    (:stloc ,key)
                                    ,@box-marker))
                                 ;; LispObject[1] box (labels cell, or non-debug).
                                 (is-outer-boxed
                                  `((:declare-local ,key "LispObject[]")
                                    (:load-env ,i)
                                    (:stloc ,key)
                                    ,@box-marker))
                                 (t
                                  `((:declare-local ,key "LispObject")
                                    (:load-env ,i)
                                    (:stloc ,key)
                                    ,@marker)))))))
      ;; Set up params
      (let* ((param-locals
               (loop for p in all-params
                     collect (let ((key (gen-local (var-name p))))
                               (cons p key))))
             ;; Shadow symbol-macros whose names match captured env variables or params.
             ;; When a variable like X is both a symbol-macro (x -> (svref #:inst 0)) AND
             ;; captured in the env, accessing X inside the closure must use the local
             ;; variable, not the expansion. Without this shadow, the expansion would be
             ;; compiled but its free variables (e.g. #:inst) wouldn't be in *locals*.
             (*symbol-macros*
              (let ((local-syms (append all-params free-vars)))
                (remove-if (lambda (entry)
                             (let ((k (car entry)))
                               (some (lambda (l) (shadows-var-p l k)) local-syms)))
                           *symbol-macros*)))
             ;; One pack update: params+env into locals; boxed-vars maps
             ;; NEEDS-BOXING (a name list, the mutation/capture analysis is
             ;; string-keyed) back to the symbol that owns each name, a param or
             ;; a captured free var: interning only as a last resort, plus the
             ;; already-symbol OUTER-BOXED-FVS; and the captured flet/labels
             ;; functions, block tags, and non-local go tags are re-established
             ;; from the OUTER-ENV snapshot against the env slot locals.
             (*cstate*
              (cstate-with *cstate*
                +cs-locals+ (append param-locals env-locals)
                +cs-boxed-vars+
                (append
                 (mapcar (lambda (name)
                           (or (find name all-params :key #'var-name :test #'string=)
                               (find name free-vars :key #'var-name :test #'string=)
                               (compiler-name-sym name)))
                         needs-boxing)
                 (or (compile-env-boxed-fvs outer-env) '()))
                +cs-local-functions+
                (loop for outer-fn in (compile-env-local-functions outer-env)
                      for fn-name = (first outer-fn)
                      for fn-key = (second outer-fn)
                      for fn-boxed-p = (third outer-fn)
                      for mangled = (concatenate 'string "__LABELFN_" fn-name)
                      for captured = (or (find mangled free-vars :key #'var-name :test #'string=)
                                         (find fn-name free-vars :key #'var-name :test #'string=))
                      for inline-only = (and (not captured)
                                             (find mangled *closure-inline-only*
                                                   :key #'var-name :test #'string=))
                      when inline-only
                        collect (let ((key (gen-local "NOFN")))
                                  (push key (car *closure-inline-only-keys*))
                                  (list fn-name key fn-boxed-p nil nil nil (seventh outer-fn)))
                      when captured
                      collect (let ((env-entry (assoc captured env-locals :test #'eq)))
                                ;; A capture-lifted function cannot be reached from
                                ;; here: its extra parameters name slots of the
                                ;; ENCLOSING frame, which this closure has not
                                ;; captured, so a call from inside would pass the
                                ;; wrong count. Unwind and recompile the FLET the
                                ;; ordinary way.
                                (when (fourth outer-fn)
                                  (throw (%lift-capture-tag outer-fn) :lift-aborted))
                                (list fn-name (cdr env-entry) fn-boxed-p
                                      nil nil nil (seventh outer-fn))))
                +cs-block-tags+
                (loop for (bname . binfo) in (compile-env-block-tags outer-env)
                      for tag-var = (block-tag-var-name bname)
                      for env-entry = (local-entry-by-name tag-var env-locals)
                      when env-entry
                      collect (cons bname (list (cdr env-entry) nil nil nil nil (sixth binfo))))
                +cs-go-tags+
                ;; Format: (tag-name tb-var-name tb-id-key label-idx); after
                ;; capture tb-id-key becomes the env local key.
                (loop for gt-entry in (compile-env-go-tags outer-env)
                      for tag-name = (first gt-entry)
                      for tb-var-name = (second gt-entry)
                      for label-idx = (fourth gt-entry)
                      for env-entry = (local-entry-by-name tb-var-name env-locals)
                      when env-entry
                      ;; 5th/6th nil (closure go -> throw path); 7th/8th preserve
                      ;; the outer tagbody's needs-catch / has-go cells so the
                      ;; throw flags them (has-go: a go that exists only inside
                      ;; the closure still makes the outer tagbody a loop).
                      collect (list tag-name tb-var-name (cdr env-entry) label-idx
                                    nil nil (seventh gt-entry) (eighth gt-entry)))))
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
                          env-locals))
                (compile-env-boxed-fvs outer-env)))
             (inline-plans (%carry-inline-plans outer-env env-locals))
             (*cstate* (%with-inline-only-fn-locals *cstate*)))
        (declare (ignore inline-plans))
        (let* ((arity-instrs
                 (if direct-p
                     ;; :direct closure: arity check moves to the runtime-side
                     ;; args-array wrapper (MakeDirectClosure); the per-arity
                     ;; delegate signature enforces argc on the direct path.
                     ;; Structural guard: :direct must imply the required-only
                     ;; shape: a mismatch means the ignition predicate in
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
                   (%union-eq (fn-body-special-params body all-params)
                          (remove-if-not #'global-special-p all-params)))
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
                 (free-special-syms
                   (remove-if (lambda (s)
                                (some (lambda (p) (shadows-var-p p s)) all-params))
                              body-declared-specials))
                 (*cstate* (cstate-with *cstate* +cs-locals+
                                        (remove-locals-shadowed-by
                                         (append special-param-syms free-special-syms)
                                         (cstate-locals))))
                 ;; Self-TCO for a captured labels function; NIL for every other
                 ;; closure. Both helpers sit outside this body on purpose: it is
                 ;; one enormous form, and a binding added here costs far more
                 ;; emitted code than the same work called out of line.
                 (float-plan (%float-param-plan
                              required body
                              (append needs-boxing captured-inner
                                      (mapcar #'var-name special-param-syms))))
                 (*cstate* (%float-param-cstate float-plan))
                 (self-tco-key (closure-self-tco-key self-tco-name special-param-syms
                                                     optional key rest-param))
                 (tco-loop-label (when self-tco-key (gen-label "TCOLOOP")))
                 (*cstate* (closure-self-tco-cstate self-tco-name self-tco-key
                                                    tco-loop-label required
                                                    (%float-param-keys float-plan
                                                                       param-locals)))
                 (mv-key (closure-mv-mode-key direct-p special-param-syms))
                 (body-instrs (let ((*mv-mode-local* mv-key))
                                (call-with-param-type-locals
                                 all-params param-locals body
                                 (lambda () (compile-progn body))
                                 (compile-env-fv-types outer-env) env-locals))))
            (merge-disjoint-locals
             (if special-param-syms
                 `(,@arity-instrs
                   ,@key-check-instrs
                   ,@(%frame-enter-instrs fn-name)
                   ,@env-instrs ,@param-instrs
                   ,@(%float-param-entry-instrs float-plan param-locals)
                   ,@(compile-let-with-specials '() special-push-instrs body-instrs special-param-syms)
                   (:ret))
                 `(,@(%mv-mode-prologue mv-key body-instrs nil)
                   ,@arity-instrs
                   ,@key-check-instrs
                   ,@(%frame-enter-instrs fn-name)
                   ,@env-instrs ,@param-instrs
                   ,@(%float-param-entry-instrs float-plan param-locals)
                   ;; After the env and the parameters are bound: a tail
                   ;; self-call rewrites the parameter locals and branches here,
                   ;; so the captured env must not be re-read.
                   ,@(when tco-loop-label `((:label ,tco-loop-label)))
                   ,@(%tail-self-instrs fn-name body body-instrs t)
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
    ;; (funcall #'(setf f) v args...) for a (SETF F) function proclaimed
    ;; INLINE: the form a SETF of (F args...) expands to when F has no setf
    ;; expander. Substitute the definition like any other inline call. The
    ;; SETF expansion has already bound the place's arguments and then the new
    ;; value to variables in that order, so the substitution evaluates nothing
    ;; out of order.
    (when (and (consp fn-expr)
               (eq (car fn-expr) 'function)
               (consp (cdr fn-expr)) (null (cddr fn-expr))
               (%setf-function-name-p (cadr fn-expr)))
      ;; One that stores a word of adjacent octets: one wide store.
      (let ((ow (octet-word-store-info (cons 'funcall args))))
        (when ow
          (return-from compile-funcall (compile-octet-word-store ow t))))
      (let ((inl (maybe-expand-inline (cadr fn-expr) (cons (cadr fn-expr) (cdr args)))))
        (when inl
          (return-from compile-funcall
            (let ((*at-toplevel* nil))
              (compile-expr inl))))))
    ;; (funcall #'(setf acc) v obj) for a DEFCLASS slot writer: the form the
    ;; accessor's setf expander produces. Compile it as the named call
    ;; ((setf acc) v obj), whose writer inline cache would otherwise never see
    ;; (SETF (ACC OBJ) V), and every such write would be a full generic
    ;; function call.
    (when (and (consp fn-expr)
               (eq (car fn-expr) 'function)
               (consp (cdr fn-expr)) (null (cddr fn-expr))
               (consp (cadr fn-expr))
               (eq (car (cadr fn-expr)) 'setf)
               (consp (cdr (cadr fn-expr)))
               (symbolp (cadr (cadr fn-expr)))
               (null (cddr (cadr fn-expr)))
               (= (length (cdr args)) 2)
               (not *cross-compiling*)
               (not (local-function-entry (cadr fn-expr)))
               (gethash (cadr (cadr fn-expr)) *clos-accessor-writers*))
      (return-from compile-funcall
        (compile-named-call (cadr fn-expr) (cdr args))))
    ;; (funcall #'f args...) for a local function F is the call (f args...),
    ;; which can then be substituted when F is inline.
    (let ((call (%local-funcall-as-call (cons 'funcall args))))
      (when call
        (return-from compile-funcall (compile-expr call))))
    (if (and (consp fn-expr)
             (or (eq (car fn-expr) 'quote) (eq (car fn-expr) 'function))
             (symbolp (cadr fn-expr))
             ;; Only optimize if sym is NOT shadowed by a local flet/labels.
             (not (local-function-entry (cadr fn-expr))))
        ;; (funcall 'sym ...) or (funcall #'sym ...) -> compile as named call
        (compile-named-call (cadr fn-expr) (cdr args))
        ;; General case: evaluate fn, coerce symbol to function if needed
        (let ((call-args (cdr args))
              (n-call-args (length (cdr args)))
              (aref-plan (%funcall-aref-plan args)))
          (cond
            (aref-plan (compile-funcall-aref fn-expr aref-plan (%call-site-mv-mode args)))
            ((%funcall-long-arg-p args)
             (compile-funcall-long-arg fn-expr (cadr args) (%call-site-mv-mode args)))
            ((<= n-call-args 8)
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
                  ,@(%invoke-instrs n-call-args (%call-site-mv-mode args)))))
            (t
              `(,@(let ((*in-mv-context* nil) (*in-tail-position* nil))
                    (compile-expr fn-expr))
                (:call "Runtime.CoerceToFunction")
                ,@(compile-args-array call-args)
                (:callvirt "LispFunction.Invoke"))))))))

(defun %funcall-aref-plan (args)
  "For ARGS = (FN (AREF A S...)) of a FUNCALL whose value is a single value
   used here, A of rank 1 or 2 with its element storage not known here (or
   integer storage read without a hoisted buffer) and every subscript lowered
   to an int64: (A . SUBSCRIPTS). The runtime can then
   hand the element to a typed entry of FN without boxing it."
  (let ((arg (cadr args)))
    (and (consp (cdr args)) (null (cddr args))
         (consp arg) (eq (car arg) 'aref)
         (not (eq *in-tail-position* t))
         (not *in-mv-context*)
         (not (local-function-entry 'aref))
         (not (macrolet-shadowed-p 'aref))
         (consp (cdr arg))
         (<= 1 (length (cddr arg)) 2)
         (every #'fixnum-long-lowerable-p (cddr arg))
         (or (not (numeric-array-aref-entry arg))
             (and (numeric-array-aref-info arg)
                  (not (array-backing-entry-for-read arg))))
         (let ((mode (%call-site-mv-mode args)))
           (or (null mode) (equal mode `((:ldc-i4 ,+mv-mode-primary+)))))
         (cons (cadr arg) (cddr arg)))))

(defun compile-funcall-aref (fn-expr plan mode-instrs &optional long-p)
  "(FUNCALL FN-EXPR (AREF . PLAN)) through Runtime.FuncallAref<rank>L[L][M]: the
   function, the array and the subscripts are evaluated in source order, the
   function coerced before the array is read, as for any FUNCALL. LONG-P: the
   value is left as an int64, unboxed as UNBOX-FIXNUM does."
  (let* ((arr (car plan))
         (idxs (cdr plan))
         (fn-tmp (gen-local "FAF"))
         (arr-tmp (gen-local "FAA"))
         (idx-tmps (mapcar (lambda (i) (declare (ignore i)) (gen-local "FAI")) idxs)))
    `((:declare-local ,fn-tmp "LispFunction")
      (:declare-local ,arr-tmp "LispObject")
      ,@(mapcar (lambda (tk) `(:declare-local ,tk "Int64")) idx-tmps)
      ,@(let ((*in-mv-context* nil) (*in-tail-position* nil))
          (compile-expr fn-expr))
      (:call "Runtime.CoerceToFunction")
      (:stloc ,fn-tmp)
      ,@(let ((*in-mv-context* nil) (*in-tail-position* nil))
          (compile-expr arr))
      (:stloc ,arr-tmp)
      ,@(loop for idx in idxs
              for tk in idx-tmps
              append `(,@(compile-expr-to-long idx) (:stloc ,tk)))
      (:ldloc ,fn-tmp)
      (:ldloc ,arr-tmp)
      ,@(mapcar (lambda (tk) `(:ldloc ,tk)) idx-tmps)
      ,@mode-instrs
      (:call ,(format nil "Runtime.FuncallAref~DL~:[~;L~]~:[~;M~]"
                      (length idxs) long-p mode-instrs)))))

(defun %funcall-long-arg-p (args)
  "For ARGS = (FN X) of a FUNCALL whose value is a single value used here, X
   an element of an array whose integer storage is known here: X is read as an
   int64 and handed to a typed entry of FN without being boxed."
  (and (consp (cdr args)) (null (cddr args))
       (not (eq *in-tail-position* t))
       (not *in-mv-context*)
       (numeric-array-aref-info (cadr args))
       (let ((mode (%call-site-mv-mode args)))
         (or (null mode) (equal mode `((:ldc-i4 ,+mv-mode-primary+)))))))

(defun compile-funcall-long-arg (fn-expr x mode-instrs &optional long-p)
  "(FUNCALL FN-EXPR X) through Runtime.FuncallLong1[L][M], X compiled as an
   int64 after the function is coerced. LONG-P: the value is left as an int64,
   unboxed as UNBOX-FIXNUM does."
  (let ((fn-tmp (gen-local "FLF"))
        (x-tmp (gen-local "FLX")))
    `((:declare-local ,fn-tmp "LispFunction")
      (:declare-local ,x-tmp "Int64")
      ,@(let ((*in-mv-context* nil) (*in-tail-position* nil))
          (compile-expr fn-expr))
      (:call "Runtime.CoerceToFunction")
      (:stloc ,fn-tmp)
      ,@(let ((*in-mv-context* nil) (*in-tail-position* nil))
          (compile-as-long x))
      (:stloc ,x-tmp)
      (:ldloc ,fn-tmp)
      (:ldloc ,x-tmp)
      ,@mode-instrs
      (:call ,(format nil "Runtime.FuncallLong1~:[~;L~]~:[~;M~]" long-p mode-instrs)))))

(defun %funcall-long-arg-expr-p (expr)
  "For EXPR a (FUNCALL FN X) that COMPILE-FUNCALL would compile with
   COMPILE-FUNCALL-LONG-ARG, true."
  (and (consp expr) (eq (car expr) 'funcall)
       (consp (cdr expr))
       (let ((fn-expr (cadr expr)))
         (not (and (consp fn-expr) (member (car fn-expr) '(quote function)))))
       (%funcall-long-arg-p (cdr expr))))

(defun %funcall-aref-long-plan (expr)
  "For EXPR a (FUNCALL FN (AREF ...)) that COMPILE-FUNCALL would compile with
   COMPILE-FUNCALL-AREF, its plan."
  (and (consp expr) (eq (car expr) 'funcall)
       (consp (cdr expr))
       (let ((fn-expr (cadr expr)))
         (not (and (consp fn-expr) (member (car fn-expr) '(quote function)))))
       (%funcall-aref-plan (cdr expr))))

;;; ============================================================
;;; function special form + flet / labels
;;; ============================================================

;;; (TCO defvars moved to top of file for proper SBCL special-var recognition)

(defun compile-function-special (thing)
  "Compile (function name) or (function (lambda ...))."
  (cond
    ((and (consp thing) (eq (car thing) 'lambda))
     (compile-lambda (cadr thing) (cddr thing)))
    ;; (function (named-lambda name lambda-list . body)): SBCL extension
    ;; Treat as plain lambda, discarding the name
    ((and (consp thing) (symbolp (car thing))
          (string= (symbol-name (car thing)) "NAMED-LAMBDA"))
     (compile-lambda (caddr thing) (cdddr thing)))
    ;; (function (setf name)): a lexical (flet/labels) (setf name) shadows the
    ;; global one (CLHS 5.1.2.9 / the FUNCTION special form consults the lexical
    ;; environment), so check *local-functions* first, exactly like the symbol
    ;; case below. Without this, #'(setf f) always took the global SetfFunction
    ;; path and the setf-function fallback expansion, (funcall #'(setf f) ...),
    ;; failed with "Undefined function: (SETF F)" inside an flet that binds it
    ;; (eclector's set-standard-syntax-types).
    ((and (consp thing) (eq (car thing) 'setf) (symbolp (cadr thing)))
     (let ((local-fn (local-function-entry thing)))
       (if local-fn
           (let ((key (second local-fn))
                 (boxed-p (third local-fn)))
             (if boxed-p
                 `((:ldloc ,key) (:ldc-i4 0) (:ldelem-ref))
                 `((:ldloc ,key))))
           (progn
             ;; xref: #'(setf f) is an indirect-call reference, same edge as a call
             (xref-record-call thing)
             `(,@(compile-sym-lookup (cadr thing))
               (:castclass "Symbol")
               (:call "CilAssembler.GetSetfFunctionBySymbol"))))))
    ((symbolp thing)
     ;; Check local functions first
     (let ((local-fn (local-function-entry thing)))
       (if local-fn
           (let ((key (second local-fn))
                 (boxed-p (third local-fn)))
             ;; A lifted FLET function takes its captures as extra parameters,
             ;; so it cannot be handed out as a value. %LIFT-PLAN refuses a
             ;; function used as #'NAME in the source, but a macro can still
             ;; produce one (SBCL's backquote reads ,X as a structure the walk
             ;; does not see into); the FLET is then compiled as a closure.
             (when (fourth local-fn)
               (throw (%lift-capture-tag local-fn) :lift-aborted))
             (if boxed-p
                 `((:ldloc ,key) (:ldc-i4 0) (:ldelem-ref))
                 `((:ldloc ,key))))
           ;; Global function (built-in or user-defined): use symbol-based lookup
           ;; so that #'sym returns the SAME stable object as (symbol-function sym),
           ;; i.e. (eq #'car #'car) and (eq #'car (symbol-function 'car)) are T.
           ;; A fresh arity-checking wrapper per #' would break eq-on-builtin code
           ;; (memoization, function tables, cl-store's fdefinition round-trip).
           ;; Package-qualified names (e.g. #'bt2:current-thread) resolve to the
           ;; correct package's symbol via compile-fn-sym-lookup. GetFunctionBySymbol
           ;; is authoritative (no cross-package fallback): unqualified #'sym resolves
           ;; via Startup.Sym's bare-name bridge at symbol-resolution time.
           (progn
             ;; xref: #'f is an indirect-call reference (funcall/mapcar target),
             ;; recorded as the same caller->callee edge as a direct call.
             (xref-record-call thing)
             (when *labels-demoted* (%labels-demoted-check thing))
             `(,@(compile-fn-sym-lookup thing)
               (:castclass "Symbol")
               (:call "CilAssembler.GetFunctionBySymbol"))))))
    (t (error "FUNCTION: unsupported argument ~s" thing))))
