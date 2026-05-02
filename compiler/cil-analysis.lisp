;;; cil-analysis.lisp — Free variable, mutation, and capture analysis
;;; Part of the CIL compiler (A2 instruction list architecture)

(in-package :dotcl.cil-compiler)

;;; ============================================================
;;; Safe list iteration (handles dotted pairs)
;;; ============================================================

(defmacro do-list-safe ((var list) &body body)
  "Like dolist but handles dotted pairs without error.
   Iterates over car elements; stops at non-cons cdr."
  (let ((cur (gensym "CUR")))
    `(let ((,cur ,list))
       (loop while (consp ,cur)
             do (let ((,var (car ,cur)))
                  ,@body)
                (setf ,cur (cdr ,cur))))))

;;; ============================================================
;;; Block tag variable names (for non-local return-from capture)
;;; ============================================================

(defun block-tag-var-name (block-name)
  "Return the synthetic variable name for a block's tag.
   Used to track block tags as capturable variables for closures."
  (concatenate 'string "%BTAG-" (symbol-name block-name) "%"))

;;; ============================================================
;;; Free variable analysis (for lambda/closure)
;;; ============================================================

(defun find-free-vars (body bound-names)
  "Find free variable names (strings) referenced in body but not in bound-names set."
  (let ((free-ht (make-hash-table :test #'equal)))
    (dolist (form body)
      (find-free-vars-expr form bound-names free-ht))
    (let ((keys '())) (maphash (lambda (k v) (declare (ignore v)) (push k keys)) free-ht) keys)))

(defvar *macro-expand-depth-limit* 50)

;; Stub for cross-compilation: always returns T (no stack limit during self-compile)
(unless (fboundp '%stack-space-available-p)
  (defun %stack-space-available-p () t))

;;; find-free-vars-expr: iterative version using explicit worklist (#18).
;;; Each worklist entry is (expr bound . mdepth).
;;; Macrolet restore sentinels: (:restore-macro name . old-entry-or-nil).
(defun find-free-vars-expr (expr bound free-ht)
  "Walk expr finding free variable references. Results accumulated in free-ht.
   Iterative worklist version — no recursion depth limit."
  (let ((worklist (list (cons expr (cons bound 0)))))
    (loop while worklist do
      (let* ((item (pop worklist))
             (e (car item)))
        (cond
          ;; Restore-macro sentinel: restore *macros* entry after macrolet body.
          ;; Guard symbolp name so that a bare :restore-macro keyword from analyzed
          ;; source code (where cadr item is a bnd-list, not a symbol) is ignored.
          ((and (eq e :restore-macro) (symbolp (cadr item)))
           (let ((name (cadr item))
                 (old-entry (cddr item)))
             (if old-entry
                 (setf (gethash name *macros*) old-entry)
                 (remhash name *macros*))))
          ;; Restore-symbol-macros sentinel: restore *symbol-macros* after symbol-macrolet body
          ((eq e :restore-symbol-macros)
           (setf *symbol-macros* (cdr item)))
          (t
           (let ((bnd (cadr item))
                 (mdepth (cddr item)))
             (cond
               ;; Symbol: check if it's a free variable reference
               ((symbolp e)
                (when (and e
                           (or (not (eq e t)) (local-bound-p e))
                           (or (not (keywordp e)) (local-bound-p e))
                           (not (member (symbol-name e) bnd :test #'string=))
                           (not (gethash (symbol-name e) free-ht))
                           (local-bound-p e))
                  (setf (gethash (symbol-name e) free-ht) t)))
               ;; Cons: dispatch on head
               ((consp e)
                (let ((head (car e)))
                  (cond
                    ((and (symbolp head) (eq head 'quote)) nil)
                    ((and (symbolp head) (eq head 'defun)) nil)
                    ;; Lambda introduces new bindings
                    ((and (symbolp head) (eq head 'lambda))
                     (let* ((params (cadr e))
                            (lbody (cddr e))
                            (inner-bound (append (extract-param-names params) bnd)))
                       ;; Inline scan-lambda-list-defaults: push default forms
                       ;; with progressive scoping directly onto worklist
                       (let ((state :required)
                             (progressive-bound bnd))
                         (dolist (p params)
                           (cond
                             ((lambda-list-keyword-p p)
                              (case p
                                ((&rest &body) (setf state :rest))
                                (&optional (setf state :optional))
                                (&key (setf state :key))
                                (&aux (setf state :aux))
                                (t nil)))
                             ((eq state :required)
                              (push (symbol-name p) progressive-bound))
                             ((member state '(:optional :key :aux))
                              (when (and (consp p) (cadr p))
                                (push (cons (cadr p) (cons progressive-bound mdepth)) worklist))
                              (let ((name (if (consp p) (car p) p)))
                                (when (consp name) (setf name (cadr name)))
                                (push (symbol-name name) progressive-bound))
                              (when (and (consp p) (caddr p))
                                (push (symbol-name (caddr p)) progressive-bound)))
                             ((eq state :rest)
                              (push (symbol-name p) progressive-bound)))))
                       ;; Push body forms with all params bound
                       (dolist (form lbody)
                         (push (cons form (cons inner-bound mdepth)) worklist))))
                    ;; Let/Let* introduces bindings
                    ((and (symbolp head) (member head '(let let*)))
                     (let* ((bindings (cadr e))
                            (lbody (cddr e))
                            (inner-bound (copy-list bnd))
                            (is-star (eq head 'let*)))
                       (dolist (b bindings)
                         (let ((init (if (consp b) (cadr b) nil))
                               (vn (symbol-name (if (consp b) (car b) b))))
                           (when init
                             (push (cons init (cons (if is-star inner-bound bnd) mdepth)) worklist))
                           (push vn inner-bound)))
                       (dolist (form lbody)
                         (push (cons form (cons inner-bound mdepth)) worklist))))
                    ;; setq: analyze all target/value pairs
                    ((and (symbolp head) (eq head 'setq))
                     (loop for (var val) on (cdr e) by #'cddr
                           do (when var (push (cons var (cons bnd mdepth)) worklist))
                              (when val (push (cons val (cons bnd mdepth)) worklist))))
                    ;; go: check if tagbody ID needs capture
                    ((and (symbolp head) (eq head 'go))
                     (let* ((tag (cadr e))
                            (entry (assoc tag *go-tags*)))
                       (when entry
                         (let ((tb-var-name (second entry)))
                           (when (and (not (member tb-var-name bnd :test #'string=))
                                      (not (gethash tb-var-name free-ht))
                                      (local-bound-p (intern tb-var-name :dotcl.cil-compiler)))
                             (setf (gethash tb-var-name free-ht) t))))))
                    ;; Block introduces a synthetic block-tag variable
                    ((and (symbolp head) (eq head 'block))
                     (let* ((bname (cadr e))
                            (tag-var (block-tag-var-name bname))
                            (inner-bound (cons tag-var bnd)))
                       (dolist (form (cddr e))
                         (push (cons form (cons inner-bound mdepth)) worklist))))
                    ;; return-from: check block tag capture + scan value
                    ((and (symbolp head) (eq head 'return-from))
                     (let* ((bname (cadr e))
                            (tag-var (block-tag-var-name bname)))
                       (when (and (not (member tag-var bnd :test #'string=))
                                  (not (gethash tag-var free-ht))
                                  (local-bound-p (intern tag-var :dotcl.cil-compiler)))
                         (setf (gethash tag-var free-ht) t)))
                     (when (caddr e)
                       (push (cons (caddr e) (cons bnd mdepth)) worklist)))
                    ;; return: (return expr) = (return-from nil expr)
                    ((and (symbolp head) (eq head 'return))
                     (let ((tag-var (block-tag-var-name nil)))
                       (when (and (not (member tag-var bnd :test #'string=))
                                  (not (gethash tag-var free-ht))
                                  (local-bound-p (intern tag-var :dotcl.cil-compiler)))
                         (setf (gethash tag-var free-ht) t)))
                     (when (cadr e)
                       (push (cons (cadr e) (cons bnd mdepth)) worklist)))
                    ;; (function sym) or (function (lambda ...))
                    ((and (symbolp head) (eq head 'function))
                     (let ((arg (cadr e)))
                       (cond
                         ((and (consp arg) (eq (car arg) 'lambda))
                          (push (cons arg (cons bnd mdepth)) worklist))
                         ((symbolp arg)
                          (when (and arg (or (not (eq arg t)) (local-bound-p arg))
                                     (not (special-var-p arg))
                                     (local-bound-p arg))
                            (let* ((plain-name (symbol-name arg))
                                   (mangled-name (concatenate 'string "__LABELFN_" plain-name))
                                   (capture-name (cond
                                                   ((local-bound-p (intern mangled-name :dotcl.cil-compiler))
                                                    mangled-name)
                                                   (t plain-name))))
                              (when (and (not (member capture-name bnd :test #'string=))
                                         (not (gethash capture-name free-ht)))
                                (setf (gethash capture-name free-ht) t))))))))
                    ;; handler-case: body + clauses with optional var binding
                    ((and (symbolp head) (eq head 'handler-case))
                     (let ((body-form (cadr e))
                           (hc-clauses (cddr e)))
                       (push (cons body-form (cons bnd mdepth)) worklist)
                       (dolist (clause hc-clauses)
                         (let* ((lambda-list (cadr clause))
                                (var (if (and lambda-list (car lambda-list))
                                         (car lambda-list) nil))
                                (handler-body (cddr clause))
                                (inner-bound (if var
                                                 (cons (symbol-name var) bnd)
                                                 bnd)))
                           (dolist (form handler-body)
                             (push (cons form (cons inner-bound mdepth)) worklist))))))
                    ;; handler-bind: bindings + body
                    ((and (symbolp head) (eq head 'handler-bind))
                     (let ((hb-bindings (cadr e))
                           (hb-body (cddr e)))
                       (dolist (binding hb-bindings)
                         (when (cadr binding)
                           (push (cons (cadr binding) (cons bnd mdepth)) worklist)))
                       (dolist (form hb-body)
                         (push (cons form (cons bnd mdepth)) worklist))))
                    ;; restart-case: body + clauses with params
                    ((and (symbolp head) (eq head 'restart-case))
                     (push (cons (cadr e) (cons bnd mdepth)) worklist)
                     (dolist (clause (cddr e))
                       (let* ((params (cadr clause))
                              (handler-body (cddr clause))
                              (param-names
                                (let ((names nil))
                                  (dolist (p params)
                                    (cond ((member p '(&optional &rest &key &aux &allow-other-keys)) nil)
                                          ((consp p) (push (symbol-name (car p)) names))
                                          ((symbolp p) (push (symbol-name p) names))))
                                  (nreverse names)))
                              (inner-bound (append param-names bnd)))
                         ;; Default value forms in optional/key params
                         (dolist (p params)
                           (when (and (consp p) (cdr p))
                             (push (cons (cadr p) (cons bnd mdepth)) worklist)))
                         (dolist (form handler-body)
                           (push (cons form (cons inner-bound mdepth)) worklist)))))
                    ;; macrolet: register macros, push body, push restore sentinels
                    ((and (symbolp head) (eq head 'macrolet))
                     (let ((macro-defs (cadr e))
                           (mlbody (cddr e)))
                       ;; Push restore sentinels FIRST (LIFO: processed LAST, after body)
                       (dolist (def macro-defs)
                         (let* ((mname (car def))
                                (old-entry (gethash mname *macros*)))
                           (push (cons :restore-macro (cons mname old-entry)) worklist)))
                       ;; Register macros immediately (same as compile-macrolet)
                       (dolist (def macro-defs)
                         (let* ((mname (car def))
                                (mparams (cadr def))
                                (mbody (cddr def)))
                           (setf (gethash mname *macros*)
                                 (eval `(lambda (form)
                                          (destructuring-bind ,mparams (cdr form)
                                            ,@mbody))))))
                       ;; Push body forms (LIFO: processed BEFORE restore sentinels)
                       (dolist (form mlbody)
                         (push (cons form (cons bnd mdepth)) worklist))))
                    ;; symbol-macrolet: extend *symbol-macros* during body walk
                    ((and (symbolp head) (eq head 'symbol-macrolet))
                     (let ((sm-bindings (cadr e))
                           (sm-body (cddr e)))
                       ;; Push restore sentinel FIRST (LIFO: processed LAST, after body)
                       (push (cons :restore-symbol-macros *symbol-macros*) worklist)
                       ;; Extend *symbol-macros* immediately so macro expansions inside
                       ;; the body see the correct symbol-macro bindings
                       (setf *symbol-macros*
                             (append (mapcar (lambda (b) (cons (car b) (cadr b))) sm-bindings)
                                     *symbol-macros*))
                       ;; Push body forms (LIFO: processed BEFORE restore sentinel)
                       (dolist (form sm-body)
                         (push (cons form (cons bnd mdepth)) worklist))))
                    ;; flet/labels: function definitions + body
                    ((and (symbolp head) (member head '(flet labels)))
                     (let* ((fn-defs (cadr e))
                            (lbody (cddr e))
                            (fn-names (loop for fd in fn-defs
                                            for name = (car fd)
                                            when (symbolp name) collect (symbol-name name))))
                       ;; Function bodies see outer scope (flet) or same scope (labels)
                       ;; Labels fn-names are NOT added to fn body bound — they are captured
                       ;; as free vars via boxed variables in *locals*
                       (let ((fn-body-bound bnd))
                         (dolist (fd fn-defs)
                           (let* ((params (cadr fd))
                                  (fn-body (cddr fd))
                                  (inner-bound (append (extract-param-names params)
                                                       fn-body-bound)))
                             ;; Inline scan-lambda-list-defaults for fn params
                             (let ((state :required)
                                   (progressive-bound fn-body-bound))
                               (dolist (p params)
                                 (cond
                                   ((lambda-list-keyword-p p)
                                    (case p
                                      ((&rest &body) (setf state :rest))
                                      (&optional (setf state :optional))
                                      (&key (setf state :key))
                                      (&aux (setf state :aux))
                                      (t nil)))
                                   ((eq state :required)
                                    (push (symbol-name p) progressive-bound))
                                   ((member state '(:optional :key :aux))
                                    (when (and (consp p) (cadr p))
                                      (push (cons (cadr p) (cons progressive-bound mdepth)) worklist))
                                    (let ((name (if (consp p) (car p) p)))
                                      (when (consp name) (setf name (cadr name)))
                                      (push (symbol-name name) progressive-bound))
                                    (when (and (consp p) (caddr p))
                                      (push (symbol-name (caddr p)) progressive-bound)))
                                   ((eq state :rest)
                                    (push (symbol-name p) progressive-bound)))))
                             ;; Push fn body forms
                             (dolist (form fn-body)
                               (push (cons form (cons inner-bound mdepth)) worklist)))))
                       ;; Body sees all fn-names as bound
                       (let ((body-bound (append fn-names bnd)))
                         (dolist (form lbody)
                           (push (cons form (cons body-bound mdepth)) worklist)))))
                    ;; CLOS primitives — analyze sub-expressions normally
                    ((and (symbolp head) (member head '(%make-class %make-slot-def %register-class %set-class-default-initargs
                                                        find-class %find-class-or-nil class-of class-name
                                                        slot-value slot-boundp %set-slot-value
                                                        %allocate-instance %slot-exists-p
                                                        make-instance %make-instance-with-initargs
                                                        %make-gf %register-gf %set-method-combination %set-method-combination-order %set-method-combination-args %find-gf
                                                        %clear-defgeneric-inline-methods %mark-defgeneric-inline-method
                                                        %make-method %add-method
                                                        %gf-methods %method-specializers
                                                        %method-qualifiers %method-function
                                                        call-next-method next-method-p
                                                        %change-class)))
                     (dolist (sub (cdr e))
                       (push (cons sub (cons bnd mdepth)) worklist)))
                    ;; Default: try macro expansion, then generic walk
                    (t
                     (let ((expanded nil))
                       (when (and (symbolp head) head
                                  (< mdepth *macro-expand-depth-limit*)
                                  (%stack-space-available-p)
                                  (find-macro-expander head))
                         (let ((expander (find-macro-expander head)))
                           (setf expanded (handler-case (cached-macroexpand e expander)
                                            (error () nil)))))
                       (if expanded
                           (push (cons expanded (cons bnd (1+ mdepth))) worklist)
                           (progn
                             ;; Labels function mangled name capture
                             (when (and (symbolp head) head)
                               (let* ((name (symbol-name head))
                                      (mangled (concatenate 'string "__LABELFN_" name)))
                                 (when (and (local-bound-p (intern mangled :dotcl.cil-compiler))
                                            (not (member mangled bnd :test #'string=))
                                            (not (gethash mangled free-ht)))
                                   (setf (gethash mangled free-ht) t))))
                             ;; Generic: push all sub-expressions
                             (do-list-safe (sub e)
                               (push (cons sub (cons bnd mdepth)) worklist)))))))))))))))))

;;; ============================================================
;;; Mutated/captured variable analysis (for boxing)
;;; ============================================================

(defun find-mutated-vars (body)
  "Find variable names (strings) that are setq targets in body."
  (let ((result (make-hash-table :test #'equal)))
    (dolist (form body)
      (find-mutated-vars-expr form result))
    (let ((keys '())) (maphash (lambda (k v) (declare (ignore v)) (push k keys)) result) keys)))

;;; find-mutated-vars-expr: iterative version using explicit worklist (#18).
;;; Each worklist entry is (expr . macro-depth) OR a restore sentinel
;;; (:restore-symbol-macros . old-*symbol-macros*).
(defun find-mutated-vars-expr (expr result-ht)
  (let ((worklist (list (cons expr 0))))
    (loop while worklist do
      (let* ((item (pop worklist))
             (e (car item))
             (mdepth (cdr item)))
        (cond
          ;; Restore-symbol-macros sentinel: mdepth slot holds old *symbol-macros*
          ((eq e :restore-symbol-macros)
           (setf *symbol-macros* mdepth))
          ;; Restore-macro sentinel: mdepth slot holds (name . old-entry).
          ;; Guard consp so that a bare :restore-macro keyword from analyzed source
          ;; code (where mdepth is a number) does not trigger this branch.
          ((and (eq e :restore-macro) (consp mdepth))
           (let ((name (car mdepth))
                 (old-entry (cdr mdepth)))
             (if old-entry
                 (setf (gethash name *macros*) old-entry)
                 (remhash name *macros*))))
          (t
           (when (consp e)
             (let ((head (car e)))
               (cond
                 ((and (symbolp head) (or (eq head 'setq) (eq head 'setf)
                                           (eq head 'psetq) (eq head 'psetf)))
                   (loop for (var val) on (cdr e) by #'cddr
                         do (cond
                              ((and var (symbolp var))
                               (setf (gethash (symbol-name var) result-ht) t))
                              ((consp var)
                               (push (cons var mdepth) worklist)))
                            (when val (push (cons val mdepth) worklist))))
                  ((and (symbolp head) (string= (symbol-name head) "MULTIPLE-VALUE-SETQ"))
                   (let ((vars (cadr e)))
                     (when (listp vars)
                       (dolist (v vars)
                         (when (symbolp v)
                           (setf (gethash (symbol-name v) result-ht) t)))))
                   (when (caddr e)
                     (push (cons (caddr e) mdepth) worklist)))
                  ((and (symbolp head) (member (symbol-name head) '("PUSH" "PUSHNEW") :test #'string=))
                   (let ((place (caddr e)))
                     (when (and place (symbolp place) (not (eq place t)))
                       (setf (gethash (symbol-name place) result-ht) t)))
                   (do-list-safe (sub (cdr e))
                     (push (cons sub mdepth) worklist)))
                  ((and (symbolp head) (member (symbol-name head) '("POP" "INCF" "DECF") :test #'string=))
                   (let ((place (cadr e)))
                     (when (and place (symbolp place) (not (eq place t)))
                       (setf (gethash (symbol-name place) result-ht) t)))
                   (do-list-safe (sub (cdr e))
                     (push (cons sub mdepth) worklist)))
                  ((and (symbolp head) (member (symbol-name head) '("ROTATEF" "SHIFTF") :test #'string=))
                   (dolist (arg (cdr e))
                     (when (and (symbolp arg) arg (not (eq arg t)))
                       (setf (gethash (symbol-name arg) result-ht) t))
                     (when (consp arg)
                       (push (cons arg mdepth) worklist))))
                  ((and (symbolp head) (eq head 'quote)) nil)
                  ((and (symbolp head) (eq head 'defun))
                   (dolist (form (cdddr e))
                     (push (cons form mdepth) worklist)))
                  ((and (symbolp head) (member head '(let let*)))
                   (let ((bindings (cadr e))
                         (lbody (cddr e)))
                     (dolist (b bindings)
                       (when (and (consp b) (cadr b))
                         (push (cons (cadr b) mdepth) worklist)))
                     (dolist (form lbody)
                       (push (cons form mdepth) worklist))))
                  ((and (symbolp head) (eq head 'handler-case))
                   (when (cadr e) (push (cons (cadr e) mdepth) worklist))
                   (dolist (clause (cddr e))
                     (dolist (form (cddr clause))
                       (push (cons form mdepth) worklist))))
                  ((and (symbolp head) (eq head 'handler-bind))
                   (dolist (binding (cadr e))
                     (when (cadr binding) (push (cons (cadr binding) mdepth) worklist)))
                   (dolist (form (cddr e))
                     (push (cons form mdepth) worklist)))
                  ((and (symbolp head) (eq head 'restart-case))
                   (when (cadr e) (push (cons (cadr e) mdepth) worklist))
                   (dolist (clause (cddr e))
                     (dolist (form (cddr clause))
                       (push (cons form mdepth) worklist))))
                  ;; macrolet: register local macros, push restore sentinels, push body
                  ((and (symbolp head) (eq head 'macrolet))
                   (let ((macro-defs (cadr e))
                         (mlbody (cddr e)))
                     ;; Push restore sentinels FIRST (LIFO: processed LAST, after body)
                     (dolist (def macro-defs)
                       (let* ((mname (car def))
                              (old-entry (gethash mname *macros*)))
                         (push (cons :restore-macro (cons mname old-entry)) worklist)))
                     ;; Register macros immediately
                     (dolist (def macro-defs)
                       (let* ((mname (car def))
                              (mparams (cadr def))
                              (mbody (cddr def)))
                         (setf (gethash mname *macros*)
                               (eval `(lambda (form)
                                        (destructuring-bind ,mparams (cdr form)
                                          ,@mbody))))))
                     ;; Push body forms (LIFO: processed BEFORE restore sentinels)
                     (dolist (form mlbody)
                       (push (cons form mdepth) worklist))))
                  ;; symbol-macrolet: extend *symbol-macros* during body walk
                  ((and (symbolp head) (eq head 'symbol-macrolet))
                   (let ((sm-bindings (cadr e))
                         (sm-body (cddr e)))
                     (push (cons :restore-symbol-macros *symbol-macros*) worklist)
                     (setf *symbol-macros*
                           (append (mapcar (lambda (b) (cons (car b) (cadr b))) sm-bindings)
                                   *symbol-macros*))
                     (dolist (form sm-body)
                       (push (cons form mdepth) worklist))))
                  (t
                   (let ((expanded nil))
                     (when (and (symbolp head) head
                                (< mdepth *macro-expand-depth-limit*)
                                (%stack-space-available-p)
                                (find-macro-expander head))
                       (let ((expander (find-macro-expander head)))
                         (setf expanded (handler-case (cached-macroexpand e expander)
                                          (error () nil)))))
                     (if expanded
                         (push (cons expanded (1+ mdepth)) worklist)
                         (do-list-safe (sub e)
                           (push (cons sub mdepth) worklist))))))))))))))

(defun find-captured-vars (body var-names)
  "Find variable names (strings) from var-names that are referenced inside lambda bodies."
  (let ((result (make-hash-table :test #'equal)))
    (dolist (form body)
      (find-captured-vars-expr form var-names result nil))
    (let ((keys '())) (maphash (lambda (k v) (declare (ignore v)) (push k keys)) result) keys)))


;;; find-captured-vars-expr: iterative version using explicit worklist (#18).
;;; Each worklist entry is (expr inside-lambda . macro-depth).
(defun find-captured-vars-expr (expr var-names result-ht inside-lambda)
  (let ((worklist (list (cons expr (cons inside-lambda 0)))))
    (loop while worklist do
      (let* ((item (pop worklist))
             (e (car item))
             (in-lambda (cadr item))
             (mdepth (cddr item)))
        (cond
          ;; Restore-symbol-macros sentinel: in-lambda slot holds old *symbol-macros*
          ((eq e :restore-symbol-macros)
           (setf *symbol-macros* in-lambda))
          ;; Restore-macro sentinel: in-lambda slot holds (name . old-entry).
          ;; Guard consp so that a bare :restore-macro keyword from analyzed source
          ;; code does not trigger this branch.
          ((and (eq e :restore-macro) (consp in-lambda))
           (let ((name (car in-lambda))
                 (old-entry (cdr in-lambda)))
             (if old-entry
                 (setf (gethash name *macros*) old-entry)
                 (remhash name *macros*))))
          ((and (symbolp e) in-lambda)
           (when (member (symbol-name e) var-names :test #'string=)
             (setf (gethash (symbol-name e) result-ht) t)))
          ((consp e)
           (let ((head (car e)))
             (cond
               ((and (symbolp head) (eq head 'quote)) nil)
               ((and (symbolp head) (eq head 'defun))
                (dolist (form (cdddr e))
                  (push (cons form (cons t mdepth)) worklist)))
               ((and (symbolp head) (eq head 'lambda))
                (dolist (sub (cdr e))
                  (push (cons sub (cons t mdepth)) worklist)))
               ((and (symbolp head) (member head '(let let*)))
                (let ((bindings (cadr e))
                      (lbody (cddr e)))
                  (dolist (b bindings)
                    (when (and (consp b) (cadr b))
                      (push (cons (cadr b) (cons in-lambda mdepth)) worklist)))
                  (dolist (form lbody)
                    (push (cons form (cons in-lambda mdepth)) worklist))))
               ((and (symbolp head) (or (eq head 'flet) (eq head 'labels)))
                (dolist (fdef (cadr e))
                  (dolist (form (cddr fdef))
                    (push (cons form (cons t mdepth)) worklist)))
                (dolist (form (cddr e))
                  (push (cons form (cons in-lambda mdepth)) worklist)))
               ((and (symbolp head) (eq head 'handler-case))
                (when (cadr e)
                  (push (cons (cadr e) (cons in-lambda mdepth)) worklist))
                (dolist (clause (cddr e))
                  (dolist (form (cddr clause))
                    (push (cons form (cons in-lambda mdepth)) worklist))))
               ((and (symbolp head) (eq head 'handler-bind))
                (dolist (binding (cadr e))
                  (when (cadr binding)
                    (push (cons (cadr binding) (cons in-lambda mdepth)) worklist)))
                (dolist (form (cddr e))
                  (push (cons form (cons in-lambda mdepth)) worklist)))
               ((and (symbolp head) (eq head 'restart-case))
                (when (cadr e)
                  (push (cons (cadr e) (cons in-lambda mdepth)) worklist))
                (dolist (clause (cddr e))
                  (dolist (form (cddr clause))
                    (push (cons form (cons in-lambda mdepth)) worklist))))
               ;; macrolet: register local macros, push restore sentinels, push body
               ;; Sentinel reuses in-lambda slot for (name . old-entry)
               ((and (symbolp head) (eq head 'macrolet))
                (let ((macro-defs (cadr e))
                      (mlbody (cddr e)))
                  ;; Push restore sentinels FIRST (LIFO: processed LAST, after body)
                  (dolist (def macro-defs)
                    (let* ((mname (car def))
                           (old-entry (gethash mname *macros*)))
                      (push (cons :restore-macro (cons (cons mname old-entry) mdepth)) worklist)))
                  ;; Register macros immediately
                  (dolist (def macro-defs)
                    (let* ((mname (car def))
                           (mparams (cadr def))
                           (mbody (cddr def)))
                      (setf (gethash mname *macros*)
                            (eval `(lambda (form)
                                     (destructuring-bind ,mparams (cdr form)
                                       ,@mbody))))))
                  ;; Push body forms (LIFO: processed BEFORE restore sentinels)
                  (dolist (form mlbody)
                    (push (cons form (cons in-lambda mdepth)) worklist))))
               ;; symbol-macrolet: extend *symbol-macros* during body walk
               ;; Sentinel reuses in-lambda slot for old-*symbol-macros*
               ((and (symbolp head) (eq head 'symbol-macrolet))
                (let ((sm-bindings (cadr e))
                      (sm-body (cddr e)))
                  ;; Push restore sentinel FIRST (in-lambda slot holds old value)
                  (push (cons :restore-symbol-macros (cons *symbol-macros* mdepth)) worklist)
                  ;; Extend *symbol-macros* immediately
                  (setf *symbol-macros*
                        (append (mapcar (lambda (b) (cons (car b) (cadr b))) sm-bindings)
                                *symbol-macros*))
                  ;; Push body forms
                  (dolist (form sm-body)
                    (push (cons form (cons in-lambda mdepth)) worklist))))
               (t
                (let ((expanded nil))
                  (when (and (symbolp head) head
                             (< mdepth *macro-expand-depth-limit*)
                             (%stack-space-available-p)
                             (find-macro-expander head))
                    (let ((expander (find-macro-expander head)))
                      (setf expanded (handler-case (cached-macroexpand e expander)
                                       (error () nil)))))
                  (if expanded
                      (push (cons expanded (cons in-lambda (1+ mdepth))) worklist)
                      (do-list-safe (sub e)
                        (push (cons sub (cons in-lambda mdepth)) worklist)))))))))))))

;;; ============================================================
;;; Copy propagation: eliminate single-reference let locals (#198)
;;; ============================================================

(defun eliminate-single-ref-locals (instrs)
  "Peephole: remove :declare-local / :stloc / :ldloc for single-reference locals.
   A local KEY is eligible when:
     - exactly 1 (:stloc KEY) and 1 (:ldloc KEY) appear in INSTRS
     - type in the corresponding :declare-local is \"LispObject\" (not a box array)
     - (:stloc KEY) and (:ldloc KEY) are consecutive in the non-:declare-local
       instruction subsequence (only :declare-local instructions may appear between)
   Preserves CIL stack semantics and evaluation order."
  (let ((stloc-count (make-hash-table :test #'equal))
        (ldloc-count (make-hash-table :test #'equal))
        (local-type  (make-hash-table :test #'equal)))
    (dolist (instr instrs)
      (when (consp instr)
        (let ((op (car instr)) (key (cadr instr)))
          (cond
            ((eq op :declare-local) (setf (gethash key local-type) (caddr instr)))
            ((eq op :stloc) (incf (gethash key stloc-count 0)))
            ((eq op :ldloc) (incf (gethash key ldloc-count 0)))))))
    ;; Eligible keys: single stloc, single ldloc, LispObject type
    (let ((single-ref (make-hash-table :test #'equal)))
      (maphash (lambda (key sc)
                 (when (and (= sc 1)
                            (= (gethash key ldloc-count 0) 1)
                            (string= (gethash key local-type "") "LispObject"))
                   (setf (gethash key single-ref) t)))
               stloc-count)
      (when (zerop (hash-table-count single-ref))
        (return-from eliminate-single-ref-locals instrs))
      ;; Find consecutive (stloc KEY)(ldloc KEY) pairs skipping :declare-local
      (let ((removable (make-hash-table :test #'equal))
            (prev nil))
        (dolist (instr instrs)
          (when (consp instr)
            (let ((op (car instr)) (key (cadr instr)))
              (cond
                ((eq op :declare-local))        ; transparent: don't reset prev
                ((eq op :stloc)
                 (setf prev (and (gethash key single-ref) key)))
                ((eq op :ldloc)
                 (cond
                   ((and prev (equal prev key))
                    (setf (gethash key removable) t)
                    (setf prev nil))
                   (t (setf prev nil))))
                (t (setf prev nil))))))
        (if (zerop (hash-table-count removable))
            instrs
            (remove-if (lambda (instr)
                         (and (consp instr)
                              (gethash (cadr instr) removable)
                              (member (car instr) '(:declare-local :stloc :ldloc))))
                       instrs))))))

;;; ============================================================
;;; Slot sharing: merge LispObject locals with disjoint flat ranges (#199)
;;; ============================================================

(defun merge-disjoint-locals (instrs)
  "Linear-scan slot sharing: merge LispObject locals whose flat live ranges
   do not overlap. When last-use(K1) < first-def(K2) in flat instruction order,
   K2 can reuse K1's slot. Reduces local variable count across exclusive cond arms.
   Applied once per function body. Does NOT recurse into nested :body lists.
   Skipped entirely when any backward branch is present (loops, TCO)."
  ;; Pre-scan: bail out if any backward branch is present.
  ;; A backward branch targets a label whose position <= the branch's own position.
  (let ((label-pos (make-hash-table :test #'equal))
        (scan-pos 0))
    (dolist (instr instrs)
      (when (and (consp instr) (eq (car instr) :label))
        (setf (gethash (cadr instr) label-pos) scan-pos))
      (incf scan-pos))
    (let ((fwd-pos 0))
      (dolist (instr instrs)
        (when (and (consp instr)
                   (member (car instr) '(:br :brtrue :brfalse))
                   (let ((tgt (gethash (cadr instr) label-pos)))
                     (and tgt (<= tgt fwd-pos))))
          (return-from merge-disjoint-locals instrs))
        (incf fwd-pos))))
  (let ((first-pos  (make-hash-table :test #'equal))
        (last-pos   (make-hash-table :test #'equal))
        (local-type (make-hash-table :test #'equal))
        (pos 0))
    ;; Pass 1: collect types and compute [first-pos, last-pos] for each key
    (dolist (instr instrs)
      (when (consp instr)
        (let ((op (car instr)) (key (cadr instr)))
          (cond
            ((eq op :declare-local)
             (setf (gethash key local-type) (caddr instr)))
            ((or (eq op :stloc) (eq op :ldloc))
             (unless (gethash key first-pos)
               (setf (gethash key first-pos) pos))
             (setf (gethash key last-pos) pos)))))
      (incf pos))
    ;; Collect eligible candidates: LispObject type with at least one use
    (let ((candidates nil))
      (maphash (lambda (key type)
                 (when (and (string= type "LispObject")
                            (gethash key first-pos)
                            (gethash key last-pos))
                   (push (list (gethash key first-pos)
                               (gethash key last-pos)
                               key)
                         candidates)))
               local-type)
      (when (< (length candidates) 2)
        (return-from merge-disjoint-locals instrs))
      ;; Sort by first-pos ascending
      (setf candidates (sort candidates #'< :key #'first))
      ;; Linear scan: for each key in order, find an expired free slot to reuse
      ;; free-slots: list of (last-pos . canonical-key) cons cells
      (let ((rename (make-hash-table :test #'equal))
            (free-slots nil))
        (dolist (cand candidates)
          (let* ((fp (first cand))
                 (lp (second cand))
                 (key (third cand))
                 (slot (find-if (lambda (s) (< (car s) fp)) free-slots)))
            (if slot
                (let ((canonical (cdr slot)))
                  (setf (gethash key rename) canonical)
                  (setf free-slots (delete slot free-slots :test #'eq))
                  (push (cons lp canonical) free-slots))
                (push (cons lp key) free-slots))))
        (when (zerop (hash-table-count rename))
          (return-from merge-disjoint-locals instrs))
        ;; Pass 2: rename stloc/ldloc for merged keys; deduplicate :declare-local
        (let ((seen-declare (make-hash-table :test #'equal)))
          (remove nil
                  (mapcar (lambda (instr)
                            (cond
                              ((not (consp instr)) instr)
                              (t
                               (let ((op (car instr)) (key (cadr instr)))
                                 (cond
                                   ((eq op :declare-local)
                                    (let ((canonical (or (gethash key rename) key)))
                                      (if (gethash canonical seen-declare)
                                          nil
                                          (progn
                                            (setf (gethash canonical seen-declare) t)
                                            `(:declare-local ,canonical ,(caddr instr))))))
                                   ((or (eq op :stloc) (eq op :ldloc))
                                    (let ((canonical (gethash key rename)))
                                      (if canonical
                                          `(,op ,canonical)
                                          instr)))
                                   (t instr))))))
                          instrs)))))))

;;; ============================================================
;;; Top-level compilation
;;; ============================================================

(defun compile-toplevel (expr)
  "Compile a top-level expression. Returns instruction list."
  (let ((*locals* '())
        (*var-counter* 0)
        (*label-counter* 0)
        (*block-tags* '())
        (*go-tags* '())
        (*specials* '())
        (*boxed-vars* '())
        (*local-functions* '())
        (*at-toplevel* t)
        (*macroexpand-cache* (make-hash-table :test #'eq)))
    `(,@(compile-expr expr)
      (:ret))))

(defun compile-toplevel-eval (expr)
  "Compile a top-level expression for EVAL.
   Like compile-toplevel but preserves MvReturn at the tail so EVAL's
   caller can observe the form's multiple values (D638, issue #19)."
  (let ((*locals* '())
        (*var-counter* 0)
        (*label-counter* 0)
        (*block-tags* '())
        (*go-tags* '())
        (*specials* '())
        (*boxed-vars* '())
        (*local-functions* '())
        (*at-toplevel* t)
        (*in-tail-position* t)
        (*macroexpand-cache* (make-hash-table :test #'eq)))
    `(,@(compile-expr expr)
      (:ret))))
