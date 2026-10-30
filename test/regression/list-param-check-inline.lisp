;;; A LIST, CONS or NULL argument declaration is checked by an open-coded test
;;; (NIL identity plus a CONS type test), not by a call into a runtime helper.
;;; The TYPE-ERROR on failure is unchanged.

(setf dotcl:*save-sil* t)

(defun %lpci-outcome (thunk)
  (handler-case (list :value (funcall thunk))
    (type-error (e) (list :type-error (type-error-datum e)
                          (type-error-expected-type e)))))

(defun %lpci-list (x) (declare (type list x)) x)
(defun %lpci-cons (x) (declare (type cons x)) x)
(defun %lpci-null (x) (declare (type null x)) x)

(defun %lpci-inline-let (n)
  (flet ((f (k) (declare (type list k)) k))
    (declare (inline f))
    (f n)))

(defun %lpci-sil (fn) (%flat-text (dotcl:function-sil fn)))
(defun %lpci-has (needle fn) (and (search needle (%lpci-sil fn)) t))

(defun %lpci-both (form &rest args)
  (flet ((run (mode)
           (let ((dotcl:*evaluator-mode* mode))
             (%lpci-outcome (lambda () (apply (eval form) args))))))
    (list (run :compile) (run :interpret))))

(deftest list-param-check-inline.values
  (list (%lpci-list nil) (%lpci-list '(1)) (%lpci-cons '(1))
        (%lpci-null nil) (%lpci-inline-let '(2 3)))
  (nil (1) (1) nil (2 3)))

(deftest list-param-check-inline.wrong-type
  (list (%lpci-outcome (lambda () (%lpci-list 3)))
        (%lpci-outcome (lambda () (%lpci-cons 3)))
        (%lpci-outcome (lambda () (%lpci-cons nil)))
        (%lpci-outcome (lambda () (%lpci-null 3)))
        (%lpci-outcome (lambda () (%lpci-inline-let 3))))
  ((:type-error 3 list) (:type-error 3 cons) (:type-error nil cons)
   (:type-error 3 null) (:type-error 3 list)))

(deftest list-param-check-inline.both-evaluators
  (list (%lpci-both '(lambda (x) (declare (type list x)) x) nil)
        (%lpci-both '(lambda (x) (declare (type list x)) x) 3)
        (%lpci-both '(lambda (x) (declare (type cons x)) x) 3)
        (%lpci-both '(lambda (x) (declare (type null x)) x) 3))
  (((:value nil) (:value nil))
   ((:type-error 3 list) (:type-error 3 list))
   ((:type-error 3 cons) (:type-error 3 cons))
   ((:type-error 3 null) (:type-error 3 null))))

(defun %lpci-safety-0 (x)
  (declare (optimize (safety 0)) (type list x))
  x)

(deftest list-param-check-inline.safety-0-trusts
  (%lpci-safety-0 3)
  3)

(deftest-emitting-only list-param-check-inline.sil
  (list (%lpci-has "Runtime.CheckArgList" #'%lpci-list)
        (%lpci-has "Runtime.CheckArgCons" #'%lpci-cons)
        (%lpci-has "Runtime.CheckArgType" #'%lpci-null)
        (%lpci-has "Runtime.CheckArgList" #'%lpci-inline-let)
        (%lpci-has "ISINST Cons" #'%lpci-list)
        (%lpci-has "ISINST Cons" #'%lpci-cons)
        (%lpci-has "Nil.Instance" #'%lpci-list)
        (%lpci-has "Nil.Instance" #'%lpci-null)
        (%lpci-has "Runtime.ArgTypeError" #'%lpci-list))
  (nil nil nil nil t t t t t))

(deftest-emitting-only list-param-check-inline.safety-0-no-check
  (%lpci-has "Runtime.ArgTypeError" #'%lpci-safety-0)
  nil)
