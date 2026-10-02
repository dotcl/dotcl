;;; Regression: a NOTINLINE declaration is lexical (CLHS 3.3.4), so it covers
;;; a lambda written inside its scope too. A function proclaimed INLINE and
;;; then redefined with (setf fdefinition) has to be called through the new
;;; definition from such a lambda.
;;;
;;; Every function and lambda body started from an empty NOTINLINE set, so a
;;; declaration made in the enclosing body, or by LOCALLY around it, was lost
;;; at the closure boundary and the old inline expansion was used. cl-mock's
;;; DFLET (a dynamic FLET via setf fdefinition) relies on this.

(declaim (inline nnl-foo))
(defun nnl-foo () 23)

(defun nnl-call-with-foo (thunk)
  (let ((old (fdefinition 'nnl-foo)))
    (setf (fdefinition 'nnl-foo) (lambda () 42))
    (unwind-protect (funcall thunk)
      (setf (fdefinition 'nnl-foo) old))))

;;; Declared in the enclosing function body.
(defun nnl-outer-declare ()
  (declare (notinline nnl-foo))
  (nnl-call-with-foo (lambda () (nnl-foo))))
(deftest notinline-nested-lambda-outer-declare (nnl-outer-declare) 42)

;;; Declared by LOCALLY around the lambda.
(defun nnl-locally ()
  (locally (declare (notinline nnl-foo))
    (nnl-call-with-foo (lambda () (nnl-foo)))))
(deftest notinline-nested-lambda-locally (nnl-locally) 42)

;;; Two levels of lambda.
(defun nnl-two-levels ()
  (declare (notinline nnl-foo))
  (funcall (lambda () (nnl-call-with-foo (lambda () (nnl-foo))))))
(deftest notinline-nested-lambda-two-levels (nnl-two-levels) 42)

;;; Declared in the lambda itself (this always worked).
(defun nnl-inner-declare ()
  (nnl-call-with-foo (lambda () (declare (notinline nnl-foo)) (nnl-foo))))
(deftest notinline-nested-lambda-inner-declare (nnl-inner-declare) 42)
