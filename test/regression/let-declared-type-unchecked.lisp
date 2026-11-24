;;; At the default safety the declared type of a LET / LET* / MULTIPLE-VALUE-BIND /
;;; DESTRUCTURING-BIND variable is not checked against the value being bound (a deliberate
;;; difference from SBCL, docs/deviations.md); a lambda parameter's declaration is checked.
;;; This fixes the present behaviour so that a change of it is a decision, not an accident.

(deftest let-declared-type-unchecked.let
  (list (let ((a "x")) (declare (type fixnum a)) a)
        (funcall (lambda (x) (let ((a x)) (declare (type fixnum a)) a)) "y")
        (funcall (lambda (x) (let ((a x)) (declare (type list a)) a)) 5)
        (funcall (lambda (x) (let ((a x)) (declare (type (integer 0 10) a)) a)) 50))
  ("x" "y" 5 50))

(deftest let-declared-type-unchecked.other-binders
  (list (funcall (lambda (x) (let* ((a x)) (declare (type fixnum a)) a)) "s")
        (funcall (lambda (x) (multiple-value-bind (a) (values x) (declare (type fixnum a)) a)) "m")
        (funcall (lambda (x) (destructuring-bind (a) x (declare (type fixnum a)) a)) '("d")))
  ("s" "m" "d"))

(deftest let-declared-type-unchecked.parameter-is-checked
  (handler-case (funcall (lambda (a) (declare (type fixnum a)) a) "z")
    (type-error () :type-error))
  :type-error)
