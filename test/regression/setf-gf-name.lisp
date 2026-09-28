;;; A (SETF accessor) generic function is named by the list (SETF accessor)
;;; wherever its name is visible: GENERIC-FUNCTION-NAME and the third value of
;;; FUNCTION-LAMBDA-EXPRESSION. The name round-trips through FDEFINITION.

(defgeneric (setf sgn-acc) (v x))
(defmethod (setf sgn-acc) (v x) (list v x))

(deftest setf-gf-name.default-class
  (let ((n (dotcl-mop:generic-function-name #'(setf sgn-acc))))
    (list n (eq (fdefinition n) #'(setf sgn-acc))))
  ((setf sgn-acc) t))

(deftest setf-gf-name.function-lambda-expression
  (nth-value 2 (function-lambda-expression #'(setf sgn-acc)))
  (setf sgn-acc))

(defclass sgn-gf (standard-generic-function) ()
  (:metaclass dotcl-mop:funcallable-standard-class))
(defgeneric (setf sgn-acc2) (v x) (:generic-function-class sgn-gf))

(deftest setf-gf-name.user-class
  (dotcl-mop:generic-function-name #'(setf sgn-acc2))
  (setf sgn-acc2))

(deftest setf-gf-name.ensure-generic-function
  (dotcl-mop:generic-function-name
   (ensure-generic-function '(setf sgn-acc3) :lambda-list '(v x)))
  (setf sgn-acc3))

;; A standard one defined by the runtime, not by DEFGENERIC.
(deftest setf-gf-name.class-name
  (dotcl-mop:generic-function-name #'(setf class-name))
  (setf class-name))

(deftest setf-gf-name.plain-symbol-unchanged
  (progn (defgeneric sgn-plain (x))
         (dotcl-mop:generic-function-name #'sgn-plain))
  sgn-plain)
