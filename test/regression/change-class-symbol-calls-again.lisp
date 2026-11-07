;;; (change-class instance 'name) calls CHANGE-CLASS again with the class object (CLHS),
;;; so a method specialised on the class runs as well as one on the symbol. A call with
;;; the class object runs the class method once, and the slots are carried over as before.

(defclass ccsa-a () ((a :initform 1 :accessor ccsa-a-a)))
(defclass ccsa-b () ((a :initform 5) (b :initform 2 :accessor ccsa-b-b)))
(defvar *ccsa-log* nil)

(deftest change-class-symbol-calls-again.symbol-reaches-class-methods
  (let ((m1 (eval '(defmethod change-class :before ((o ccsa-a) (c symbol) &rest r)
                     (declare (ignore c r)) (push :symbol *ccsa-log*))))
        (m2 (eval '(defmethod change-class :before ((o ccsa-a) (c standard-class) &rest r)
                     (declare (ignore c r)) (push :class *ccsa-log*)))))
    (setf *ccsa-log* nil)
    (unwind-protect
         (let ((o (make-instance 'ccsa-a)))
           (change-class o 'ccsa-b)
           (list (reverse *ccsa-log*) (type-of o) (ccsa-b-b o)))
      (remove-method #'change-class m1)
      (remove-method #'change-class m2)))
  ((:symbol :class) ccsa-b 2))

(deftest change-class-symbol-calls-again.class-object-once
  (let ((m2 (eval '(defmethod change-class :before ((o ccsa-a) (c standard-class) &rest r)
                     (declare (ignore c r)) (push :class *ccsa-log*)))))
    (setf *ccsa-log* nil)
    (unwind-protect
         (let ((o (make-instance 'ccsa-a)))
           (change-class o (find-class 'ccsa-b))
           (list *ccsa-log* (type-of o)))
      (remove-method #'change-class m2)))
  ((:class) ccsa-b))

(deftest change-class-symbol-calls-again.plain
  (let ((o (make-instance 'ccsa-a)))
    (setf (ccsa-a-a o) 9)
    (change-class o 'ccsa-b)
    (list (type-of o) (slot-value o 'a) (slot-value o 'b)))
  (ccsa-b 9 2))

(deftest change-class-symbol-calls-again.unknown-class-still-errors
  (handler-case (progn (change-class (make-instance 'ccsa-a) 'ccsa-no-such-class) :no-error)
    (error () :error))
  :error)
