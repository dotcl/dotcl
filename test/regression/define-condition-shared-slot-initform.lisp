;;; A shared slot's initform is evaluated when the class is defined.

(defvar *shared-initform-timing* 0)
(defvar *shared-initform-where* :definition)

(define-condition shared-initform-condition (condition)
  ((v :initform (incf *shared-initform-timing*)
      :allocation :class
      :reader shared-initform-condition-v)))

(define-condition shared-initform-environment (condition)
  ((v :initform *shared-initform-where*
      :allocation :class
      :reader shared-initform-environment-v)))

(defvar *defclass-shared-initform-timing* 0)

(defclass shared-initform-class ()
  ((v :initform (incf *defclass-shared-initform-timing*)
      :allocation :class
      :reader shared-initform-class-v)))

(deftest define-condition-shared-slot-initform.definition-time
  *shared-initform-timing*
  1)

(deftest define-condition-shared-slot-initform.not-reevaluated
  (let ((before *shared-initform-timing*))
    (make-condition 'shared-initform-condition)
    (make-condition 'shared-initform-condition)
    (list before *shared-initform-timing*))
  (1 1))

(deftest define-condition-shared-slot-initform.dynamic-environment
  (let ((*shared-initform-where* :instantiation))
    (shared-initform-environment-v (make-condition 'shared-initform-environment)))
  :definition)

(deftest defclass-shared-slot-initform.definition-time
  *defclass-shared-initform-timing*
  1)
