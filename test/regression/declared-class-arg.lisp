;;; A parameter declared with the type of a standard class accepts an instance of
;;; that class and of a subclass, and signals TYPE-ERROR for anything else.

(defclass dca-base () ((x :initarg :x :accessor dca-x)))
(defclass dca-sub (dca-base) ())
(defclass dca-other () ((x :initarg :x :accessor dca-other-x)))

(defun dca-read (p) (declare (type dca-base p)) (dca-x p))

(deftest declared-class-arg.exact
  (dca-read (make-instance 'dca-base :x 1))
  1)

(deftest declared-class-arg.subclass
  (dca-read (make-instance 'dca-sub :x 2))
  2)

(deftest declared-class-arg.repeated
  (let ((o (make-instance 'dca-base :x 3)))
    (dca-read o)
    (dca-read o))
  3)

(deftest declared-class-arg.other-class
  (handler-case (dca-read (make-instance 'dca-other :x 4))
    (type-error () :type-error))
  :type-error)

(deftest declared-class-arg.non-instance
  (handler-case (dca-read 5)
    (type-error () :type-error))
  :type-error)

;; A class replaced under the same name: an instance of the old class is not of the new one.
(defclass dca-qux () ())
(defun dca-take-qux (x) (declare (dca-qux x)) :ok)

(deftest declared-class-arg.class-replaced-under-its-name
  (let ((old (make-instance 'dca-qux)))
    (list (dca-take-qux old)
          (progn (setf (find-class 'dca-qux) (make-instance 'standard-class :name 'dca-qux))
                 (handler-case (dca-take-qux old) (type-error () :type-error)))
          (dca-take-qux (make-instance (find-class 'dca-qux)))))
  (:ok :type-error :ok))

(defclass dca-redef () ((a :initarg :a)))
(defun dca-take-redef (x) (declare (dca-redef x)) :ok)

(deftest declared-class-arg.redefined-class-still-passes
  (let ((old (make-instance 'dca-redef :a 1)))
    (eval '(defclass dca-redef () ((a :initarg :a) (b :initform 2))))
    (list (dca-take-redef old) (dca-take-redef (make-instance 'dca-redef))))
  (:ok :ok))
