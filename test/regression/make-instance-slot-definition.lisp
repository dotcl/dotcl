;;; MAKE-INSTANCE of a slot-definition class takes the AMOP initargs (AMOP 5.4.2)
;;; and gives a slot definition. It signalled "Invalid initarg :NAME". McCLIM's
;;; ESA (MODUAL-CLASS) adds a slot to every class of its metaclass this way,
;;; from a COMPUTE-SLOTS method.

(defpackage :misd (:use :cl :dotcl-mop)
  (:shadowing-import-from :dotcl-mop
   #:standard-effective-slot-definition #:standard-direct-slot-definition))
(in-package :misd)

(defclass extra-slot-class (standard-class) ())
(defmethod validate-superclass ((c extra-slot-class) (s standard-class)) t)
(defmethod compute-slots ((c extra-slot-class))
  (append (call-next-method)
          (list (make-instance 'standard-effective-slot-definition
                               :name '%extra :allocation :instance
                               :documentation "Added by the metaclass."))))
(defclass with-extra () ((a :initarg :a :reader with-extra-a))
  (:metaclass extra-slot-class))

(defclass tagged-direct-slot (standard-direct-slot-definition)
  ((tag :initarg :tag :initform :none :reader slot-tag)))
(in-package :cl-user)

(deftest make-instance-slot-definition.accessors
  (let ((s (make-instance 'misd::standard-effective-slot-definition
                         :name 'x :initargs '(:x) :type 'fixnum
                         :initform 1 :initfunction (lambda () 1))))
    (list (misd::slot-definition-name s) (misd::slot-definition-initargs s)
          (misd::slot-definition-type s) (misd::slot-definition-initform s)
          (funcall (misd::slot-definition-initfunction s))
          (misd::slot-definition-allocation s)
          (typep s 'misd::standard-effective-slot-definition)))
  (x (:x) fixnum 1 1 :instance t))

(deftest make-instance-slot-definition.direct
  (let ((s (make-instance 'misd::standard-direct-slot-definition
                         :name 'y :readers '(get-y) :allocation :class)))
    (list (misd::slot-definition-readers s) (misd::slot-definition-allocation s)
          (typep s 'misd::standard-direct-slot-definition)))
  ((get-y) :class t))

(deftest make-instance-slot-definition.compute-slots
  (let ((i (make-instance 'misd::with-extra :a 1)))
    (setf (slot-value i 'misd::%extra) 5)
    (list (misd::with-extra-a i) (slot-value i 'misd::%extra)
          (mapcar #'misd::slot-definition-name
                  (misd::class-slots (find-class 'misd::with-extra)))))
  (1 5 (misd::a misd::%extra)))

(deftest make-instance-slot-definition.subclass
  (let ((s (make-instance 'misd::tagged-direct-slot :name 'z :tag :t1))
        (d (make-instance 'misd::tagged-direct-slot :name 'z)))
    (list (misd::slot-tag s) (misd::slot-tag d) (class-name (class-of s))))
  (:t1 :none misd::tagged-direct-slot))
