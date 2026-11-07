;;; COMPUTE-EFFECTIVE-SLOT-DEFINITION hands EFFECTIVE-SLOT-DEFINITION-CLASS the
;;; slot's initargs (AMOP): the merged standard ones and the other options of the
;;; direct slot definitions. A slot option that is not an initarg of the direct
;;; slot definition class is an invalid initialization argument.

(defpackage :esdi (:use :cl :dotcl-mop))
(in-package :esdi)

(defvar *esdc-args* nil)

(defclass opt-dsd (standard-direct-slot-definition)
  ((col :initarg :col :initform nil :reader dsd-col)))
(defclass opt-esd (standard-effective-slot-definition)
  ((col :initarg :col :initform :none :reader esd-col)))

(defclass opt-meta (standard-class) ())
(defmethod validate-superclass ((c opt-meta) (s standard-class)) t)
(defmethod direct-slot-definition-class ((c opt-meta) &rest initargs)
  (declare (ignore initargs))
  (find-class 'opt-dsd))
(defmethod effective-slot-definition-class ((c opt-meta) &rest initargs)
  (setf *esdc-args* (copy-list initargs))
  (find-class 'opt-esd))

(defclass opt-row ()
  ((a :initarg :a :initform 3 :documentation "doc" :col "A"))
  (:metaclass opt-meta))
(in-package :cl-user)

(deftest effective-slotd-initargs.passed
  (let ((p esdi::*esdc-args*))
    (list (getf p :name) (getf p :initargs) (getf p :initform)
          (funcall (getf p :initfunction)) (getf p :documentation)
          (getf p :col) (getf p :allocation)))
  (esdi::a (:a) 3 3 "doc" "A" :instance))

(deftest effective-slotd-initargs.extra-slot-set
  (esdi::esd-col (first (esdi::class-slots (find-class 'esdi::opt-row))))
  "A")

(deftest effective-slotd-initargs.unknown-option-errors
  (handler-case
      (progn (eval '(defclass esdi::bad-row ()
                     ((a :bogus 1))
                     (:metaclass esdi::opt-meta)))
             :no-error)
    (program-error () :program-error))
  :program-error)
