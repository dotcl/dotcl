;;; A slot option is a valid initarg of the direct slot definition when a method of
;;; INITIALIZE-INSTANCE / SHARED-INITIALIZE applicable to it takes the keyword
;;; (CLHS 7.1.2), even though no slot of the slot definition class has it as an
;;; initarg. Postmodern's DAO classes use this: (col-name aa) is a keyword
;;; parameter of a SHARED-INITIALIZE :AFTER method on its direct slot class.

(defpackage :soik (:use :cl :dotcl-mop))
(in-package :soik)

(defclass key-dsd (standard-direct-slot-definition)
  ((seen :initform :none :accessor dsd-seen)))
(defmethod shared-initialize :after ((slot key-dsd) slot-names
                                     &key (col-name nil col-name-p) &allow-other-keys)
  (declare (ignorable slot-names))
  (setf (dsd-seen slot) (if col-name-p (list :col-name col-name) :absent)))

(defclass key-meta (standard-class) ())
(defmethod validate-superclass ((c key-meta) (s standard-class)) t)
(defmethod direct-slot-definition-class ((c key-meta) &rest initargs)
  (declare (ignore initargs))
  (find-class 'key-dsd))

(defclass key-row ()
  ((a :col-name aa :initarg :a)
   (b :initarg :b))
  (:metaclass key-meta))

;; only a keyword parameter, no &allow-other-keys
(defclass only-dsd (standard-direct-slot-definition) ((seen :initform :none :accessor only-seen)))
(defmethod initialize-instance :after ((slot only-dsd) &key (tag nil tag-p) &allow-other-keys)
  (setf (only-seen slot) (if tag-p (list :tag tag) :absent)))
(defclass only-meta (standard-class) ())
(defmethod validate-superclass ((c only-meta) (s standard-class)) t)
(defmethod direct-slot-definition-class ((c only-meta) &rest initargs)
  (declare (ignore initargs))
  (find-class 'only-dsd))
(defclass only-row () ((a :tag 7 :initarg :a)) (:metaclass only-meta))

;; a class with no such method keeps rejecting an unknown option
(defclass plain-dsd (standard-direct-slot-definition) ())
(defclass plain-meta (standard-class) ())
(defmethod validate-superclass ((c plain-meta) (s standard-class)) t)
(defmethod direct-slot-definition-class ((c plain-meta) &rest initargs)
  (declare (ignore initargs))
  (find-class 'plain-dsd))
(in-package :cl-user)

(deftest slot-option-init-keys.shared-initialize-key-accepted
  (mapcar #'soik::dsd-seen (soik::class-direct-slots (find-class 'soik::key-row)))
  ((:col-name soik::aa) :absent))

(deftest slot-option-init-keys.initialize-instance-key-accepted
  (mapcar #'soik::only-seen (soik::class-direct-slots (find-class 'soik::only-row)))
  ((:tag 7)))

(deftest slot-option-init-keys.unknown-option-still-errors
  (handler-case
      (progn (eval '(defclass soik::bad-row () ((a :bogus 1)) (:metaclass soik::plain-meta)))
             :no-error)
    (program-error () :program-error))
  :program-error)
