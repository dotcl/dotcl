;;; A slot definition of a user-defined slot-definition class goes through
;;; INITIALIZE-INSTANCE, as AMOP has it, both when DEFCLASS makes it (the
;;; metaclass's DIRECT-SLOT-DEFINITION-CLASS names the class) and when
;;; MAKE-INSTANCE does. Methods on it run, and an :around that rewrites the
;;; initargs with CALL-NEXT-METHOD changes what the slot definition gets.
;;; clsql's view classes store the slot's declared type this way and replace
;;; :type with a Lisp type computed from it.

(defpackage :sdii (:use :cl :dotcl-mop))
(in-package :sdii)

(defclass tagged-dsd (standard-direct-slot-definition)
  ((spec :initarg spec :accessor spec :initform :unset)
   (column :initarg :column :accessor column :initform nil)
   (seen-after :accessor seen-after :initform nil)))

(defvar *around-calls* 0)

(defmethod initialize-instance :around ((o tagged-dsd) &rest initargs &key type &allow-other-keys)
  (incf *around-calls*)
  (apply #'call-next-method o 'spec type :type (if (consp type) (car type) type) initargs))

(defmethod initialize-instance :after ((o tagged-dsd) &key)
  (setf (seen-after o) (list (slot-definition-type o) (spec o))))

(defclass tagged-class (standard-class) ())
(defmethod validate-superclass ((c tagged-class) (s standard-class)) t)
(defmethod direct-slot-definition-class ((c tagged-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'tagged-dsd))

(defclass row ()
  ((name :type (string 30) :initarg :name :column "NAME")
   (id :type integer :initarg :id))
  (:metaclass tagged-class))

(cl-user::deftest slot-definition-initialize-instance-defclass
  (let ((slots (class-direct-slots (find-class 'row))))
    (mapcar (lambda (d)
              (list (slot-definition-name d) (slot-definition-type d) (spec d)
                    (column d) (seen-after d)))
            slots))
  ((name string (string 30) "NAME" (string (string 30)))
   (id integer integer nil (integer integer))))

(cl-user::deftest slot-definition-initialize-instance-instances-still-work
  (let ((r (make-instance 'row :name "x" :id 3)))
    (list (slot-value r 'name) (slot-value r 'id)))
  ("x" 3))

(cl-user::deftest slot-definition-initialize-instance-make-instance
  (let ((d (make-instance 'tagged-dsd :name 'extra :type '(simple-array fixnum) :column "X")))
    (list (slot-definition-name d) (slot-definition-type d) (spec d) (column d)
          (seen-after d) (plusp *around-calls*)))
  (extra simple-array (simple-array fixnum) "X" (simple-array (simple-array fixnum)) t))
(in-package :cl-user)
