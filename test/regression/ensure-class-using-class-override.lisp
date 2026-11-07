;;; DEFCLASS and ENSURE-CLASS go through the generic function
;;; ENSURE-CLASS-USING-CLASS (AMOP), so a method of the program on it runs for a
;;; class of a custom metaclass: with NIL as the class for a name not defined yet,
;;; and with the class for a redefinition. A method that changes the arguments
;;; changes the class; classes without such a method are made as before.

(defpackage :ecuco (:use :cl :dotcl-mop))
(in-package :ecuco)

(defvar *log* nil)
(defclass meta (standard-class) ())
(defmethod validate-superclass ((c meta) (s standard-class)) t)

(defmethod ensure-class-using-class :before ((class null) name &rest initargs &key metaclass &allow-other-keys)
  (declare (ignore initargs))
  (when (and metaclass (eq (class-name (if (symbolp metaclass) (find-class metaclass) metaclass)) 'meta))
    (push (list :new name) *log*)))

(defmethod ensure-class-using-class :before ((class meta) name &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (push (list :redefine name) *log*))

(defun run (thunk)
  (setq *log* nil)
  (let ((r (funcall thunk)))
    (list r (reverse *log*))))
(in-package :cl-user)

(deftest ensure-class-using-class-override.defclass-new-and-redefined
  (ecuco::run (lambda ()
                (eval '(defclass ecuco::thing () ((a :initarg :a :initform 1 :reader ecuco::thing-a))
                         (:metaclass ecuco::meta)))
                (eval '(defclass ecuco::thing () ((a :initarg :a :initform 2 :reader ecuco::thing-a) (b :initarg :b))
                         (:metaclass ecuco::meta)))
                (ecuco::thing-a (make-instance 'ecuco::thing))))
  (2 ((:new ecuco::thing) (:redefine ecuco::thing))))

(deftest ensure-class-using-class-override.ensure-class
  (ecuco::run (lambda ()
                (let ((c (dotcl-mop:ensure-class 'ecuco::made :metaclass 'ecuco::meta
                                                              :direct-superclasses '(standard-object)
                                                              :direct-slots '((:name ecuco::x :initargs (:x))))))
                  (list (class-name c) (typep c 'ecuco::meta)
                        (slot-value (make-instance c :x 4) 'ecuco::x)))))
  ((ecuco::made t 4) ((:new ecuco::made))))

(deftest ensure-class-using-class-override.arguments-can-change
  (progn
    (defmethod dotcl-mop:ensure-class-using-class :around ((class null) name &rest initargs &key direct-slots &allow-other-keys)
      (if (eq name 'ecuco::extra-slot-class)
          (apply #'call-next-method class name
                 :direct-slots (append direct-slots '((:name ecuco::added :initargs (:added))))
                 (loop for (k v) on initargs by #'cddr
                       unless (eq k :direct-slots) append (list k v)))
          (call-next-method)))
    (eval '(defclass ecuco::extra-slot-class () ((p :initarg :p)) (:metaclass ecuco::meta)))
    (mapcar #'dotcl-mop:slot-definition-name
            (dotcl-mop:class-slots (find-class 'ecuco::extra-slot-class))))
  (p ecuco::added))

(deftest ensure-class-using-class-override.plain-classes-unaffected
  (ecuco::run (lambda ()
                (eval '(defclass ecuco::plain () ((a :initform 3 :reader ecuco::plain-a))))
                (ecuco::plain-a (make-instance 'ecuco::plain))))
  (3 nil))

;;; The methods above are for this file's classes; take them off the generic
;;; function so that no class made later goes through them.
(dolist (m (copy-list (dotcl-mop:generic-function-methods #'dotcl-mop:ensure-class-using-class)))
  (let ((first (first (dotcl-mop:method-specializers m))))
    (when (or (eq first (find-class 'null)) (eq first (find-class 'ecuco::meta)))
      (remove-method #'dotcl-mop:ensure-class-using-class m))))
