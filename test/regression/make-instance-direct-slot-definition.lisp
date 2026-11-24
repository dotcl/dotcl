;;; AMOP: a class makes its direct slot definitions with MAKE-INSTANCE on the class that
;;; DIRECT-SLOT-DEFINITION-CLASS chose. A MAKE-INSTANCE method of the program for that class
;;; takes the call, and what it returns is the slot definition (hu.dwim.perec hands back one
;;; it made earlier when :INSTANCE is given). Without such a method the direct path stays.

(defclass mdsd-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((a mdsd-meta) (b standard-class)) t)
(defclass mdsd-slot (dotcl-mop:standard-direct-slot-definition) ())
(defmethod dotcl-mop:direct-slot-definition-class ((c mdsd-meta) &rest args &key instance &allow-other-keys)
  (declare (ignore args))
  (if instance (class-of instance) (call-next-method)))
(defmethod make-instance ((c (eql (find-class 'mdsd-slot))) &key instance &allow-other-keys)
  (or instance (call-next-method)))

(defvar *mdsd-instance* (make-instance 'mdsd-slot :name 'a))

(deftest make-instance-direct-slot-definition.instance-is-adopted
  (progn
    (dotcl-mop:ensure-class 'mdsd-1 :metaclass 'mdsd-meta :direct-superclasses nil
                            :direct-slots (list (list :name 'a :persistent t :instance *mdsd-instance*)
                                                (list :name 'b)))
    (let ((slots (dotcl-mop:class-direct-slots (find-class 'mdsd-1))))
      (list (mapcar #'dotcl-mop:slot-definition-name slots)
            (mapcar #'type-of slots)
            (eq (first slots) *mdsd-instance*)
            (mapcar #'dotcl-mop:slot-definition-name (dotcl-mop:class-slots (find-class 'mdsd-1))))))
  ((a b) (mdsd-slot dotcl-mop:standard-direct-slot-definition) t (a b)))

(deftest make-instance-direct-slot-definition.redefinition-keeps-the-instance
  (progn
    (dotcl-mop:ensure-class 'mdsd-2 :metaclass 'mdsd-meta :direct-superclasses nil
                            :direct-slots (list (list :name 'a :persistent t :instance *mdsd-instance*)))
    (dotcl-mop:ensure-class 'mdsd-2 :metaclass 'mdsd-meta :direct-superclasses nil
                            :direct-slots (list (list :name 'a :persistent t :instance *mdsd-instance*)))
    (let ((slots (dotcl-mop:class-direct-slots (find-class 'mdsd-2))))
      (list (length slots) (eq (first slots) *mdsd-instance*))))
  (1 t))

;; No MAKE-INSTANCE method for the chosen class: the ordinary path, and a DEFCLASS slot
;; option the chosen class does not know is still an error.
(defclass mdsd-plain-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((a mdsd-plain-meta) (b standard-class)) t)
(defclass mdsd-plain-slot (dotcl-mop:standard-direct-slot-definition) ((marked :initarg :marked :initform nil)))
(defmethod dotcl-mop:direct-slot-definition-class ((c mdsd-plain-meta) &rest args &key marked &allow-other-keys)
  (declare (ignore args))
  (if marked (find-class 'mdsd-plain-slot) (call-next-method)))

(deftest make-instance-direct-slot-definition.ordinary-path-unchanged
  (progn
    (eval '(defclass mdsd-3 () ((a :initarg :a :accessor mdsd-3-a) (b :marked t)) (:metaclass mdsd-plain-meta)))
    (let ((slots (dotcl-mop:class-direct-slots (find-class 'mdsd-3))))
      (list (mapcar #'type-of slots)
            (mapcar #'dotcl-mop:slot-definition-name slots)
            (mdsd-3-a (make-instance 'mdsd-3 :a 5))
            (length (dotcl-mop:generic-function-methods #'mdsd-3-a)))))
  ((dotcl-mop:standard-direct-slot-definition mdsd-plain-slot) (a b) 5 1))

(deftest make-instance-direct-slot-definition.unknown-option-still-an-error
  (handler-case (progn (eval '(defclass mdsd-4 () ((a :nonsense 1)) (:metaclass mdsd-plain-meta))) :no-error)
    (error () :error))
  :error)

(deftest make-instance-direct-slot-definition.standard-defclass-unchanged
  (progn
    (eval '(defclass mdsd-5 () ((x :initform 1 :initarg :x :reader mdsd-5-x) (y :allocation :class :initform 2))))
    (list (mapcar #'type-of (dotcl-mop:class-direct-slots (find-class 'mdsd-5)))
          (mdsd-5-x (make-instance 'mdsd-5 :x 9))
          (slot-value (make-instance 'mdsd-5) 'y)))
  ((dotcl-mop:standard-direct-slot-definition dotcl-mop:standard-direct-slot-definition) 9 2))
