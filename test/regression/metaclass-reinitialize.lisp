;;; Redefining a class under a custom metaclass reinitializes the existing class
;;; metaobject: the metaclass's REINITIALIZE-INSTANCE methods run (with
;;; :DIRECT-SUPERCLASSES, :DIRECT-SLOTS and :DIRECT-DEFAULT-INITARGS) and
;;; INITIALIZE-INSTANCE does not. It used to build a fresh class, run
;;; INITIALIZE-INSTANCE on it and copy it into the old one, so a metaclass that
;;; treats the two differently (clsql's view classes: the root class must not be
;;; made its own superclass) broke on the second load. REINITIALIZE-INSTANCE with
;;; new :DIRECT-SUPERCLASSES on a finalized class used to be ignored. Expected
;;; values were checked against SBCL.

(defclass mri-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((a mri-meta) (b standard-class)) t)

(defvar *mri-log* nil)
(defmethod initialize-instance :around ((c mri-meta) &rest initargs)
  (push (list :ii (getf initargs :name)) *mri-log*)
  (call-next-method))
(defmethod reinitialize-instance :around ((c mri-meta) &rest initargs)
  (push (list :ri (class-name c)
              (loop for k in initargs by #'cddr
                    when (member k '(:name :direct-superclasses :direct-slots
                                     :direct-default-initargs))
                      collect k))
        *mri-log*)
  (call-next-method))

(deftest metaclass-reinitialize.redefine
  (progn
    (setf *mri-log* nil)
    (defclass mri-c1 () ((a :initarg :a)) (:metaclass mri-meta))
    (let ((c1 (find-class 'mri-c1)))
      (defclass mri-c1 () ((a :initarg :a) (b :initarg :b :initform 2)) (:metaclass mri-meta)
        (:default-initargs :a 1))
      (let ((o (make-instance 'mri-c1)))
        (list (reverse *mri-log*)
              (eq c1 (find-class 'mri-c1))
              (slot-value o 'a) (slot-value o 'b)))))
  (((:ii mri-c1) (:ri mri-c1 (:direct-superclasses :direct-slots :direct-default-initargs)))
   t 1 2))

;;; The clsql pattern: every class of the metaclass gets a root superclass added,
;;; except the root itself -- which INITIALIZE-INSTANCE cannot tell apart from a
;;; new class of the same name, but REINITIALIZE-INSTANCE can.
(defclass mri-root-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((a mri-root-meta) (b standard-class)) t)
(defun mri-add-root (class next initargs direct-superclasses)
  (let ((root (find-class 'mri-root nil)))
    (if (and root (not (eq class root)) (not (member root direct-superclasses)))
        (apply next class :direct-superclasses (cons root direct-superclasses) initargs)
        (funcall next))))
(defmethod initialize-instance :around ((c mri-root-meta) &rest initargs
                                        &key direct-superclasses)
  (mri-add-root c (lambda (&rest args) (if args (apply #'call-next-method args) (call-next-method)))
                initargs direct-superclasses))
(defmethod reinitialize-instance :around ((c mri-root-meta) &rest initargs
                                          &key direct-superclasses)
  (mri-add-root c (lambda (&rest args) (if args (apply #'call-next-method args) (call-next-method)))
                initargs direct-superclasses))

(deftest metaclass-reinitialize.root-class
  (progn
    (defclass mri-root () () (:metaclass mri-root-meta))
    (defclass mri-root () () (:metaclass mri-root-meta))
    (defclass mri-view () ((x :initarg :x)) (:metaclass mri-root-meta))
    (defclass mri-view () ((x :initarg :x)) (:metaclass mri-root-meta))
    (list (mapcar #'class-name (dotcl-mop:class-direct-superclasses (find-class 'mri-root)))
          (mapcar #'class-name (dotcl-mop:class-direct-superclasses (find-class 'mri-view)))
          (typep (make-instance 'mri-view :x 1) 'mri-root)))
  ((standard-object) (mri-root) t))

;;; REINITIALIZE-INSTANCE of a finalized class with new superclasses.
(defclass mri-s1 () ((s1 :initform 1)))
(defclass mri-s2 () ((s2 :initform 2)))
(defclass mri-sub () ())
(defclass mri-subsub (mri-sub) ())

(deftest metaclass-reinitialize.change-superclasses
  (progn
    (make-instance 'mri-subsub)
    (reinitialize-instance (find-class 'mri-sub)
                           :direct-superclasses (list (find-class 'mri-s1) (find-class 'mri-s2)))
    (let ((o (make-instance 'mri-subsub)))
      (list (mapcar #'class-name (dotcl-mop:class-direct-superclasses (find-class 'mri-sub)))
            (slot-value o 's1) (slot-value o 's2)
            (typep o 'mri-s2)
            (not (null (member (find-class 'mri-sub)
                               (dotcl-mop:class-direct-subclasses (find-class 'mri-s1))))))))
  ((mri-s1 mri-s2) 1 2 t t))

(deftest metaclass-reinitialize.change-superclasses-custom-metaclass
  (progn
    (defclass mri-c2 () () (:metaclass mri-meta))
    (make-instance 'mri-c2)
    (reinitialize-instance (find-class 'mri-c2) :direct-superclasses (list (find-class 'mri-s1)))
    (slot-value (make-instance 'mri-c2) 's1))
  1)
