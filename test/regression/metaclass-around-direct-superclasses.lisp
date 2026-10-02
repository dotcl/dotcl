;;; MAKE-INSTANCE of a metaclass with :DIRECT-SUPERCLASSES NIL passes the empty
;;; list to INITIALIZE-INSTANCE as given. It used to be replaced by
;;; (STANDARD-OBJECT) first, so a metaclass's INITIALIZE-INSTANCE :AROUND that
;;; appends a superclass of its own (ContextL's SPECIAL-CLASS appends
;;; SPECIAL-OBJECT) produced (STANDARD-OBJECT SPECIAL-OBJECT) and the class
;;; precedence list could not be computed. ContextL's DEFINE-LAYERED-CLASS
;;; failed this way. With the key absent, the initargs still carry
;;; (STANDARD-OBJECT), as in SBCL (metaclass-direct-superclasses.lisp).

(defclass mad-object () ())
(defclass mad-class (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mad-class) (s standard-class)) t)
(defvar *mad-seen* :unset)
(defmethod initialize-instance :around ((class mad-class) &rest initargs
                                        &key direct-superclasses)
  (setf *mad-seen* (mapcar #'class-name direct-superclasses))
  (apply #'call-next-method class
         :direct-superclasses (append direct-superclasses (list (find-class 'mad-object)))
         initargs))

(deftest metaclass-around-direct-superclasses.seen-as-given
  (progn (make-instance 'mad-class :direct-superclasses nil) *mad-seen*)
  nil)

(deftest metaclass-around-direct-superclasses.appended
  (let ((c (make-instance 'mad-class :direct-superclasses nil)))
    (dotcl-mop:finalize-inheritance c)
    (mapcar #'class-name (dotcl-mop:class-precedence-list c)))
  (nil mad-object standard-object t))

(deftest metaclass-around-direct-superclasses.standard-default
  (let ((c (make-instance 'standard-class :direct-superclasses nil)))
    (mapcar #'class-name (dotcl-mop:class-direct-superclasses c)))
  (standard-object))
