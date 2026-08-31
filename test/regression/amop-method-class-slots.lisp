;;; A user-defined method class can add slots, and they behave like slots.
;;;
;;; A method is not a LispInstance and has no slot vector, so the slots its class
;;; adds live in the same ExtraSlots escape hatch that class, generic function and
;;; slot-definition metaobjects use. Without it CLASS-OF answered the right class
;;; while SLOT-VALUE signalled a type error -- metaobject code that hangs its own
;;; information on a method had nowhere to put it.

(defclass amcs-method (standard-method)
  ((tag :initarg :tag :accessor amcs-tag :initform :untagged)))

(defclass amcs-gf (standard-generic-function) ()
  (:metaclass dotcl-mop:funcallable-standard-class))

(defmethod dotcl-mop:generic-function-method-class ((gf amcs-gf))
  (find-class 'amcs-method))

(defgeneric amcs-double (x) (:generic-function-class amcs-gf))
(defmethod amcs-double ((x integer)) (* x 2))

(defun amcs-the-method ()
  (first (dotcl-mop:generic-function-methods #'amcs-double)))

;;; DEFMETHOD asked the generic function for its method class, so the method is one.

(deftest amop-method-class-slots.class-of
  (class-name (class-of (amcs-the-method)))
  amcs-method)

;;; The method is an instance of that class, so the initialization protocol ran on
;;; the slots it adds. DEFMETHOD passes no initargs for them, so the initform is
;;; what fills them.

(deftest amop-method-class-slots.initform-ran
  (list (slot-boundp (amcs-the-method) 'tag)
        (slot-value (amcs-the-method) 'tag))
  (t :untagged))

(deftest amop-method-class-slots.setf-and-read-back
  (progn
    (setf (slot-value (amcs-the-method) 'tag) :written)
    (list (slot-value (amcs-the-method) 'tag)
          (amcs-tag (amcs-the-method))))
  (:written :written))

;;; The dispatch the method backs is unaffected.

(deftest amop-method-class-slots.still-dispatches
  (amcs-double 21)
  42)

;;; MAKE-INSTANCE of the method class: CLASS-OF answers it, and an initarg for one
;;; of the added slots is applied.

(defparameter *amcs-made*
  (make-instance 'amcs-method
                 :qualifiers '()
                 :lambda-list '(x)
                 :specializers (list (find-class 'integer))
                 :tag :made
                 :function (lambda (args next-methods)
                             (declare (ignore next-methods))
                             (+ (first args) 1))))

(deftest amop-method-class-slots.make-instance-class-of
  (class-name (class-of *amcs-made*))
  amcs-method)

(deftest amop-method-class-slots.make-instance-initarg
  (slot-value *amcs-made* 'tag)
  :made)

;;; ...and it is still a method: ADD-METHOD takes it and it dispatches.

(deftest amop-method-class-slots.made-method-is-usable
  (progn
    (add-method #'amcs-double *amcs-made*)
    (list (amcs-double 41)
          (slot-value (find-if (lambda (m) (eq m *amcs-made*))
                               (dotcl-mop:generic-function-methods #'amcs-double))
                      'tag)))
  (42 :made))

;;; An unbound added slot signals UNBOUND-SLOT rather than a type error.

(defclass amcs-method-2 (standard-method)
  ((note :initarg :note :accessor amcs-note)))

(deftest amop-method-class-slots.unbound-added-slot
  (let ((m (make-instance 'amcs-method-2
                          :qualifiers '()
                          :lambda-list '(x)
                          :specializers (list (find-class 'integer))
                          :function (lambda (args next-methods)
                                      (declare (ignore args next-methods))
                                      nil))))
    (list (slot-boundp m 'note)
          (handler-case (slot-value m 'note)
            (unbound-slot () :unbound-slot)
            (error () :wrong-condition))))
  (nil :unbound-slot))
