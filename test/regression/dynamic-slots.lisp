;;; Custom slot-definition metaobject protocol.
;;; A reduced version of McCLIM's class-with-dynamic-slots metaclass, exercising
;;; direct-/effective-slot-definition-class, compute-effective-slot-definition,
;;; the allocate-instance override, and slot-value-using-class dispatch on a
;;; custom effective slot whose backing store is a per-instance "dynamic variable".

(defvar *dynslot-dvar-type* nil)

(defstruct dynslot-box (value :unbound) (bound nil))

(defclass dynslot-direct (dotcl-mop:standard-direct-slot-definition)
  ((dynamic :initform nil :initarg :dynamic :reader dynslot-dvar-type)))

(defclass dynslot-effective (dotcl-mop:standard-effective-slot-definition)
  ((dynamic :initform *dynslot-dvar-type* :reader dynslot-dvar-type)))

(defclass dynslot-metaclass (standard-class) ())

(defmethod dotcl-mop:validate-superclass
    ((c dynslot-metaclass) (s standard-class))
  t)

(defmethod dotcl-mop:direct-slot-definition-class
    ((class dynslot-metaclass) &rest initargs)
  (loop for (key) on initargs by #'cddr
        when (eq key :dynamic)
          do (return-from dotcl-mop:direct-slot-definition-class
               (find-class 'dynslot-direct)))
  (call-next-method))

(defmethod dotcl-mop:compute-effective-slot-definition
    ((class dynslot-metaclass) name direct-slotds)
  (declare (ignore name))
  (let ((latest (first direct-slotds)))
    (if (typep latest 'dynslot-direct)
        (let ((*dynslot-dvar-type* (dynslot-dvar-type latest)))
          (call-next-method))
        (call-next-method))))

(defmethod dotcl-mop:effective-slot-definition-class
    ((class dynslot-metaclass) &rest initargs)
  (declare (ignore initargs))
  (if *dynslot-dvar-type*
      (find-class 'dynslot-effective)
      (call-next-method)))

(defun dynslot-box-of (object slotd)
  (dotcl-mop:standard-instance-access
   object (dotcl-mop:slot-definition-location slotd)))

(defmethod dotcl-mop:slot-value-using-class
    ((class standard-class) object (slotd dynslot-effective))
  (let ((b (dynslot-box-of object slotd)))
    (if (dynslot-box-bound b)
        (dynslot-box-value b)
        (slot-unbound class object (dotcl-mop:slot-definition-name slotd)))))

(defmethod (setf dotcl-mop:slot-value-using-class)
    (new-value (class standard-class) object (slotd dynslot-effective))
  (let ((b (dynslot-box-of object slotd)))
    (setf (dynslot-box-value b) new-value
          (dynslot-box-bound b) t)
    new-value))

(defmethod dotcl-mop:slot-boundp-using-class
    ((class standard-class) object (slotd dynslot-effective))
  (dynslot-box-bound (dynslot-box-of object slotd)))

(defmethod dotcl-mop:slot-makunbound-using-class
    ((class standard-class) object (slotd dynslot-effective))
  (setf (dynslot-box-bound (dynslot-box-of object slotd)) nil)
  object)

(defmethod allocate-instance ((class dynslot-metaclass) &rest initargs)
  (declare (ignore initargs))
  (let ((object (call-next-method)))
    (loop for slotd in (dotcl-mop:class-slots class)
          when (typep slotd 'dynslot-effective) do
            (setf (dotcl-mop:standard-instance-access
                   object (dotcl-mop:slot-definition-location slotd))
                  (make-dynslot-box)))
    object))

(defclass dynslot-widget ()
  ((x :dynamic t :accessor dynslot-x)
   (y :initarg :y :initform 99 :accessor dynslot-y)
   ;; A dynamic slot WITH an initform — the McCLIM stream-recording-p case.
   (z :dynamic t :initform 5 :accessor dynslot-z))
  (:metaclass dynslot-metaclass))

;;; --- effective slot metaobjects get the custom class ---

(deftest dynslot-effective-slot-is-custom
  (let ((sd (find 'x (dotcl-mop:class-slots (find-class 'dynslot-widget))
                  :key #'dotcl-mop:slot-definition-name)))
    (notnot (typep sd 'dynslot-effective)))
  t)

(deftest dynslot-normal-slot-not-custom
  (let ((sd (find 'y (dotcl-mop:class-slots (find-class 'dynslot-widget))
                  :key #'dotcl-mop:slot-definition-name)))
    (typep sd 'dynslot-effective))
  nil)

(deftest dynslot-class-of-effective-slot
  (let ((sd (find 'x (dotcl-mop:class-slots (find-class 'dynslot-widget))
                  :key #'dotcl-mop:slot-definition-name)))
    (eqt (class-of sd) (find-class 'dynslot-effective)))
  t)

(deftest dynslot-location-is-integer
  (let ((sd (find 'x (dotcl-mop:class-slots (find-class 'dynslot-widget))
                  :key #'dotcl-mop:slot-definition-name)))
    (integerp (dotcl-mop:slot-definition-location sd)))
  t)

;;; --- normal slot behaves normally ---

(deftest dynslot-normal-slot-initform
  (dynslot-y (make-instance 'dynslot-widget))
  99)

(deftest dynslot-normal-slot-setf
  (let ((w (make-instance 'dynslot-widget)))
    (setf (dynslot-y w) 7)
    (dynslot-y w))
  7)

;;; --- dynamic slot routes through slot-value-using-class + the box ---

(deftest dynslot-dynamic-accessor-roundtrip
  (let ((w (make-instance 'dynslot-widget)))
    (setf (dynslot-x w) 42)
    (dynslot-x w))
  42)

(deftest dynslot-dynamic-slot-value-roundtrip
  (let ((w (make-instance 'dynslot-widget)))
    (setf (slot-value w 'x) 13)
    (slot-value w 'x))
  13)

(deftest dynslot-instances-independent
  (let ((w1 (make-instance 'dynslot-widget))
        (w2 (make-instance 'dynslot-widget)))
    (setf (dynslot-x w1) 100)
    (setf (dynslot-x w2) -1)
    (list (dynslot-x w1) (dynslot-x w2)))
  (100 -1))

;;; --- dynamic slot WITH initform (stream-recording-p case) ---
;;; The initform must be applied through (setf slot-value-using-class) so it
;;; reaches the dynamic backing store, with boundness via slot-boundp-using-class.

(deftest dynslot-initform-applies
  (dynslot-z (make-instance 'dynslot-widget))
  5)

(deftest dynslot-initform-settable
  (let ((w (make-instance 'dynslot-widget)))
    (setf (dynslot-z w) 17)
    (dynslot-z w))
  17)

(deftest dynslot-initform-slot-boundp
  (slot-boundp (make-instance 'dynslot-widget) 'z)
  t)

(deftest dynslot-no-initform-slot-unbound
  (slot-boundp (make-instance 'dynslot-widget) 'x)
  nil)

(deftest dynslot-makunbound
  (let ((w (make-instance 'dynslot-widget)))
    (slot-makunbound w 'z)
    (slot-boundp w 'z))
  nil)
