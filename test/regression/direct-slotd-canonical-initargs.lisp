;;; DEFCLASS makes a direct slot definition from the canonicalized slot
;;; specification (AMOP: the DEFCLASS macro and the initialization of class
;;; metaobjects): DIRECT-SLOT-DEFINITION-CLASS and INITIALIZE-INSTANCE of the
;;; slot definition both receive :name, :initform, :initfunction, :initargs,
;;; :readers, :writers, :documentation and the slot's other options. Only :name
;;; and :initargs used to reach them, so a slot of a user slot-definition class
;;; whose initarg is :documentation (or :initform ...) stayed at its initform.

(defvar *dsci-class-initargs* nil)
(defvar *dsci-init-initargs* nil)

(defclass dsci-slotd (dotcl-mop:standard-direct-slot-definition)
  ((my-doc :initarg :documentation :initform nil :reader dsci-my-doc)
   (my-form :initarg :initform :initform :none :reader dsci-my-form)
   (extra :initarg :extra :initform nil :reader dsci-extra)))

(defmethod initialize-instance :before ((d dsci-slotd) &rest initargs)
  (setf *dsci-init-initargs* (copy-list initargs)))

(defclass dsci-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c dsci-meta) (s standard-class)) t)
(defmethod dotcl-mop:direct-slot-definition-class ((c dsci-meta) &rest initargs)
  (setf *dsci-class-initargs* (copy-list initargs))
  (find-class 'dsci-slotd))

(defclass dsci-widget ()
  ((width :initarg :width :initform (+ 1 2) :reader dsci-width :writer dsci-set-width
          :documentation "The width." :extra 7))
  (:metaclass dsci-meta))

(defun dsci-summary (plist)
  (list (getf plist :name) (getf plist :initform)
        (functionp (getf plist :initfunction))
        (and (getf plist :initfunction) (funcall (getf plist :initfunction)))
        (getf plist :initargs) (getf plist :readers) (getf plist :writers)
        (getf plist :documentation) (getf plist :extra)))

(deftest direct-slotd-canonical-initargs.direct-slot-definition-class
  (dsci-summary *dsci-class-initargs*)
  (width (+ 1 2) t 3 (:width) (dsci-width) (dsci-set-width) "The width." 7))

(deftest direct-slotd-canonical-initargs.initialize-instance
  (dsci-summary *dsci-init-initargs*)
  (width (+ 1 2) t 3 (:width) (dsci-width) (dsci-set-width) "The width." 7))

(defun dsci-slot ()
  (first (dotcl-mop:class-direct-slots (find-class 'dsci-widget))))

(deftest direct-slotd-canonical-initargs.added-slots-filled
  (list (dsci-my-doc (dsci-slot)) (dsci-my-form (dsci-slot)) (dsci-extra (dsci-slot)))
  ("The width." (+ 1 2) 7))

(deftest direct-slotd-canonical-initargs.standard-accessors
  (let ((d (dsci-slot)))
    (list (dotcl-mop:slot-definition-name d)
          (dotcl-mop:slot-definition-initform d)
          (dotcl-mop:slot-definition-initargs d)
          (dotcl-mop:slot-definition-readers d)
          (dotcl-mop:slot-definition-writers d)
          (documentation d t)))
  (width (+ 1 2) (:width) (dsci-width) (dsci-set-width) "The width."))

;;; A slot with no initform passes neither :initform nor :initfunction.

(defclass dsci-bare () ((b :initarg :b)) (:metaclass dsci-meta))

(deftest direct-slotd-canonical-initargs.no-initform
  (list (getf *dsci-class-initargs* :name)
        (getf *dsci-class-initargs* :initform :absent)
        (getf *dsci-class-initargs* :initfunction :absent)
        (dsci-my-form (first (dotcl-mop:class-direct-slots (find-class 'dsci-bare)))))
  (b :absent :absent :none))

(deftest direct-slotd-canonical-initargs.instances-still-work
  (let ((w (make-instance 'dsci-widget)))
    (list (dsci-width w) (dsci-width (make-instance 'dsci-widget :width 9))))
  (3 9))
