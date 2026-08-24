;;; INITIALIZE-INSTANCE's default method calls SHARED-INITIALIZE. When nothing has
;;; a method of its own on SHARED-INITIALIZE for the instance, the default method is
;;; what the dispatch reaches, so it now runs directly on the array already in hand
;;; instead of building an (instance T . initargs) copy for every instance created
;;; (72 B each).
;;;
;;; The shortcut must be indistinguishable from the dispatch, so these tests pin the
;;; parts that would notice: slot filling, initforms, a user SHARED-INITIALIZE method
;;; (which must disable the shortcut), and the error for a malformed initarg list.

(defclass sis-plain ()
  ((a :initarg :a :initform :no-a) (b :initarg :b :initform :no-b)))
(defmethod initialize-instance :after ((self sis-plain) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value self 'b) (list :after (slot-value self 'b))))

(deftest shared-initialize-span.slots-and-initforms
  (let ((x (make-instance 'sis-plain :a 1)))
    (list (slot-value x 'a) (slot-value x 'b)))
  (1 (:after :no-b)))

;;; A user SHARED-INITIALIZE method still runs, and still sees the initargs.
(defclass sis-custom () ((a :initarg :a :initform 0) (seen :initform nil)))
(defmethod initialize-instance :after ((self sis-custom) &rest initargs)
  (declare (ignore initargs)) nil)
(defmethod shared-initialize :after ((self sis-custom) slot-names &rest initargs)
  (declare (ignore slot-names))
  (setf (slot-value self 'seen) (copy-list initargs)))

(deftest shared-initialize-span.user-method-runs
  (let ((x (make-instance 'sis-custom :a 7)))
    (list (slot-value x 'a) (slot-value x 'seen)))
  (7 (:a 7)))

;;; Defining that method AFTER instances have already been made must take effect:
;;; the per-class "no method of its own" answer is cached.
(defclass sis-late () ((a :initarg :a :initform 0) (seen :initform :none)))
(defmethod initialize-instance :after ((self sis-late) &rest initargs)
  (declare (ignore initargs)) nil)

(deftest shared-initialize-span.late-method-invalidates-cache
  (let ((before (slot-value (make-instance 'sis-late :a 1) 'seen)))
    (eval '(defmethod shared-initialize :after ((self sis-late) slot-names &rest initargs)
            (declare (ignore slot-names initargs))
            (setf (slot-value self 'seen) :ran)))
    (list before (slot-value (make-instance 'sis-late :a 1) 'seen)))
  (:none :ran))

;;; SHARED-INITIALIZE called directly keeps its own argument shape.
(deftest shared-initialize-span.direct-call
  (let ((x (make-instance 'sis-plain :a 1)))
    (shared-initialize x t :a 42)
    (slot-value x 'a))
  42)

;;; An odd initarg list is still rejected on both routes.
(deftest shared-initialize-span.odd-initargs-signal
  (let ((x (make-instance 'sis-plain :a 1)))
    (list (handler-case (progn (shared-initialize x t :a) :no-error)
            (error () :error))
          (handler-case (progn (make-instance 'sis-plain :a) :no-error)
            (error () :error))))
  (:error :error))

;;; :allocation :class slots are shared, and initforms fill them once.
(defclass sis-classalloc ()
  ((shared :allocation :class :initform :from-initform)
   (own :initarg :own :initform 0)))
(defmethod initialize-instance :after ((self sis-classalloc) &rest initargs)
  (declare (ignore initargs)) nil)

(deftest shared-initialize-span.class-allocated-slot
  (let ((x (make-instance 'sis-classalloc :own 1))
        (y (make-instance 'sis-classalloc :own 2)))
    (setf (slot-value x 'shared) :written)
    (list (slot-value y 'shared) (slot-value x 'own) (slot-value y 'own)))
  (:written 1 2))
