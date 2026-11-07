;;; The subforms of a place are evaluated before the new value (CLHS 5.1.1.1), also for the
;;; accessors DEFSTRUCT and DEFCLASS define.

(defvar *spo-log* nil)
(defun spo-note (x) (push x *spo-log*) x)

(defstruct spo-std a)
(defstruct (spo-list (:type list)) a)
(defstruct (spo-vec (:type vector)) a)
(defclass spo-cls () ((x :accessor spo-x :initarg :x)))

(defmacro spo-order (form)
  `(progn (setf *spo-log* nil) ,form (reverse *spo-log*)))

(deftest setf-order.standard-struct
  (let ((s (make-spo-std)))
    (spo-order (setf (spo-std-a (progn (spo-note :place) s)) (spo-note :value))))
  (:place :value))

(deftest setf-order.list-struct
  (let ((s (make-spo-list)))
    (spo-order (setf (spo-list-a (progn (spo-note :place) s)) (spo-note :value))))
  (:place :value))

(deftest setf-order.vector-struct
  (let ((s (make-spo-vec)))
    (spo-order (setf (spo-vec-a (progn (spo-note :place) s)) (spo-note :value))))
  (:place :value))

(deftest setf-order.class-accessor
  (let ((o (make-instance 'spo-cls :x 1)))
    (spo-order (setf (spo-x (progn (spo-note :place) o)) (spo-note :value))))
  (:place :value))

(deftest setf-order.incf-struct-and-store
  (let ((s (make-spo-std :a 1)))
    (list (spo-order (incf (spo-std-a (progn (spo-note :place) s)) (progn (spo-note :value) 2)))
          (spo-std-a s)
          (let ((r nil))
            (setf r (setf (spo-std-a s) 9))
            (list r (spo-std-a s)))))
  ((:place :value) 3 (9 9)))
