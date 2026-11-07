;;; CHANGE-CLASS hands SHARED-INITIALIZE only the local slots the new class adds;
;;; a class-allocated slot keeps its value and is not in the list.

(defvar *%ccal-log* nil)
(defclass ccal-a () ((x :initarg :x :initform 1) (y :initarg :y) (z :allocation :class :initform 'zz)))
(defclass ccal-b () ((q :initarg :q :initform 0)))
(defmethod shared-initialize :after ((o ccal-a) slots &rest initargs)
  (declare (ignore initargs))
  (push (list 'a slots) *%ccal-log*))

(deftest change-class-added.skips-class-allocated-slots
  (let ((*%ccal-log* nil) (o (make-instance 'ccal-b)))
    (setf *%ccal-log* nil)
    (change-class o 'ccal-a)
    (list (type-of o)
          (mapcar #'second (reverse *%ccal-log*))
          (sort (copy-list (second (first *%ccal-log*))) #'string<)))
  (ccal-a ((y x)) (x y)))
