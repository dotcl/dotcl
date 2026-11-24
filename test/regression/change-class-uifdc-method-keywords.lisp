;;; Keyword arguments of an applicable UPDATE-INSTANCE-FOR-DIFFERENT-CLASS method are valid initargs
;;; of CHANGE-CLASS; other keywords are still rejected.

(defclass ccu-old () ((p :initarg :p)))
(defclass ccu-new () ((p :initarg :p) (w :initform 1)))
(defclass ccu-other () ((p :initarg :p)))

(defvar *ccu-seen* nil)
(defmethod update-instance-for-different-class :after ((old ccu-old) (new ccu-new) &key extra)
  (push extra *ccu-seen*)
  (setf (slot-value new 'w) extra))

(deftest change-class-uifdc-method-keywords.accepted
  (let ((o (make-instance 'ccu-old :p 3)))
    (setf *ccu-seen* nil)
    (change-class o 'ccu-new :extra 9)
    (list (slot-value o 'p) (slot-value o 'w) *ccu-seen*))
  (3 9 (9)))

(deftest change-class-uifdc-method-keywords.other-class-rejects
  (handler-case (progn (change-class (make-instance 'ccu-old :p 1) 'ccu-other :extra 9) :accepted)
    (error () :rejected))
  :rejected)

(deftest change-class-uifdc-method-keywords.unknown-rejected
  (handler-case (progn (change-class (make-instance 'ccu-old :p 1) 'ccu-new :bogus 9) :accepted)
    (error () :rejected))
  :rejected)
