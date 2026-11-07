;;; READER-METHOD-CLASS / WRITER-METHOD-CLASS: a metaclass method on them is called
;;; with the direct slot definition when DEFCLASS makes an accessor, and the class it
;;; answers is the class of the accessor method.

(defvar *amc-log* nil)
(defclass amc-reader (standard-reader-method) ())
(defclass amc-writer (standard-writer-method) ())
(defclass amc-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((a amc-meta) (b standard-class)) t)
(defmethod dotcl-mop:reader-method-class ((c amc-meta) (d dotcl-mop:standard-direct-slot-definition)
                                          &rest initargs)
  (declare (ignore initargs))
  (push (list :reader (dotcl-mop:slot-definition-name d)) *amc-log*)
  (find-class 'amc-reader))
(defmethod dotcl-mop:writer-method-class ((c amc-meta) (d dotcl-mop:standard-direct-slot-definition)
                                          &rest initargs)
  (declare (ignore initargs))
  (push (list :writer (dotcl-mop:slot-definition-name d)) *amc-log*)
  (find-class 'amc-writer))

(defclass amc-c () ((a :accessor amc-a :initarg :a)) (:metaclass amc-meta))
(defclass amc-plain () ((a :accessor amc-plain-a :initarg :a)))

(deftest mop-accessor-method-class.called
  (reverse *amc-log*)
  ((:reader a) (:writer a)))

(deftest mop-accessor-method-class.result-is-the-class
  (list (mapcar #'class-name (mapcar #'class-of (dotcl-mop:generic-function-methods #'amc-a)))
        (mapcar #'class-name (mapcar #'class-of (dotcl-mop:generic-function-methods #'(setf amc-a)))))
  ((amc-reader) (amc-writer)))

(deftest mop-accessor-method-class.accessors-still-work
  (let ((o (make-instance 'amc-c :a 3)))
    (setf (amc-a o) (1+ (amc-a o)))
    (amc-a o))
  4)

(deftest mop-accessor-method-class.standard-metaclass-unchanged
  (list (mapcar #'class-name (mapcar #'class-of (dotcl-mop:generic-function-methods #'amc-plain-a)))
        (amc-plain-a (make-instance 'amc-plain :a 7)))
  ((dotcl-mop:standard-reader-method) 7))
