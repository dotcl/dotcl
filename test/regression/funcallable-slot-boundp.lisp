;;; SLOT-BOUNDP, SLOT-MAKUNBOUND and SLOT-EXISTS-P on a funcallable instance.
;;;
;;; An instance of a FUNCALLABLE-STANDARD-CLASS is a generic function object
;;; that keeps its slots on the side. SLOT-VALUE and (SETF SLOT-VALUE) knew
;;; that; SLOT-BOUNDP and SLOT-MAKUNBOUND signalled "not a CLOS instance" and
;;; SLOT-EXISTS-P said NIL. The TRY test framework checks SLOT-BOUNDP on its
;;; funcallable TRIAL objects in an :AFTER method, so no test run through it
;;; could start (named-readtables' suite, for one).

(defclass fsb-obj ()
  ((a :initarg :a :accessor fsb-a)
   (b :accessor fsb-b))
  (:metaclass dotcl-mop:funcallable-standard-class))

(defvar *fsb-after-seen* nil)
(defmethod initialize-instance :after ((x fsb-obj) &key)
  (setf *fsb-after-seen* (list (slot-boundp x 'a) (slot-boundp x 'b))))

(deftest funcallable-slot-boundp.after-method
  (progn (make-instance 'fsb-obj :a 1) *fsb-after-seen*)
  (t nil))

(deftest funcallable-slot-boundp.boundp-and-exists
  (let ((x (make-instance 'fsb-obj :a 1)))
    (list (slot-boundp x 'a) (slot-boundp x 'b)
          (slot-exists-p x 'a) (slot-exists-p x 'b) (slot-exists-p x 'nope)))
  (t nil t t nil))

(deftest funcallable-slot-boundp.makunbound
  (let ((x (make-instance 'fsb-obj :a 1)))
    (setf (fsb-b x) 2)
    (slot-makunbound x 'a)
    (list (slot-boundp x 'a) (slot-boundp x 'b) (fsb-b x)))
  (nil t 2))
