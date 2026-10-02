;;; A program's method on (SETF SLOT-VALUE-USING-CLASS) or
;;; SLOT-VALUE-USING-CLASS that is specialized on the INSTANCE of a
;;; STANDARD-CLASS class. dotcl went through the *-USING-CLASS generic
;;; functions only for classes with a custom metaclass, so for an ordinary
;;; class such a method never ran: an :AFTER method observing slot writes saw
;;; nothing. The observables library (Shirakumo) builds its OBSERVABLE-OBJECT on
;;; exactly this. SBCL runs the methods for SLOT-VALUE, its SETF and the
;;; slot accessors alike.

(defvar *svucs-log* nil)

(defclass svucs-watched () ((foo :initform 1 :accessor svucs-foo)))
(defclass svucs-plain () ((foo :initform 1 :accessor svucs-plain-foo)))

(defparameter *svucs-after*
  (defmethod (setf dotcl-mop:slot-value-using-class) :after
      (value class (object svucs-watched) slot)
    (declare (ignore class))
    (push (list :write value (dotcl-mop:slot-definition-name slot)) *svucs-log*)))

(defmethod dotcl-mop:slot-value-using-class :around (class (object svucs-watched) slot)
  (declare (ignore class slot))
  (push :read *svucs-log*)
  (call-next-method))

(defun %svucs-run ()
  (let ((*svucs-log* nil)
        (w (make-instance 'svucs-watched))
        (name 'foo))
    (setf (slot-value w 'foo) 10)
    (setf (slot-value w name) 11)
    (setf (svucs-foo w) 12)
    (list (svucs-foo w) (slot-value w 'foo) (reverse *svucs-log*))))

(deftest svuc-methods-on-standard-class.run
  (%svucs-run)
  (12 12 ((:write 10 foo) (:write 11 foo) (:write 12 foo) :read :read)))

(deftest svuc-methods-on-standard-class.other-classes-untouched
  (let ((*svucs-log* nil) (p (make-instance 'svucs-plain)))
    (setf (slot-value p 'foo) 3)
    (setf (svucs-plain-foo p) 4)
    (list (svucs-plain-foo p) *svucs-log*))
  (4 nil))

(deftest svuc-methods-on-standard-class.after-remove-method
  (progn
    (remove-method (dotcl-mop:method-generic-function *svucs-after*) *svucs-after*)
    (let ((*svucs-log* nil) (w (make-instance 'svucs-watched)))
      (setf (slot-value w 'foo) 5)
      (list (slot-value w 'foo) (remove :read *svucs-log*))))
  (5 nil))
