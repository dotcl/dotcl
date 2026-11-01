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

;; A method added after the accessor has been called. The accessor's generic
;; function also has a method of another kind, so a compiled call site does not
;; cache the slot itself and the call goes through the generic function's own
;; dispatch cache, which held a direct reader / writer for the class from the
;; calls before the method existed: the new method did not run until something
;; else cleared that cache.
(defclass svucs-late () ((foo :initform 1 :accessor svucs-late-foo)))
(defclass svucs-late-other () ())
(defmethod svucs-late-foo ((o svucs-late-other)) :other)
(defmethod (setf svucs-late-foo) (v (o svucs-late-other)) v)
(defun %svucs-late-read (o) (svucs-late-foo o))
(defun %svucs-late-write (v o) (setf (svucs-late-foo o) v))

(deftest svuc-method-added-after-accessor-calls
  (let ((o (make-instance 'svucs-late)) (*svucs-log* nil))
    (%svucs-late-read o) (%svucs-late-read o)
    (%svucs-late-write 2 o) (%svucs-late-write 3 o)
    (let ((r (defmethod dotcl-mop:slot-value-using-class :around
                 ((c standard-class) (o svucs-late) s)
               (declare (ignore c s))
               (push :read *svucs-log*)
               (call-next-method)))
          (w (defmethod (setf dotcl-mop:slot-value-using-class) :after
                 (v (c standard-class) (o svucs-late) s)
               (declare (ignore c s))
               (push (list :write v) *svucs-log*))))
      (let ((with (list (%svucs-late-read o) (%svucs-late-write 4 o)))
            (log-with (reverse *svucs-log*)))
        (remove-method #'dotcl-mop:slot-value-using-class r)
        (remove-method #'(setf dotcl-mop:slot-value-using-class) w)
        (setq *svucs-log* nil)
        (list with log-with
              (list (%svucs-late-read o) (%svucs-late-write 5 o)) *svucs-log*))))
  ((3 4) (:read (:write 4)) (4 5) nil))
