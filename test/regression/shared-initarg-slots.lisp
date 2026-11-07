;;; One initarg naming several slots initializes every one of them (CLHS 7.1.1).
;;;
;;; MAKE-INSTANCE on a class with no initialization methods of its own sets the
;;; slots straight from a name -> slot map, and that map kept only the first slot
;;; of a shared name, so the second slot stayed unbound (or kept its initform).

(defclass sis-two ()
  ((a :initarg :i :reader sis-two-a)
   (b :initarg :i :reader sis-two-b)))

(deftest shared-initarg-slots.both-filled
  (let ((o (make-instance 'sis-two :i 1)))
    (list (sis-two-a o) (and (slot-boundp o 'b) (sis-two-b o))))
  (1 1))

(defclass sis-default ()
  ((a :initarg :i :reader sis-default-a)
   (b :initarg :i :initform :default :reader sis-default-b)))

(deftest shared-initarg-slots.initarg-beats-initform
  (let ((o (make-instance 'sis-default :i 1)))
    (list (sis-default-a o) (sis-default-b o)))
  (1 1))

(deftest shared-initarg-slots.initform-without-initarg
  (let ((o (make-instance 'sis-default)))
    (list (slot-boundp o 'a) (sis-default-b o)))
  (nil :default))

(defclass sis-class-object () ((a :initarg :i) (b :initarg :i)))

(deftest shared-initarg-slots.class-object
  (let ((o (make-instance (find-class 'sis-class-object) :i 3)))
    (list (slot-value o 'a) (slot-value o 'b)))
  (3 3))

;;; The shared name may come from a superclass slot.

(defclass sis-base () ((a :initarg :i)))
(defclass sis-derived (sis-base) ((b :initarg :i)))

(deftest shared-initarg-slots.inherited
  (let ((o (make-instance 'sis-derived :i 4)))
    (list (slot-value o 'a) (slot-value o 'b)))
  (4 4))

;;; The leftmost occurrence of the initarg supplies the value.

(deftest shared-initarg-slots.first-value-wins
  (let ((o (make-instance 'sis-two :i 1 :i 2)))
    (list (sis-two-a o) (sis-two-b o)))
  (1 1))
