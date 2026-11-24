;;; MAKE-INSTANCE of a class with :BEFORE methods on INITIALIZE-INSTANCE and SHARED-INITIALIZE
;;; (alone or with :AFTER ones): the standard order, on the first call and on the later calls
;;; that reuse what the first one learned. The expected values are what SBCL gives for the
;;; same forms.

(defvar *cpb-log* nil)

(defclass cpb-base () ((a :initarg :a :initform 10 :accessor cpb-a)
                       (b :initarg :b :initform 20 :accessor cpb-b)))
(defclass cpb-sub (cpb-base) ((c :initarg :c :initform 30 :accessor cpb-c))
  (:default-initargs :b 21))

(defmethod initialize-instance :before ((o cpb-base) &rest initargs &key &allow-other-keys)
  (push (list :ii-before-base (length initargs) (slot-boundp o 'a)) *cpb-log*))
(defmethod initialize-instance :before ((o cpb-sub) &key &allow-other-keys)
  (push (list :ii-before-sub (slot-boundp o 'c)) *cpb-log*))
(defmethod shared-initialize :before ((o cpb-base) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (push (list :si-before-base slot-names (slot-boundp o 'b)) *cpb-log*))
(defmethod shared-initialize :after ((o cpb-base) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (push (list :si-after-base slot-names (cpb-a o)) *cpb-log*))
(defmethod initialize-instance :after ((o cpb-sub) &key &allow-other-keys)
  (push (list :ii-after-sub (cpb-c o)) *cpb-log*))

(defun %cpb-run (class &rest args)
  (setq *cpb-log* nil)
  (let ((o (apply #'make-instance class args)))
    (list (reverse *cpb-log*) (list (cpb-a o) (cpb-b o) (if (typep o 'cpb-sub) (cpb-c o) :none)))))

(deftest ctor-plan-before.order-base
  (let (r) (dotimes (i 3) (setq r (%cpb-run 'cpb-base :a 1))) r)
  (((:ii-before-base 2 nil) (:si-before-base t nil) (:si-after-base t 1)) (1 20 :none)))

(deftest ctor-plan-before.order-sub-with-default-initargs
  (let (r) (dotimes (i 3) (setq r (%cpb-run 'cpb-sub :c 5))) r)
  (((:ii-before-sub nil) (:ii-before-base 4 nil) (:si-before-base t nil) (:si-after-base t 10)
    (:ii-after-sub 5))
   (10 21 5)))

(deftest ctor-plan-before.no-initargs
  (let (r) (dotimes (i 3) (setq r (%cpb-run 'cpb-base))) r)
  (((:ii-before-base 0 nil) (:si-before-base t nil) (:si-after-base t 10)) (10 20 :none)))

;;; A :before method that stores into a slot: an initarg for the slot still wins, an initform
;;; does not replace a slot that is already bound.
(defclass cpb-w () ((p :initarg :p :initform :form-p :accessor cpb-p)
                    (q :initarg :q :initform :form-q :accessor cpb-q)))
(defmethod initialize-instance :before ((o cpb-w) &key &allow-other-keys)
  (setf (slot-value o 'p) :set-by-before (slot-value o 'q) :set-by-before))

(deftest ctor-plan-before.before-writes-slot
  (let (r)
    (dotimes (i 3)
      (let ((o (make-instance 'cpb-w :p :initarg-p)))
        (setq r (list (cpb-p o) (cpb-q o)))))
    r)
  (:initarg-p :set-by-before))

;;; Reading an unbound slot in a :before method is an error, as it is for any unbound slot.
(defclass cpb-r () ((v :initarg :v :initform 1)))
(defmethod initialize-instance :before ((o cpb-r) &key &allow-other-keys)
  (slot-value o 'v))

(deftest ctor-plan-before.read-unbound-slot
  (let (r)
    (dotimes (i 3)
      (setq r (handler-case (progn (make-instance 'cpb-r) :no-error) (unbound-slot () :unbound-slot))))
    r)
  :unbound-slot)

;;; A method added later is seen on the next call; an :around turns it into the full protocol.
(defclass cpb-l () ((x :initarg :x :initform 0)))
(deftest ctor-plan-before.method-added-later
  (let (before after)
    (dotimes (i 3) (setq *cpb-log* nil) (make-instance 'cpb-l :x 1) (setq before (reverse *cpb-log*)))
    (defmethod initialize-instance :before ((o cpb-l) &key &allow-other-keys) (push :late *cpb-log*))
    (dotimes (i 3) (setq *cpb-log* nil) (make-instance 'cpb-l :x 1) (setq after (reverse *cpb-log*)))
    (list before after))
  (nil (:late)))

(defclass cpb-o () ((x :initarg :x :initform 0)))
(defmethod initialize-instance :before ((o cpb-o) &key &allow-other-keys) (push :before *cpb-log*))
(deftest ctor-plan-before.around-added-later
  (let (before after)
    (dotimes (i 3) (setq *cpb-log* nil) (make-instance 'cpb-o) (setq before (reverse *cpb-log*)))
    (defmethod initialize-instance :around ((o cpb-o) &key &allow-other-keys)
      (push :around-in *cpb-log*) (prog1 (call-next-method) (push :around-out *cpb-log*)))
    (dotimes (i 3) (setq *cpb-log* nil) (make-instance 'cpb-o) (setq after (reverse *cpb-log*)))
    (list before after))
  ((:before) (:around-in :before :around-out)))

(defclass cpb-m () ((x :initarg :x :initform 0)))
(defmethod initialize-instance :before ((o cpb-m) &key &allow-other-keys) (push :m *cpb-log*))
(deftest ctor-plan-before.method-removed
  (let (before after)
    (dotimes (i 3) (setq *cpb-log* nil) (make-instance 'cpb-m) (setq before (reverse *cpb-log*)))
    (remove-method #'initialize-instance
                   (find-method #'initialize-instance '(:before) (list (find-class 'cpb-m))))
    (dotimes (i 3) (setq *cpb-log* nil) (make-instance 'cpb-m) (setq after (reverse *cpb-log*)))
    (list before after))
  ((:m) nil))

;;; reinitialize-instance does not run INITIALIZE-INSTANCE's methods, only SHARED-INITIALIZE's.
(deftest ctor-plan-before.reinitialize-instance
  (let ((o (make-instance 'cpb-base :a 1)))
    (setq *cpb-log* nil)
    (reinitialize-instance o :a 2)
    (list (reverse *cpb-log*) (cpb-a o)))
  (((:si-before-base nil t) (:si-after-base nil 2)) 2))

(deftest ctor-plan-before.change-class
  (let ((o (make-instance 'cpb-base :a 1)))
    (dotimes (i 3) (make-instance 'cpb-base))
    (change-class o 'cpb-sub)
    (list (cpb-a o) (cpb-c o) (type-of o)))
  (1 30 cpb-sub))

;;; A class that has an :around (so no fast path) gets it back after the method is removed
;;; and after another one is added.
(defclass cpb-x () ((x :initarg :x :initform 0 :accessor cpb-x-x)))
(defmethod initialize-instance :around ((o cpb-x) &key &allow-other-keys)
  (push :around-in *cpb-log*) (prog1 (call-next-method) (push :around-out *cpb-log*)))
(deftest ctor-plan-before.around-removed-later
  (let (with without readded)
    (dotimes (i 3) (setq *cpb-log* nil) (make-instance 'cpb-x :x 1) (setq with (reverse *cpb-log*)))
    (remove-method #'initialize-instance
                   (find-method #'initialize-instance '(:around) (list (find-class 'cpb-x))))
    (dotimes (i 3) (setq *cpb-log* nil) (make-instance 'cpb-x :x 1) (setq without (reverse *cpb-log*)))
    (defmethod initialize-instance :before ((o cpb-x) &key &allow-other-keys) (push :before *cpb-log*))
    (dotimes (i 3) (setq *cpb-log* nil) (make-instance 'cpb-x :x 1) (setq readded (reverse *cpb-log*)))
    (list with without readded (cpb-x-x (make-instance 'cpb-x :x 7))))
  ((:around-in :around-out) nil (:before) 7))
