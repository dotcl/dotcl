;;; MAKE-INSTANCE of a class with :AFTER methods on SHARED-INITIALIZE (alone or with
;;; INITIALIZE-INSTANCE ones) gives the result of the standard protocol, on the first
;;; call and on the later calls that reuse what the first one learned.

(defvar *cpsa-log* nil)

(defclass cpsa-base () ((a :initarg :a :initform 0 :accessor cpsa-a)
                        (b :initarg :b :initform nil :accessor cpsa-b)))
(defclass cpsa-sub (cpsa-base) ((c :initarg :c :initform 5 :accessor cpsa-c)))

(defmethod shared-initialize :after ((o cpsa-base) slot-names &rest initargs &key (a nil a-p) &allow-other-keys)
  (declare (ignore initargs))
  (push (list :shared-base slot-names a-p (and a-p a)) *cpsa-log*))
(defmethod shared-initialize :after ((o cpsa-sub) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (push (list :shared-sub slot-names) *cpsa-log*))
(defmethod initialize-instance :after ((o cpsa-base) &rest initargs &key &allow-other-keys)
  (push (list :init-base (length initargs)) *cpsa-log*))
(defmethod initialize-instance :after ((o cpsa-sub) &key &allow-other-keys)
  (push (list :init-sub) *cpsa-log*))

(defun %cpsa-run (class &rest args)
  (setq *cpsa-log* nil)
  (let ((o (apply #'make-instance class args)))
    (list (reverse *cpsa-log*) o)))

(deftest ctor-plan-shared-after.order-base
  (let (r)
    (dotimes (i 3) (setq r (first (%cpsa-run 'cpsa-base :a 4 :b 9))))
    r)
  ((:shared-base t t 4) (:init-base 4)))

(deftest ctor-plan-shared-after.order-sub
  (let (r)
    (dotimes (i 3) (setq r (first (%cpsa-run 'cpsa-sub :c 1))))
    r)
  ((:shared-base t nil nil) (:shared-sub t) (:init-base 2) (:init-sub)))

(deftest ctor-plan-shared-after.slots
  (let (r)
    (dotimes (i 3)
      (let ((o (second (%cpsa-run 'cpsa-sub :a 7 :c 1))))
        (setq r (list (cpsa-a o) (cpsa-b o) (cpsa-c o)))))
    r)
  (7 nil 1))

(deftest ctor-plan-shared-after.default-initform
  (let (r)
    (dotimes (i 3) (setq r (let ((o (second (%cpsa-run 'cpsa-sub)))) (list (cpsa-a o) (cpsa-c o)))))
    r)
  (0 5))

(defclass cpsa-only () ((x :initarg :x :initform 1 :accessor cpsa-x)))
(defmethod shared-initialize :after ((o cpsa-only) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (push (list :only slot-names (cpsa-x o)) *cpsa-log*))

(deftest ctor-plan-shared-after.shared-only
  (let (r)
    (dotimes (i 3) (setq r (first (%cpsa-run 'cpsa-only :x 3))))
    r)
  ((:only t 3)))

(deftest ctor-plan-shared-after.method-added-later
  (let (before after)
    (dotimes (i 3) (setq before (first (%cpsa-run 'cpsa-only :x 3))))
    (defmethod initialize-instance :after ((o cpsa-only) &key &allow-other-keys)
      (push (list :late) *cpsa-log*))
    (dotimes (i 3) (setq after (first (%cpsa-run 'cpsa-only :x 3))))
    (list before after))
  (((:only t 3)) ((:only t 3) (:late))))

(defclass cpsa-aro () ((x :initarg :x :initform 1 :accessor cpsa-ax)))
(defmethod shared-initialize :after ((o cpsa-aro) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (push :after *cpsa-log*))

(deftest ctor-plan-shared-after.around-added-later
  (let (before after)
    (dotimes (i 3) (setq before (first (%cpsa-run 'cpsa-aro :x 1))))
    (defmethod shared-initialize :around ((o cpsa-aro) slot-names &rest initargs &key &allow-other-keys)
      (push :around-in *cpsa-log*)
      (prog1 (call-next-method) (push :around-out *cpsa-log*)))
    (dotimes (i 3) (setq after (first (%cpsa-run 'cpsa-aro :x 1))))
    (list before after))
  ((:after) (:around-in :after :around-out)))

(deftest ctor-plan-shared-after.before-method
  (progn
    (defclass cpsa-bef () ((x :initarg :x :initform 1)))
    (defmethod shared-initialize :before ((o cpsa-bef) slot-names &rest initargs &key &allow-other-keys)
      (declare (ignore initargs))
      (push :before *cpsa-log*))
    (defmethod shared-initialize :after ((o cpsa-bef) slot-names &rest initargs &key &allow-other-keys)
      (declare (ignore initargs))
      (push :after *cpsa-log*))
    (let (r) (dotimes (i 3) (setq r (first (%cpsa-run 'cpsa-bef :x 1)))) r))
  (:before :after))
