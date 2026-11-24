;;; MAKE-INSTANCE of a class with :AROUND methods on INITIALIZE-INSTANCE (with or without :BEFORE /
;;; :AFTER ones): the standard order and the standard meaning of the :around methods' calls to
;;; CALL-NEXT-METHOD, on the first call and on later ones. The expected values are SBCL's.

(defvar *cpa-log* nil)

(defclass cpa-base () ((x :initarg :x :initform :x0 :accessor cpa-x)
                       (y :initarg :y :initform :y0 :accessor cpa-y)))
(defclass cpa-sub (cpa-base) ((z :initarg :z :initform :z0 :accessor cpa-z))
  (:default-initargs :y :y-default))

(defmethod initialize-instance :around ((o cpa-base) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (push :around-base-in *cpa-log*)
  (prog1 (call-next-method) (push :around-base-out *cpa-log*)))
(defmethod initialize-instance :around ((o cpa-sub) &key &allow-other-keys)
  (push :around-sub-in *cpa-log*)
  (prog1 (call-next-method) (push :around-sub-out *cpa-log*)))
(defmethod initialize-instance :before ((o cpa-base) &key &allow-other-keys) (push :before-base *cpa-log*))
(defmethod initialize-instance :after ((o cpa-sub) &key &allow-other-keys) (push :after-sub *cpa-log*))

(defun cpa-run (class &rest args)
  (setq *cpa-log* nil)
  (let ((o (apply #'make-instance class args)))
    (list (reverse *cpa-log*) (cpa-x o) (cpa-y o) (if (typep o 'cpa-sub) (cpa-z o) :none))))
(defun cpa-3 (class &rest args) (let (r) (dotimes (i 3) (setq r (apply #'cpa-run class args))) r))

(deftest ctor-plan-around.order
  (list (cpa-3 'cpa-base :x 1) (cpa-3 'cpa-sub :z 5))
  (((:around-base-in :before-base :around-base-out) 1 :y0 :none)
   ((:around-sub-in :around-base-in :before-base :after-sub :around-base-out :around-sub-out)
    :x0 :y-default 5)))

;;; An :around that does not call the next method leaves the slots unbound; one that calls it twice
;;; runs the rest twice; one that calls it with other arguments passes those on (the first of two
;;; initargs for a slot wins, a key the call did not carry reaches the slot it names).
(defclass cpa-none () ((x :initarg :x :initform 1)))
(defmethod initialize-instance :around ((o cpa-none) &key &allow-other-keys) o)
(defclass cpa-twice () ((x :initarg :x :initform 1)))
(defmethod initialize-instance :around ((o cpa-twice) &key &allow-other-keys)
  (call-next-method) (call-next-method))
(defmethod initialize-instance :before ((o cpa-twice) &key &allow-other-keys) (push :before *cpa-log*))
(defclass cpa-args () ((x :initarg :x :initform :x0 :accessor cpa-args-x)
                       (y :initarg :y :initform :y0 :accessor cpa-args-y)))
(defmethod initialize-instance :around ((o cpa-args) &rest initargs &key replace &allow-other-keys)
  (if replace
      (call-next-method o :y 7 :x 1 :x 2 :unknown-key 3 :allow-other-keys t)
      (apply #'call-next-method o initargs)))

(deftest ctor-plan-around.call-next-method-shapes
  (let (r)
    (dotimes (i 3)
      (setq *cpa-log* nil)
      (let ((n (make-instance 'cpa-none :x 4))
            (tw (make-instance 'cpa-twice :x 4))
            (a1 (make-instance 'cpa-args :x 9))
            (a2 (make-instance 'cpa-args :replace t :x 9)))
        (setq r (list (slot-boundp n 'x) (slot-value tw 'x) (reverse *cpa-log*)
                      (list (cpa-args-x a1) (cpa-args-y a1))
                      (list (cpa-args-x a2) (cpa-args-y a2))))))
    r)
  (nil 4 (:before :before) (9 :y0) (1 7)))

;;; A condition signalled by an :around leaves nothing behind: the next call is normal.
(defclass cpa-err () ((x :initarg :x :initform 1)))
(defmethod initialize-instance :around ((o cpa-err) &key boom &allow-other-keys)
  (when boom (error "boom"))
  (call-next-method))
(deftest ctor-plan-around.condition
  (let (r)
    (dotimes (i 3)
      (setq r (list (handler-case (make-instance 'cpa-err :boom t) (error () :error))
                    (slot-value (make-instance 'cpa-err :x 5) 'x)
                    (handler-case (make-instance 'cpa-err :boom t) (error () :error))
                    (slot-value (make-instance 'cpa-err) 'x))))
    r)
  (:error 5 :error 1))

;;; Methods added and removed after the class has been planned.
(defclass cpa-m () ((x :initarg :x :initform 0)))
(deftest ctor-plan-around.methods-change
  (let (none with without)
    (dotimes (i 3) (setq *cpa-log* nil) (make-instance 'cpa-m :x 1) (setq none (reverse *cpa-log*)))
    (defmethod initialize-instance :around ((o cpa-m) &key &allow-other-keys)
      (push :in *cpa-log*) (prog1 (call-next-method) (push :out *cpa-log*)))
    (dotimes (i 3) (setq *cpa-log* nil) (make-instance 'cpa-m :x 1) (setq with (reverse *cpa-log*)))
    (remove-method #'initialize-instance
                   (find-method #'initialize-instance '(:around) (list (find-class 'cpa-m))))
    (dotimes (i 3) (setq *cpa-log* nil) (make-instance 'cpa-m :x 1) (setq without (reverse *cpa-log*)))
    (list none with without))
  (nil (:in :out) nil))

;;; reinitialize-instance and change-class do not run INITIALIZE-INSTANCE's :around methods.
(defclass cpa-r () ((x :initarg :x :initform 0 :accessor cpa-r-x)))
(defclass cpa-r2 (cpa-r) ())
(defmethod initialize-instance :around ((o cpa-r) &key &allow-other-keys)
  (push :around *cpa-log*) (call-next-method))
(deftest ctor-plan-around.reinitialize-change-class
  (let ((o (progn (dotimes (i 3) (make-instance 'cpa-r)) (make-instance 'cpa-r :x 1))))
    (setq *cpa-log* nil)
    (reinitialize-instance o :x 2)
    (change-class o 'cpa-r2)
    (list (reverse *cpa-log*) (cpa-r-x o) (type-of o)))
  (nil 2 cpa-r2))
