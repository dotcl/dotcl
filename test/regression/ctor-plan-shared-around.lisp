;;; MAKE-INSTANCE of a class with :AROUND methods on SHARED-INITIALIZE (with or without the other
;;; :before / :around / :after methods of INITIALIZE-INSTANCE and SHARED-INITIALIZE): the standard order
;;; and the standard meaning of the :around methods' calls to CALL-NEXT-METHOD, including a call that
;;; changes the slot names or the initargs, on the first call and on later ones. The expected values
;;; are SBCL's.

(defvar *cps-log* nil)

(defclass cps-base () ((x :initarg :x :initform :x0 :accessor cps-x)
                       (y :initarg :y :initform :y0 :accessor cps-y)))
(defclass cps-sub (cps-base) ((z :initarg :z :initform :z0 :accessor cps-z)))

(defmethod shared-initialize :around ((o cps-base) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (push (list :si-around-base-in slot-names) *cps-log*)
  (prog1 (call-next-method) (push :si-around-base-out *cps-log*)))
(defmethod shared-initialize :around ((o cps-sub) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (push :si-around-sub-in *cps-log*)
  (prog1 (call-next-method) (push :si-around-sub-out *cps-log*)))
(defmethod shared-initialize :before ((o cps-base) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (push :si-before *cps-log*))
(defmethod shared-initialize :after ((o cps-base) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (push :si-after *cps-log*))
(defmethod initialize-instance :before ((o cps-base) &key &allow-other-keys) (push :ii-before *cps-log*))
(defmethod initialize-instance :after ((o cps-base) &key &allow-other-keys) (push :ii-after *cps-log*))

(defun cps-run (class &rest args)
  (setq *cps-log* nil)
  (let ((o (apply #'make-instance class args)))
    (list (reverse *cps-log*) (slot-value o 'x) (slot-value o 'y)
          (if (typep o 'cps-sub) (slot-value o 'z) :none))))
(defun cps-3 (class &rest args) (let (r) (dotimes (i 3) (setq r (apply #'cps-run class args))) r))

(deftest ctor-plan-shared-around.order
  (list (cps-3 'cps-base :x 1) (cps-3 'cps-sub :z 5))
  (((:ii-before (:si-around-base-in t) :si-before :si-after :si-around-base-out :ii-after) 1 :y0 :none)
   ((:ii-before :si-around-sub-in (:si-around-base-in t) :si-before :si-after :si-around-base-out
     :si-around-sub-out :ii-after)
    :x0 :y0 5)))

;;; :around methods that do not call the next method, call it twice, change the slot names, change
;;; the initargs.
(defclass cps-none () ((x :initarg :x :initform 1)))
(defmethod shared-initialize :around ((o cps-none) slot-names &key &allow-other-keys) o)
(defclass cps-twice () ((x :initarg :x :initform 1)))
(defmethod shared-initialize :around ((o cps-twice) slot-names &key &allow-other-keys)
  (call-next-method) (call-next-method))
(defmethod shared-initialize :before ((o cps-twice) slot-names &key &allow-other-keys) (push :before *cps-log*))
(defclass cps-names () ((x :initarg :x :initform :x0 :accessor cps-n-x)
                        (y :initarg :y :initform :y0 :accessor cps-n-y)
                        (w :initarg :w :initform :w0 :accessor cps-n-w)))
(defmethod shared-initialize :around ((o cps-names) slot-names &rest initargs &key mode &allow-other-keys)
  (case mode
    (:none (call-next-method o nil :x 5))
    (:list (call-next-method o '(y) :x 6))
    (:args (call-next-method o t :y 7 :x 1 :x 2 :w 3))
    (t (apply #'call-next-method o slot-names initargs))))
(defun cps-slots (o) (list (if (slot-boundp o 'x) (cps-n-x o) :unbound)
                           (if (slot-boundp o 'y) (cps-n-y o) :unbound)
                           (if (slot-boundp o 'w) (cps-n-w o) :unbound)))

(deftest ctor-plan-shared-around.call-next-method-shapes
  (let (r)
    (dotimes (i 3)
      (setq *cps-log* nil)
      (let ((n (make-instance 'cps-none :x 4))
            (tw (make-instance 'cps-twice :x 4)))
        (setq r (list (slot-boundp n 'x) (slot-value tw 'x) (reverse *cps-log*)
                      (cps-slots (make-instance 'cps-names :x 9))
                      (cps-slots (make-instance 'cps-names :mode :none))
                      (cps-slots (make-instance 'cps-names :mode :list))
                      (cps-slots (make-instance 'cps-names :mode :args))))))
    r)
  (nil 4 (:before :before)
   (9 :y0 :w0) (5 :unbound :unbound) (6 :y0 :unbound) (1 7 3)))

;;; With an :around of INITIALIZE-INSTANCE as well, and a second parent (multiple inheritance).
(defclass cps-p1 () ((a :initarg :a :initform 1 :accessor cps-a)))
(defclass cps-p2 () ((b :initarg :b :initform 2 :accessor cps-b)))
(defclass cps-mi (cps-p1 cps-p2) ())
(defmethod shared-initialize :around ((o cps-p1) slot-names &key &allow-other-keys)
  (push :si-p1 *cps-log*) (call-next-method))
(defmethod shared-initialize :around ((o cps-p2) slot-names &key &allow-other-keys)
  (push :si-p2 *cps-log*) (call-next-method))
(defmethod initialize-instance :around ((o cps-mi) &key &allow-other-keys)
  (push :ii-around-in *cps-log*) (prog1 (call-next-method) (push :ii-around-out *cps-log*)))

(deftest ctor-plan-shared-around.with-ii-around-and-multiple-inheritance
  (let (r)
    (dotimes (i 3)
      (setq *cps-log* nil)
      (let ((o (make-instance 'cps-mi :b 8)))
        (setq r (list (reverse *cps-log*) (cps-a o) (cps-b o)))))
    r)
  ((:ii-around-in :si-p1 :si-p2 :ii-around-out) 1 8))

;;; A condition signalled by an :around, methods added and removed later, reinitialize-instance
;;; (which does run SHARED-INITIALIZE's :around) and change-class.
(defclass cps-e () ((x :initarg :x :initform 1)))
(defmethod shared-initialize :around ((o cps-e) slot-names &key boom &allow-other-keys)
  (when boom (error "boom"))
  (call-next-method))
(defclass cps-m () ((x :initarg :x :initform 0 :accessor cps-m-x)))
(defclass cps-m2 (cps-m) ())
(deftest ctor-plan-shared-around.changes
  (let (err none with without)
    (dotimes (i 3)
      (setq err (list (handler-case (make-instance 'cps-e :boom t) (error () :error))
                      (slot-value (make-instance 'cps-e :x 5) 'x)
                      (handler-case (make-instance 'cps-e :boom t) (error () :error))
                      (slot-value (make-instance 'cps-e) 'x))))
    (dotimes (i 3) (setq *cps-log* nil) (make-instance 'cps-m :x 1) (setq none (reverse *cps-log*)))
    (defmethod shared-initialize :around ((o cps-m) slot-names &key &allow-other-keys)
      (push :in *cps-log*) (prog1 (call-next-method) (push :out *cps-log*)))
    (dotimes (i 3) (setq *cps-log* nil) (make-instance 'cps-m :x 1) (setq with (reverse *cps-log*)))
    (let ((o (make-instance 'cps-m :x 1)))
      (setq *cps-log* nil)
      (reinitialize-instance o :x 2)
      (let ((re (reverse *cps-log*)))
        (setq *cps-log* nil)
        (change-class o 'cps-m2)
        (setq with (list with re (reverse *cps-log*) (cps-m-x o)))))
    (remove-method #'shared-initialize
                   (find-method #'shared-initialize '(:around) (list (find-class 'cps-m) (find-class 't))))
    (dotimes (i 3) (setq *cps-log* nil) (make-instance 'cps-m :x 1) (setq without (reverse *cps-log*)))
    (list err none with without))
  ((:error 5 :error 1) nil ((:in :out) (:in :out) (:in :out) 2) nil))
