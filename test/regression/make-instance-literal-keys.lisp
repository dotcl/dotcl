;;; MAKE-INSTANCE of a literal class name with one to three literal keywords passes the
;;; pairs as arguments. The result must be what the array path gives.

(defclass %mik-base ()
  ((a :initarg :a :initform :a-default :accessor %mik-a)
   (b :initarg :b :accessor %mik-b)
   (c :initarg :c :initarg :c2 :initform 7 :accessor %mik-c)))

(defclass %mik-after (%mik-base) ((log :initform nil :accessor %mik-log)))
(defvar *mik-after-runs* 0)
(defmethod initialize-instance :after ((x %mik-after) &key) (incf *mik-after-runs*))

(defclass %mik-defaults ()
  ((d :initarg :d :accessor %mik-d))
  (:default-initargs :d 99))

(defun %mik-slots (x)
  (list (%mik-a x) (if (slot-boundp x 'b) (%mik-b x) :unbound) (%mik-c x)))

(deftest make-instance-literal-keys.plain
  (list (%mik-slots (make-instance '%mik-base :a 1))
        (%mik-slots (make-instance '%mik-base :b 2))
        (%mik-slots (make-instance '%mik-base :a 1 :b 2))
        (%mik-slots (make-instance '%mik-base :c 3 :a 1 :b 2))
        (%mik-slots (make-instance '%mik-base :c2 5))
        ;; the first of a repeated keyword wins
        (%mik-slots (make-instance '%mik-base :a 1 :a 2))
        ;; second call takes the plan path
        (%mik-slots (make-instance '%mik-base :a 1 :b 2))
        (%mik-slots (make-instance '%mik-base :a 11 :b 22 :c 33)))
  ((1 :unbound 7) (:a-default 2 7) (1 2 7) (1 2 3) (:a-default :unbound 5) (1 :unbound 7)
   (1 2 7) (11 22 33)))

(deftest make-instance-literal-keys.evaluation-order
  (let ((trace '()))
    (make-instance '%mik-base :a (progn (push 1 trace) 1) :b (progn (push 2 trace) 2))
    (reverse trace))
  (1 2))

(deftest make-instance-literal-keys.after-and-defaults
  (let ((*mik-after-runs* 0))
    (make-instance '%mik-after :a 1)
    (make-instance '%mik-after :a 1)
    (list *mik-after-runs*
          (%mik-d (make-instance '%mik-defaults))
          (%mik-d (make-instance '%mik-defaults :d 1))
          (%mik-d (make-instance '%mik-defaults :d 1))))
  (2 99 1 1))

(deftest make-instance-literal-keys.errors
  (list (handler-case (make-instance '%mik-base :zzz 1) (error () :error))
        (handler-case (make-instance '%mik-base :a 1 :zzz 1) (error () :error))
        (handler-case (progn (make-instance '%mik-base :a 1) :ok) (error () :error))
        (handler-case (make-instance '%mik-nonexistent :a 1) (error () :error)))
  (:error :error :ok :error))

(deftest make-instance-literal-keys.redefinition
  (let ((r '()))
    (make-instance '%mik-base :a 1)
    (push (%mik-slots (make-instance '%mik-base :a 1 :b 2)) r)
    (eval '(defclass %mik-base ()
            ((a :initarg :a :initform :new-a :accessor %mik-a)
             (b :initarg :b :accessor %mik-b)
             (c :initarg :c :initform 70 :accessor %mik-c)
             (e :initarg :e :initform 5))))
    (push (%mik-slots (make-instance '%mik-base :b 2)) r)
    (push (slot-value (make-instance '%mik-base :e 9) 'e) r)
    (reverse r))
  ((1 2 7) (:new-a 2 70) 9))
