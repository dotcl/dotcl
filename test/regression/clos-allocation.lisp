;;; Allocation is asserted only on emitted code, so these tests are compiled-only.

(defparameter *cloa-sink* nil)
(defparameter *cloa-iterations* 1000)

(defun %cloa-baseline ()
  (min-allocated-bytes (lambda ()
                         (dotimes (i *cloa-iterations*)
                           (setq *cloa-sink* nil)))
                       5))

(defun %cloa-failures (rows)
  (let ((base (%cloa-baseline)) (bad nil))
    (dolist (row rows (nreverse bad))
      (funcall (caddr row))
      (let ((got (- (min-allocated-bytes (caddr row) 5) base))
            (limit (* *cloa-iterations* (+ (cadr row) 16))))
        (when (> got limit)
          (push (list (car row) (/ got (float *cloa-iterations*)) (cadr row)) bad))))))

(defmacro %cloa-row (label budget form)
  `(list ,label ,budget
         (lambda ()
           (dotimes (i *cloa-iterations*)
             (setq *cloa-sink* ,form)))))

;;; --- classes ---

(defclass cloa-base () ())

(defclass cloa-mid (cloa-base) ())

(defclass cloa-sub (cloa-mid)
  ((f :type fixnum :initarg :f :initform 1 :reader cloa-f)
   (d :type double-float :initarg :d :initform 1.5d0 :accessor cloa-d)
   (x :type t :initarg :x :initform :x :accessor cloa-x :writer cloa-x-writer)))

(defclass cloa-unbound () ((u :type t)))

(defclass cloa-init-call ()
  ((v :initform (%cloa-init-value) :reader cloa-init-v)))

(defun %cloa-init-value () 7)

(defclass cloa-after () ((a :initarg :a :initform 1 :reader cloa-after-a)))
(defmethod initialize-instance :after ((o cloa-after) &key a)
  (declare (ignore a))
  (setq *cloa-sink* o))

(defstruct (cloa-struct (:constructor %make-cloa-struct (f d x)))
  (f 1 :type fixnum)
  (d 1.5d0 :type double-float)
  (x :x))

;;; --- objects built outside every thunk ---

(defvar *cloa-sub-obj* (make-instance 'cloa-sub))
(defvar *cloa-struct-obj* (%make-cloa-struct 1 1.5d0 :x))
(defvar *cloa-class* (find-class 'cloa-sub))
(defvar *cloa-class-name* 'cloa-sub)
(defvar *cloa-slot-name* 'f)
(defvar *cloa-unbound-obj* (allocate-instance (find-class 'cloa-unbound)))
(defvar *cloa-reinit-obj* (make-instance 'cloa-sub))
(defvar *cloa-eql-obj* (make-instance 'cloa-sub))

;;; --- accessors ---

(defun %cloa-reader-f (o) (cloa-f o))
(defun %cloa-accessor-d (o) (cloa-d o))
(defun %cloa-the-d (o) (the double-float (cloa-d o)))
(defun %cloa-accessor-x (o) (cloa-x o))
(defun %cloa-setf-accessor-x (o) (setf (cloa-x o) :x))
(defun %cloa-writer-x (o) (cloa-x-writer :x o))
(defun %cloa-slot-value-const (o) (slot-value o 'f))
(defun %cloa-setf-slot-value-const (o) (setf (slot-value o 'f) 1))
(defun %cloa-slot-value-var (o) (slot-value o *cloa-slot-name*))
(defun %cloa-slot-boundp (o) (slot-boundp o 'f))
(defun %cloa-with-slots-1 (o) (with-slots (f) o f))
(defun %cloa-with-slots-3 (o)
  (with-slots (f d x) o
    (logand (+ f (if x 1 0) (if (plusp d) 2 0)) #xff)))
(defun %cloa-with-accessors-1 (o) (with-accessors ((f cloa-f)) o f))
(defun %cloa-with-accessors-3 (o)
  (with-accessors ((f cloa-f) (d cloa-d) (x cloa-x)) o
    (logand (+ f (if x 1 0) (if (plusp d) 2 0)) #xff)))
(defun %cloa-struct-f (o) (cloa-struct-f o))

;;; --- method combination, one argument ---

(defgeneric %cloa1-primary (x))
(defmethod %cloa1-primary ((x cloa-base)) 1)

(defgeneric %cloa1-before (x))
(defmethod %cloa1-before ((x cloa-base)) 2)
(defmethod %cloa1-before :before ((x cloa-base)) (setq *cloa-sink* 1))

(defgeneric %cloa1-after (x))
(defmethod %cloa1-after ((x cloa-base)) 3)
(defmethod %cloa1-after :after ((x cloa-base)) (setq *cloa-sink* 1))

(defgeneric %cloa1-cnm (x))
(defmethod %cloa1-cnm ((x cloa-base)) 6)
(defmethod %cloa1-cnm ((x cloa-sub)) (1+ (call-next-method)))

(defgeneric %cloa1-nmp (x))
(defmethod %cloa1-nmp ((x cloa-base)) 7)
(defmethod %cloa1-nmp ((x cloa-sub)) (if (next-method-p) (call-next-method) 0))

(defgeneric %cloa1-chain (x))
(defmethod %cloa1-chain ((x cloa-base)) 100)
(defmethod %cloa1-chain ((x cloa-mid)) (+ 10 (call-next-method)))
(defmethod %cloa1-chain ((x cloa-sub)) (+ 1 (call-next-method)))

(defgeneric %cloa1-eql (x))
(defmethod %cloa1-eql ((x cloa-base)) 0)
(defmethod %cloa1-eql ((x (eql *cloa-eql-obj*))) 8)

(defgeneric %cloa1-key (x &key a b))
(defmethod %cloa1-key ((x cloa-base) &key (a 0) (b 0)) (+ a b))

(defgeneric %cloa1-opt (x &optional y))
(defmethod %cloa1-opt ((x cloa-base) &optional (y 0)) (+ y 1))

(defun %cloa1-r-primary (x) (%cloa1-primary x))
(defun %cloa1-r-before (x) (%cloa1-before x))
(defun %cloa1-r-after (x) (%cloa1-after x))
(defun %cloa1-r-cnm (x) (%cloa1-cnm x))
(defun %cloa1-r-nmp (x) (%cloa1-nmp x))
(defun %cloa1-r-chain (x) (%cloa1-chain x))
(defun %cloa1-r-eql (x) (%cloa1-eql x))
(defun %cloa1-r-key-0 (x) (%cloa1-key x))
(defun %cloa1-r-opt-0 (x) (%cloa1-opt x))
(defun %cloa1-r-opt-1 (x) (%cloa1-opt x 1))

;;; --- method combination, two arguments ---

(defgeneric %cloa2-primary (x y))
(defmethod %cloa2-primary ((x cloa-base) y) (declare (ignore y)) 1)

(defgeneric %cloa2-before (x y))
(defmethod %cloa2-before ((x cloa-base) y) (declare (ignore y)) 2)
(defmethod %cloa2-before :before ((x cloa-base) y) (declare (ignore y)) (setq *cloa-sink* 1))

(defgeneric %cloa2-after (x y))
(defmethod %cloa2-after ((x cloa-base) y) (declare (ignore y)) 3)
(defmethod %cloa2-after :after ((x cloa-base) y) (declare (ignore y)) (setq *cloa-sink* 1))

(defgeneric %cloa2-cnm (x y))
(defmethod %cloa2-cnm ((x cloa-base) y) (declare (ignore y)) 6)
(defmethod %cloa2-cnm ((x cloa-sub) y) (declare (ignore y)) (1+ (call-next-method)))

(defgeneric %cloa2-nmp (x y))
(defmethod %cloa2-nmp ((x cloa-base) y) (declare (ignore y)) 7)
(defmethod %cloa2-nmp ((x cloa-sub) y)
  (declare (ignore y))
  (if (next-method-p) (call-next-method) 0))

(defgeneric %cloa2-chain (x y))
(defmethod %cloa2-chain ((x cloa-base) y) (declare (ignore y)) 100)
(defmethod %cloa2-chain ((x cloa-mid) y) (declare (ignore y)) (+ 10 (call-next-method)))
(defmethod %cloa2-chain ((x cloa-sub) y) (declare (ignore y)) (+ 1 (call-next-method)))

(defgeneric %cloa2-key (x y &key a b))
(defmethod %cloa2-key ((x cloa-base) y &key (a 0) (b 0)) (declare (ignore y)) (+ a b))

(defgeneric %cloa2-opt (x y &optional z))
(defmethod %cloa2-opt ((x cloa-base) y &optional (z 0)) (declare (ignore y)) (+ z 1))

(defun %cloa2-r-primary (x y) (%cloa2-primary x y))
(defun %cloa2-r-before (x y) (%cloa2-before x y))
(defun %cloa2-r-after (x y) (%cloa2-after x y))
(defun %cloa2-r-cnm (x y) (%cloa2-cnm x y))
(defun %cloa2-r-nmp (x y) (%cloa2-nmp x y))
(defun %cloa2-r-chain (x y) (%cloa2-chain x y))
(defun %cloa2-r-key-0 (x y) (%cloa2-key x y))
(defun %cloa2-r-opt-0 (x y) (%cloa2-opt x y))
(defun %cloa2-r-opt-1 (x y) (%cloa2-opt x y 1))

;;; --- MAKE-INSTANCE and friends ---

(defun %cloa-make-0 () (make-instance 'cloa-sub))
(defun %cloa-make-1 () (make-instance 'cloa-sub :f 2))
(defun %cloa-make-3 () (make-instance 'cloa-sub :f 2 :d 2.5d0 :x :y))
(defun %cloa-make-class-obj () (make-instance *cloa-class*))
(defun %cloa-make-class-name () (make-instance *cloa-class-name* :f 2))
(defun %cloa-make-after () (make-instance 'cloa-after :a 3))
(defun %cloa-make-init-call () (make-instance 'cloa-init-call))
(defun %cloa-allocate (class) (allocate-instance class))
(defun %cloa-reinitialize (o) (reinitialize-instance o :f 3))
(defun %cloa-class-of (o) (class-of o))
(defun %cloa-typep-name (o) (if (typep o 'cloa-sub) 1 0))
(defun %cloa-typep-class (o) (if (typep o *cloa-class*) 1 0))
(defun %cloa-setf-unbound (o) (setf (slot-value o 'u) 1))

;;; --- tests ---

(deftest-compiled-only clos-allocation.accessors
  (%cloa-failures
   (list
    (%cloa-row "accessor/reader-fixnum" 0 (%cloa-reader-f *cloa-sub-obj*))
    (%cloa-row "accessor/accessor-double" 24 (%cloa-accessor-d *cloa-sub-obj*))
    (%cloa-row "accessor/the-double" 24 (%cloa-the-d *cloa-sub-obj*))
    (%cloa-row "accessor/accessor-t" 0 (%cloa-accessor-x *cloa-sub-obj*))
    (%cloa-row "accessor/setf-accessor" 0 (%cloa-setf-accessor-x *cloa-sub-obj*))
    (%cloa-row "accessor/writer" 0 (%cloa-writer-x *cloa-sub-obj*))
    (%cloa-row "accessor/slot-value-constant" 0 (%cloa-slot-value-const *cloa-sub-obj*))
    (%cloa-row "accessor/setf-slot-value-constant" 0 (%cloa-setf-slot-value-const *cloa-sub-obj*))
    (%cloa-row "accessor/slot-value-variable" 0 (%cloa-slot-value-var *cloa-sub-obj*))
    (%cloa-row "accessor/slot-boundp" 0 (%cloa-slot-boundp *cloa-sub-obj*))
    (%cloa-row "accessor/with-slots-1" 0 (%cloa-with-slots-1 *cloa-sub-obj*))
    (%cloa-row "accessor/with-slots-3" 0 (%cloa-with-slots-3 *cloa-sub-obj*))
    (%cloa-row "accessor/with-accessors-1" 0 (%cloa-with-accessors-1 *cloa-sub-obj*))
    (%cloa-row "accessor/with-accessors-3" 0 (%cloa-with-accessors-3 *cloa-sub-obj*))
    (%cloa-row "accessor/defstruct" 0 (%cloa-struct-f *cloa-struct-obj*))))
  nil)

(deftest-compiled-only clos-allocation.combination-one
  (%cloa-failures
   (list
    (%cloa-row "combination/1/primary" 0 (%cloa1-r-primary *cloa-sub-obj*))
    (%cloa-row "combination/1/before" 0 (%cloa1-r-before *cloa-sub-obj*))
    (%cloa-row "combination/1/after" 0 (%cloa1-r-after *cloa-sub-obj*))
    (%cloa-row "combination/1/call-next-method" 0 (%cloa1-r-cnm *cloa-sub-obj*))
    (%cloa-row "combination/1/next-method-p" 0 (%cloa1-r-nmp *cloa-sub-obj*))
    (%cloa-row "combination/1/chain" 0 (%cloa1-r-chain *cloa-sub-obj*))
    (%cloa-row "combination/1/eql" 0 (%cloa1-r-eql *cloa-eql-obj*))
    (%cloa-row "combination/1/key-0" 0 (%cloa1-r-key-0 *cloa-sub-obj*))
    (%cloa-row "combination/1/optional-0" 0 (%cloa1-r-opt-0 *cloa-sub-obj*))
    (%cloa-row "combination/1/optional-1" 0 (%cloa1-r-opt-1 *cloa-sub-obj*))))
  nil)

(deftest-compiled-only clos-allocation.combination-two
  (%cloa-failures
   (list
    (%cloa-row "combination/2/primary" 0 (%cloa2-r-primary *cloa-sub-obj* 1))
    (%cloa-row "combination/2/before" 0 (%cloa2-r-before *cloa-sub-obj* 1))
    (%cloa-row "combination/2/after" 0 (%cloa2-r-after *cloa-sub-obj* 1))
    (%cloa-row "combination/2/call-next-method" 0 (%cloa2-r-cnm *cloa-sub-obj* 1))
    (%cloa-row "combination/2/next-method-p" 0 (%cloa2-r-nmp *cloa-sub-obj* 1))
    (%cloa-row "combination/2/chain" 0 (%cloa2-r-chain *cloa-sub-obj* 1))
    (%cloa-row "combination/2/key-0" 0 (%cloa2-r-key-0 *cloa-sub-obj* 1))
    (%cloa-row "combination/2/optional-0" 0 (%cloa2-r-opt-0 *cloa-sub-obj* 1))
    (%cloa-row "combination/2/optional-1" 0 (%cloa2-r-opt-1 *cloa-sub-obj* 1))))
  nil)

(deftest-compiled-only clos-allocation.make-instance
  (%cloa-failures
   (list
    (%cloa-row "make-instance/allocate" 120 (%cloa-allocate *cloa-class*))
    (%cloa-row "make-instance/0" 120 (%cloa-make-0))
    (%cloa-row "make-instance/1" 120 (%cloa-make-1))
    (%cloa-row "make-instance/3" 120 (%cloa-make-3))
    (%cloa-row "make-instance/class-object" 120 (%cloa-make-class-obj))
    (%cloa-row "make-instance/class-name" 120 (%cloa-make-class-name))
    (%cloa-row "make-instance/initform-call" 120 (%cloa-make-init-call))
    (%cloa-row "make-instance/reinitialize" 120 (%cloa-reinitialize *cloa-reinit-obj*))
    (%cloa-row "make-instance/class-of" 0 (%cloa-class-of *cloa-sub-obj*))
    (%cloa-row "make-instance/typep-name" 0 (%cloa-typep-name *cloa-sub-obj*))
    (%cloa-row "make-instance/typep-class" 0 (%cloa-typep-class *cloa-sub-obj*))
    (%cloa-row "make-instance/setf-unbound" 0 (%cloa-setf-unbound *cloa-unbound-obj*))))
  nil)

(deftest clos-allocation.values
  (let ((o (make-instance 'cloa-sub)))
    (list (cloa-f o) (cloa-d o) (cloa-x o)
          (progn (setf (cloa-x o) :y) (cloa-x o))
          (slot-value o 'f)
          (progn (cloa-x-writer :z o) (cloa-x o))
          (%cloa1-opt o) (%cloa1-opt o 2)
          (%cloa1-cnm o) (%cloa1-nmp o)
          (%cloa1-chain o)
          (cloa-f (%cloa-make-1))))
  (1 1.5d0 :x :y 1 :z 1 3 7 7 111 2))
