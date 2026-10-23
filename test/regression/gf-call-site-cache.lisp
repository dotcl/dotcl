;;; One-argument calls of a generic function written in a compiled body go through a
;;; cache of the call site. The calls below warm such a site, then change the methods,
;;; the classes, the function the name calls, or the class of the argument, and the
;;; site must follow.

(defgeneric gcs-kind (x))
(defmethod gcs-kind ((x integer)) :integer)
(defmethod gcs-kind ((x t)) :t)

(defun gcs-call-kind (x) (list (gcs-kind x)))
(defun gcs-warm (fn args) (dotimes (k 50) (dolist (a args) (funcall fn a))))

(deftest gf-call-site-cache.methods-change
  (let ((out '()))
    (gcs-warm #'gcs-call-kind '(3))
    (push (gcs-call-kind 3) out)
    (defmethod gcs-kind ((x fixnum)) :fixnum)
    (push (gcs-call-kind 3) out)
    (gcs-warm #'gcs-call-kind '(3))
    (defmethod gcs-kind ((x fixnum)) :fixnum2)
    (push (gcs-call-kind 3) out)
    (remove-method #'gcs-kind (find-method #'gcs-kind nil (list (find-class 'fixnum))))
    (push (gcs-call-kind 3) out)
    (push (gcs-call-kind 'a) out)
    (nreverse out))
  ((:integer) (:fixnum) (:fixnum2) (:integer) (:t)))

(defgeneric gcs-poly (x))
(defmethod gcs-poly ((x integer)) 'i)
(defmethod gcs-poly ((x symbol)) 's)
(defmethod gcs-poly ((x string)) 'str)
(defmethod gcs-poly ((x cons)) 'c)
(defmethod gcs-poly ((x t)) 'other)
(defun gcs-call-poly (x) (gcs-poly x))

(deftest gf-call-site-cache.polymorphic
  (let ((args (list 1 'a "s" '(1) 2.0 3 'b)))
    (gcs-warm #'gcs-call-poly args)
    (prog1 (mapcar #'gcs-call-poly args)
      (defmethod gcs-poly ((x float)) 'f)))
  (i s str c other i s))

(deftest gf-call-site-cache.polymorphic-after-change
  (mapcar #'gcs-call-poly (list 1 'a "s" '(1) 2.0 3 'b))
  (i s str c f i s))

(defclass gcs-p () ())
(defclass gcs-q () ())
(defclass gcs-r (gcs-p) ())
(defgeneric gcs-who (x))
(defmethod gcs-who ((x gcs-p)) :p)
(defmethod gcs-who ((x gcs-q)) :q)
(defmethod gcs-who ((x t)) :other)
(defun gcs-call-who (x) (gcs-who x))

(deftest gf-call-site-cache.change-class
  (let ((x (make-instance 'gcs-p)) (out '()))
    (gcs-warm #'gcs-call-who (list x))
    (push (gcs-call-who x) out)
    (change-class x 'gcs-q)
    (push (gcs-call-who x) out)
    (change-class x 'standard-object)
    (push (gcs-call-who x) out)
    (change-class x 'gcs-p)
    (push (gcs-call-who x) out)
    (nreverse out))
  (:p :q :other :p))

(deftest gf-call-site-cache.class-redefinition
  (let ((r (make-instance 'gcs-r)) (out '()))
    (gcs-warm #'gcs-call-who (list r))
    (push (gcs-call-who r) out)
    (defclass gcs-r (gcs-q) ())
    (push (gcs-call-who r) out)
    (gcs-warm #'gcs-call-who (list r))
    (defclass gcs-r (gcs-p) ())
    (push (gcs-call-who r) out)
    (defmethod gcs-who ((x gcs-r)) :r)
    (push (gcs-call-who r) out)
    (nreverse out))
  (:p :q :p :r))

(defstruct gcs-s a)
(defstruct (gcs-s2 (:include gcs-s)) b)
(defgeneric gcs-st (x))
(defmethod gcs-st ((x gcs-s)) :s)
(defmethod gcs-st ((x t)) :t)
(defun gcs-call-st (x) (gcs-st x))

(deftest gf-call-site-cache.structures
  (let ((s (make-gcs-s)) (s2 (make-gcs-s2)) (out '()))
    (gcs-warm #'gcs-call-st (list s))
    (push (gcs-call-st s) out)
    (push (gcs-call-st s2) out)
    (defmethod gcs-st ((x gcs-s2)) :s2)
    (push (gcs-call-st s2) out)
    (push (gcs-call-st s) out)
    (nreverse out))
  (:s :s :s2 :s))

(defgeneric gcs-fib (x))
(defmethod gcs-fib ((x (eql 0))) 1)
(defmethod gcs-fib ((x (eql 1))) 1)
(defmethod gcs-fib (x) (+ (gcs-fib (- x 1)) (gcs-fib (- x 2))))

(defgeneric gcs-sym (x))
(defmethod gcs-sym ((x (eql :a))) 1)
(defmethod gcs-sym ((x symbol)) 2)
(defun gcs-call-sym (x) (gcs-sym x))

(deftest gf-call-site-cache.eql
  (let ((out '()))
    (push (gcs-fib 15) out)
    (let ((m (defmethod gcs-fib ((x (eql 2))) 10)))
      (push (gcs-fib 15) out)
      (remove-method #'gcs-fib m))
    (push (gcs-fib 15) out)
    (gcs-warm #'gcs-call-sym '(:a :b))
    (push (mapcar #'gcs-call-sym '(:a :b :c)) out)
    (defmethod gcs-sym ((x (eql :b))) 3)
    (push (mapcar #'gcs-call-sym '(:a :b :c)) out)
    (nreverse out))
  (987 4003 987 (1 2 2) (1 3 2)))

(defgeneric gcs-redef (x))
(defmethod gcs-redef (x) (list :gf x))
(defun gcs-call-redef (x) (gcs-redef x))

(deftest gf-call-site-cache.name-redefined
  (let ((out '()))
    (gcs-warm #'gcs-call-redef '(1))
    (push (gcs-call-redef 1) out)
    (fmakunbound 'gcs-redef)
    (push (handler-case (gcs-call-redef 1) (undefined-function () :undefined)) out)
    (setf (fdefinition 'gcs-redef) (lambda (x) (list :fn x)))
    (push (gcs-call-redef 1) out)
    (nreverse out))
  ((:gf 1) :undefined (:fn 1)))

(defgeneric gcs-values (x))
(defmethod gcs-values (x) (values x (1+ x) (+ x 2)))
(defun gcs-call-values (x) (multiple-value-list (gcs-values x)))
(defun gcs-call-values-1 (x) (list (gcs-values x)))
(defun gcs-call-values-mvb (x) (multiple-value-bind (a b) (gcs-values x) (list b a)))

(deftest gf-call-site-cache.values
  (progn
    (gcs-warm #'gcs-call-values '(1))
    (gcs-warm #'gcs-call-values-1 '(1))
    (list (gcs-call-values 1) (gcs-call-values-1 1) (gcs-call-values-mvb 1)))
  ((1 2 3) (1) (2 1)))

(defgeneric gcs-frame (x))
(defmethod gcs-frame (x) (declare (ignore x)) (subseq (dotcl:backtrace) 0 2))
(defun gcs-in-backtrace () (list (gcs-frame 1)))

(defgeneric gcs-fail (x))
(defmethod gcs-fail (x) (if (eql x 0) (error "zero") x))
(defun gcs-call-fail (x)
  (handler-case (list (gcs-fail x))
    (error () :caught)))
(defun gcs-call-throw (x)
  (catch 'gcs-tag (list (gcs-fail x) (gcs-throw x))))
(defgeneric gcs-throw (x))
(defmethod gcs-throw (x) (throw 'gcs-tag (list :thrown x)))

(deftest gf-call-site-cache.non-local-exit
  (progn
    (gcs-warm #'gcs-call-fail '(1 0))
    (gcs-warm #'gcs-call-throw '(1))
    (list (gcs-call-fail 0) (gcs-call-fail 2) (gcs-call-throw 3)
          (gcs-in-backtrace)))
  (:caught (2) (:thrown 3) (("GCS-FRAME" "GCS-IN-BACKTRACE"))))

(defgeneric gcs-deep (x))
(defmethod gcs-deep (x) (1+ (gcs-deep (1+ x))))

(deftest-emitting-only gf-call-site-cache.stack-overflow
  (handler-case (gcs-deep 0) (storage-condition () :overflow))
  :overflow)

(defgeneric gcs-none (x))
(defmethod gcs-none ((x integer)) x)
(defun gcs-call-none (x) (gcs-none x))

(deftest gf-call-site-cache.no-applicable-method
  (progn
    (gcs-warm #'gcs-call-none '(1))
    (list (gcs-call-none 2)
          (handler-case (gcs-call-none 'a) (error () :error))))
  (2 :error))

(defgeneric gcs-many (x))
(defmethod gcs-many ((x t)) (type-of x))
(defmethod gcs-many ((x integer)) :integer)
(defun gcs-call-many (x) (gcs-many x))
(defstruct gcs-m1) (defstruct gcs-m2) (defstruct gcs-m3) (defstruct gcs-m4)
(defclass gcs-c1 () ()) (defclass gcs-c2 () ())

(deftest gf-call-site-cache.many-classes
  (let ((args (list 1 (make-gcs-m1) (make-gcs-m2) (make-gcs-m3) (make-gcs-m4)
                    (make-instance 'gcs-c1) (make-instance 'gcs-c2) 'sym '(1) #\c (expt 2 70) 2d0))
        (out '()))
    (gcs-warm #'gcs-call-many (subseq args 0 3))
    (push (mapcar #'gcs-call-many (subseq args 0 3)) out)
    (defmethod gcs-many ((x gcs-m2)) :m2)
    (push (mapcar #'gcs-call-many (subseq args 0 3)) out)
    (gcs-warm #'gcs-call-many args)
    (push (mapcar #'gcs-call-many args) out)
    (defmethod gcs-many ((x gcs-c2)) :c2)
    (push (mapcar #'gcs-call-many args) out)
    (nreverse out))
  ((:integer gcs-m1 gcs-m2)
   (:integer gcs-m1 :m2)
   (:integer gcs-m1 :m2 gcs-m3 gcs-m4 gcs-c1 gcs-c2 symbol cons standard-char :integer double-float)
   (:integer gcs-m1 :m2 gcs-m3 gcs-m4 gcs-c1 :c2 symbol cons standard-char :integer double-float)))

(defgeneric gcs-mru (x))
(defmethod gcs-mru ((x (eql 7))) :seven)
(defmethod gcs-mru ((x integer)) :int)
(defmethod gcs-mru ((x symbol)) :sym)
(defun gcs-call-mru (x) (gcs-mru x))

(deftest gf-call-site-cache.usual-class-takes-over
  (progn
    (gcs-call-mru 'a)
    (gcs-call-mru 'a)
    (dotimes (k 1000) (gcs-call-mru k))
    (list (gcs-call-mru 7) (gcs-call-mru 8) (gcs-call-mru 'b)
          (let ((c 0))
            (dotimes (k 1000) (when (eq (gcs-call-mru (if (evenp k) 7 (+ k 1000))) :seven) (incf c)))
            c)))
  (:seven :int :sym 500))
