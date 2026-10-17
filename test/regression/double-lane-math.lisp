;;; SIN, COS, TAN, ATAN, SINH, COSH, TANH and EXP of a double, and FLOAT /
;;; COERCE of a fixnum or a float to a double, are computed raw inside declared
;;; double arithmetic. They used to be generic calls: the argument was boxed to
;;; make the call, the result came back boxed, and the arithmetic around them
;;; fell off the double lane with them.
;;;
;;; What the tests pin: the shape (the raw entry is called and the loop boxes
;;; nothing), the answers agree with the generic entries, EXP still signals
;;; overflow, and the shapes that can give a complex (LOG, ASIN of a double)
;;; or are not a conversion to double stay generic.

(setf dotcl:*save-sil* t)

(defun %dlm-sil (fn) (%flat-text (dotcl:function-sil fn)))
(defun %dlm-has (needle fn) (and (search needle (%dlm-sil fn)) t))

(defun %dlm-trig (x)
  (declare (double-float x))
  (+ (sin x) (cos x) (tan x) (atan x) (sinh x) (cosh x) (tanh x) (exp x)))

(defun %dlm-float-sum (n)
  (declare (fixnum n))
  (let ((acc 0d0))
    (declare (double-float acc))
    (dotimes (i n acc)
      (setq acc (+ acc (float i 1d0) (coerce i 'double-float))))))

(defun %dlm-sin-sum (n x)
  (declare (fixnum n) (double-float x))
  (let ((acc 0d0))
    (declare (double-float acc))
    (dotimes (i n acc)
      (setq acc (+ acc (sin x) (tanh (float i 1d0)))))))

(defun %dlm-widen (s)
  (declare (single-float s))
  (+ (float s 1d0) (coerce s 'double-float) 0d0))

(defun %dlm-float-of-double (x)
  (declare (double-float x))
  (+ (float x) 1d0))

(defun %dlm-big (i)
  (declare (fixnum i))
  (+ (float i 1d0) 0d0))

(defun %dlm-exp (x)
  (declare (double-float x))
  (+ (exp x) 0d0))

(defun %dlm-log (x)
  (declare (double-float x))
  (+ (log x) 0d0))

(defun %dlm-asin (x)
  (declare (double-float x))
  (+ (asin x) 0d0))

(defun %dlm-generic (x)
  (+ (sin x) 0d0))

(deftest-emitting-only double-lane-math.shape
  (list (%dlm-has "Math.SinDouble" #'%dlm-trig)
        (%dlm-has "Math.TanhDouble" #'%dlm-trig)
        (%dlm-has "Runtime.ExpDouble" #'%dlm-trig)
        (%dlm-has "Runtime.Add" #'%dlm-trig)
        (%dlm-has "Runtime.Add" #'%dlm-float-sum)
        (%dlm-has "Runtime.Add" #'%dlm-sin-sum)
        (%dlm-has "Runtime.Add" #'%dlm-widen)
        (%dlm-has "Runtime.Add" #'%dlm-float-of-double)
        (%dlm-has "Math.SinDouble" #'%dlm-generic))
  (t t t nil nil nil nil nil nil))

(deftest-emitting-only double-lane-math.no-allocation
  (progn
    (%dlm-float-sum 1000)
    (%dlm-sin-sum 1000 0.5d0)
    (< (min-allocated-bytes (lambda ()
                              (%dlm-float-sum 100000)
                              (%dlm-sin-sum 100000 0.5d0)))
       10000))
  t)

(deftest double-lane-math.values
  (let ((x 0.7d0))
    (list (= (%dlm-trig x)
             (+ (sin x) (cos x) (tan x) (atan x) (sinh x) (cosh x) (tanh x)
                (exp x)))
          (%dlm-float-sum 10)
          (= (%dlm-sin-sum 3 x)
             (let ((a 0d0))
               (setq a (+ a (sin x) (tanh 0d0)))
               (setq a (+ a (sin x) (tanh 1d0)))
               (+ a (sin x) (tanh 2d0))))
          (%dlm-widen 1.5f0)
          (%dlm-float-of-double 2d0)
          (= (%dlm-big (1+ (expt 2 53))) (float (1+ (expt 2 53)) 1d0))
          (= (%dlm-big most-positive-fixnum) (float most-positive-fixnum 1d0))
          (%dlm-exp -1000d0)))
  (t 90d0 t 3d0 3d0 t t 0d0))

(deftest double-lane-math.exp-overflow-signals
  (handler-case (progn (%dlm-exp 1000d0) :no-error)
    (floating-point-overflow () :overflow))
  :overflow)

(deftest double-lane-math.complex-results-stay-generic
  (list (complexp (%dlm-log -1d0))
        (complexp (%dlm-asin 2d0))
        (%dlm-log 1d0))
  (t t 0d0))

;;; LOG of a double follows SQRT's rule: raw only where the program declares the
;;; result a DOUBLE-FLOAT (the logarithm of a negative double is a COMPLEX), and
;;; from (safety 1) up a negative argument is then the TYPE-ERROR of a false
;;; declaration.

(defun %dlm-log-the (x)
  (declare (double-float x))
  (+ (the double-float (log x)) 0d0))

(defun %dlm-log-the-unsafe (x)
  (declare (double-float x) (optimize (safety 0)))
  (+ (the double-float (log x)) 0d0))

(deftest-emitting-only double-lane-math.log-shape
  (list (%dlm-has "Runtime.LogDoubleChecked" #'%dlm-log-the)
        (%dlm-has "Runtime.LogDouble" #'%dlm-log-the-unsafe)
        (%dlm-has "Runtime.LogDoubleChecked" #'%dlm-log-the-unsafe)
        (%dlm-has "LogDouble" #'%dlm-log))
  (t t nil nil))

(deftest double-lane-math.log-values
  (list (= (%dlm-log-the 2.5d0) (log 2.5d0))
        (%dlm-log-the 1d0)
        (handler-case (progn (%dlm-log-the 0d0) :no-error)
          (division-by-zero () :division-by-zero))
        (handler-case (progn (%dlm-log-the-unsafe 0d0) :no-error)
          (division-by-zero () :division-by-zero))
        (handler-case (progn (%dlm-log-the -1d0) :no-error)
          (type-error () :type-error)))
  (t 0d0 :division-by-zero :division-by-zero :type-error))
