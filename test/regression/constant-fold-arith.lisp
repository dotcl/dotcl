;;; A call of a pure arithmetic function whose arguments are all rational
;;; literals (or constants, or such calls) is computed at compile time. The
;;; folded value must be the one the call returns at run time, a call that
;;; signals is left to run time, and float arithmetic is not folded.

(defparameter *cfa-cases*
  '((expt 2 64) (expt 2 -3) (expt 2/3 5) (expt -3 7) (expt 0 0) (expt 7 0)
    (ash 1 70) (ash -12345678901234567890 -7) (ash 5 0)
    (logand -1 #xffffffffffffffffff) (logior 1 2 4) (logxor -5 9) (lognot 12345)
    (logeqv 3 5) (lognand 3 5) (lognor 3 5) (logandc1 3 5) (logandc2 3 5)
    (logorc1 3 5) (logorc2 3 5) (logcount -7) (integer-length (expt 2 100))
    (+ (expt 2 63) 1) (- (expt 2 63)) (- 1/2 1/3) (* 12345678901 98765432109)
    (/ 10 4) (/ 3) (1+ 9223372036854775807) (1- -9223372036854775808)
    (floor 7 2) (floor -7 2) (ceiling 7/2) (truncate -7 2) (round 5 2) (round 7 2)
    (mod -7 3) (rem -7 3) (abs -5/3) (min 3 1/2 7) (max 3 1/2 7) (gcd 12 18)
    (lcm 4 6) (isqrt 1000001) (signum -7) (numerator 6/4) (denominator 6/4)
    (1- (expt 2 64)) (- (expt 2 31)) (+ (floor 7 2) 1)))

(deftest-emitting-only constant-fold-arith.same-as-run-time
  (loop for form in *cfa-cases*
        for folded = (multiple-value-list (funcall (compile nil `(lambda () ,form))))
        for run = (multiple-value-list
                   (apply (symbol-function (car form))
                          (mapcar #'eval (cdr form))))
        unless (equal folded run) collect (list form folded run))
  nil)

(defun %cfa-signals (form)
  (let ((fn (compile nil `(lambda () ,form))))
    (handler-case (progn (funcall fn) :no-error)
      (error (e) (type-of e)))))

(deftest-emitting-only constant-fold-arith.error-left-to-run-time
  (list (%cfa-signals '(expt 0 -1))
        (%cfa-signals '(floor 1 0))
        (%cfa-signals '(/ 5 0))
        (%cfa-signals '(mod 5 0))
        (and (typep (%cfa-signals '(isqrt -1)) '(and symbol (not (eql :no-error)))) t))
  (division-by-zero division-by-zero division-by-zero division-by-zero t))

(deftest-emitting-only constant-fold-arith.floats-and-shadowing
  (list (funcall (compile nil '(lambda () (expt 2.0 3))))
        (funcall (compile nil '(lambda () (* 1/2 0.5d0))))
        (flet ((expt (a b) (list a b))) (expt 2 3))
        (= (funcall (compile nil '(lambda () (expt 3 5000)))) (expt 3 5000)))
  (8.0 0.25d0 (2 3) t))

(defconstant +cfa-shift+ 10)

(setf dotcl:*save-sil* t)
(defun %cfa-sil (fn) (%flat-text (dotcl:function-sil fn)))
(defun %cfa-u64-max () (1- (expt 2 64)))
(defun %cfa-k () (ash 1 +cfa-shift+))
(defun %cfa-huge () (expt 3 5000))
(deftest-emitting-only constant-fold-arith.folded-in-sil
  (list (and (search "18446744073709551615" (%cfa-sil #'%cfa-u64-max)) t)
        (and (search "Expt" (%cfa-sil #'%cfa-u64-max)) t)
        (and (search "Ash" (%cfa-sil #'%cfa-k)) t)
        (and (search "Expt" (%cfa-sil #'%cfa-huge)) t))
  (t nil nil t))
(setf dotcl:*save-sil* nil)

(deftest-emitting-only constant-fold-arith.compile-file
  (let ((src (regression-temp-file "cfa-fold.lisp"))
        (fasl (regression-temp-file "cfa-fold.fasl")))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-line "(defun cfa-fasl-values () (list (1- (expt 2 64)) (- (expt 2 63)) (/ 1 3) (multiple-value-list (floor 7 2))))" s))
    (let ((*error-output* (make-broadcast-stream)))
      (load (compile-file src :output-file fasl)))
    (funcall 'cfa-fasl-values))
  (18446744073709551615 -9223372036854775808 1/3 (3 1)))
