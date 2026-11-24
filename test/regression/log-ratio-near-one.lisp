;;; LOG of a ratio (or bignum) close to 1 keeps the digits of its distance from 1 instead of rounding to 1.0 first.

(defun %lrn-close (x expected &optional (rel 1e-5))
  (and (realp x) (< (abs (- x expected)) (* rel (abs expected)))))

(deftest log-ratio-near-one.positive
  (list (%lrn-close (log (+ 1 (/ 1 (expt 2 106)))) 1.2325952e-32)
        (%lrn-close (log (+ 1 (/ 1 (expt 10 30)))) 1.0e-30)
        (%lrn-close (log (- 1 (/ 1 (expt 10 30)))) -1.0e-30)
        (%lrn-close (log (+ 1 (/ 1 (expt 2 30)))) 9.313226e-10)
        (>= (log (/ (1+ (expt 10 400)) (expt 10 400))) 0.0))
  (t t t t t))

(deftest log-ratio-near-one.exactly-one-and-far
  (list (log 1) (%lrn-close (log 3/2) 0.4054651) (%lrn-close (log 1/3) -1.0986123)
        (%lrn-close (log 5/3) 0.5108256) (%lrn-close (log 1/1000) -6.9077554)
        (%lrn-close (log (expt 10 500)) 1151.2925))
  (0.0 t t t t t))

(deftest log-ratio-near-one.negative
  (let ((z (log (- (/ (1+ (expt 10 30)) (expt 10 30))))))
    (list (complexp z) (%lrn-close (realpart z) 1.0e-30) (%lrn-close (imagpart z) 3.1415927)))
  (t t t))

(deftest log-ratio-near-one.base
  (list (%lrn-close (log (+ 1 (/ 1 (expt 2 106))) 2) 1.7782589e-32)
        (%lrn-close (log 8 2) 3.0) (%lrn-close (log 1/8 2) -3.0) (%lrn-close (log 10 1/10) -1.0))
  (t t t t))
