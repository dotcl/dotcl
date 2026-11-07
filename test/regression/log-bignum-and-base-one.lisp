;;; LOG of a number too large for a double (a bignum, a ratio of bignums) is computed from the
;;; integer's size instead of overflowing to infinity, with or without a base; (log x 1) is a
;;; DIVISION-BY-ZERO (log 1 = 0), not NaN.

(defun %lbb-near (a b) (< (abs (- a b)) (* 1d-4 (max 1 (abs b)))))

(deftest log-bignum-and-base-one.large-values
  (list (%lbb-near (log (expt 10 400)) 921.034d0)
        (%lbb-near (log (expt 10 400) 10) 400d0)
        (%lbb-near (log (expt 2 2000) 2) 2000d0)
        (%lbb-near (log (/ 1 (expt 10 400))) -921.034d0)
        (%lbb-near (log (expt 10 400) (expt 10 200)) 2d0)
        (let ((z (log (- (expt 10 400))))) (and (complexp z) (%lbb-near (realpart z) 921.034d0)
                                                (%lbb-near (imagpart z) pi))))
  (t t t t t t))

(deftest log-bignum-and-base-one.unchanged
  (list (%lbb-near (log (expt 10 30)) 69.0775527d0) (log 8 2) (log 100 10) (log 1) (log 0.0d0 0)
        (%lbb-near (log 2.5d0) 0.91629073d0))
  (t 3.0 2.0 0.0 0.0d0 t))

(deftest log-bignum-and-base-one.base-one
  (list (handler-case (log 2 1) (division-by-zero () :division-by-zero))
        (handler-case (log 2.0d0 1) (division-by-zero () :division-by-zero)))
  (:division-by-zero :division-by-zero))
