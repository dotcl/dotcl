;;; (expt -0.0 y) keeps the sign of the zero only for an odd integer power
;;; (pow(-0, y) is -0 there and +0 otherwise).

(deftest expt-negative-zero-base.even-and-fractional
  (values (eql (expt -0.0 2) 0.0) (eql (expt -0.0 4) 0.0) (eql (expt -0.0d0 2) 0.0d0)
          (eql (expt -0.0 2.0) 0.0) (eql (expt -0.0 2.5) 0.0) (eql (expt -0.0 0.5) 0.0)
          (eql (expt -0.0 10) 0.0) (eql (expt -0.0 100) 0.0))
  t t t t t t t t)

(deftest expt-negative-zero-base.odd-keeps-sign
  (values (eql (expt -0.0 1) -0.0) (eql (expt -0.0 3) -0.0) (eql (expt -0.0 3.0) -0.0)
          (eql (expt -0.0d0 5) -0.0d0) (eql (expt -0.0 101) -0.0))
  t t t t t)

(deftest expt-negative-zero-base.positive-zero-and-others
  (values (eql (expt 0.0 2) 0.0) (eql (expt 0.0 3) 0.0) (eql (expt 0 2) 0) (eql (expt 0 1/2) 0)
          (eql (expt -2.0 2) 4.0) (eql (expt -2.0 3) -8.0) (eql (expt -0.0 0) 1.0))
  t t t t t t t)
