;;; (SIGNUM -0.0) is -0.0 (CLHS: a float zero gives its own zero, sign kept).

(deftest signum-negative-zero.single
  (list (eql (signum -0.0) -0.0) (eql (signum 0.0) 0.0) (minusp (float-sign (signum -0.0))))
  (t t t))

(deftest signum-negative-zero.double
  (list (eql (signum -0.0d0) -0.0d0) (eql (signum 0.0d0) 0.0d0))
  (t t))

(deftest signum-negative-zero.others
  (list (signum -3.5) (signum 2.5d0) (signum 0) (signum -7) (signum 1/2))
  (-1.0 1.0d0 0 -1 1))
