;;; FFLOOR / FCEILING / FTRUNCATE / FROUND of rationals keep the sign of a zero
;;; quotient: (ftruncate -1 100) is -0.0, as for float arguments.
;;; Expected values are what SBCL returns.

(deftest ftruncate-rational-negative-zero.truncate
  (list (ftruncate -1/2) (ftruncate 1 -100) (ftruncate -1 100) (ftruncate 0 -5) (ftruncate 1 100))
  (-0.0 -0.0 -0.0 -0.0 0.0))

(deftest ftruncate-rational-negative-zero.others
  (list (fceiling -1/2) (fceiling -1 3) (fround -1/2) (fround 1 -4) (fround -1 4)
        (ffloor 1/2 3) (fround 1/4))
  (-0.0 -0.0 -0.0 -0.0 -0.0 0.0 0.0))

(deftest ftruncate-rational-negative-zero.nonzero-unchanged
  (list (ftruncate 7 2) (ftruncate -7 2) (ffloor -1/3) (fceiling 1/3) (fround 5 2) (fround -5 2))
  (3.0 -3.0 -1.0 1.0 2.0 -2.0))
