;;; RATIONALIZE of a float with no fraction part is that integer.

(deftest rationalize.integer-valued-single
  (list (rationalize 1.0e10) (rationalize -1.0e10) (rationalize 16777216.0)
        (rationalize 1.0e20))
  (10000000000 -10000000000 16777216 100000002004087734272))

(deftest rationalize.integer-valued-double
  (list (rationalize 1d15) (rationalize -3d0) (rationalize 1d23))
  (1000000000000000 -3 99999999999999991611392))

(deftest rationalize.fractions-unchanged
  (list (rationalize 0.1) (rationalize 3.14159d0) (rationalize 0.5) (rationalize 0))
  (1/10 314159/100000 1/2 0))
