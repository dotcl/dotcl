;;; EXPT with a complex power returns a floating-point complex even when its
;;; imaginary part is 0.0: (expt 1 #c(1 1)) is #C(1.0 0.0), not 1.0. It collapsed
;;; the result to a real, as only a rational complex may be.

(deftest expt-complex-power-keeps-complex.one-base
  (list (expt 1 #c(1 1))
        (expt 1.0 #c(1 1))
        (expt 1.0d0 #c(1 1))
        (expt 1 #c(0 1))
        (complexp (expt 1 #c(1 1))))
  (#c(1.0 0.0) #c(1.0 0.0) #c(1.0d0 0.0d0) #c(1.0 0.0) t))

(deftest expt-complex-power-keeps-complex.unchanged-cases
  (list (expt 1 1.5)
        (expt 2 3)
        (expt #c(0 1) 2)
        (complexp (expt 2 #c(0 1)))
        (expt 0 #c(1 1)))
  (1.0 8 -1 t 0))
