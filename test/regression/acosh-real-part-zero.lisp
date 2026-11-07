;;; ACOSH of a real in (-1, 1) has a real part of exactly zero.

(deftest acosh.open-interval-real-part
  (list (realpart (acosh 0.5)) (realpart (acosh -0.5d0)) (realpart (acosh 3/7))
        (realpart (acosh 0)) (realpart (acosh 0.9999d0)))
  (0.0 0.0d0 0.0 0.0 0.0d0))

(deftest acosh.open-interval-imag-part
  (list (< (abs (- (imagpart (acosh 0.5d0)) (acos 0.5d0))) 1d-15)
        (imagpart (acosh 0)))
  (t 1.5707964))

(deftest acosh.outside-unchanged
  (list (acosh 1.0) (> (acosh 2d0) 1.31) (realpart (acosh -2.0d0)))
  (0.0 t 1.3169578969248166d0))
