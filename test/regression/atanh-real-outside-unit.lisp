;;; ATANH of a real argument beyond 1 keeps the precision of its real part.

(defun %aro-close (x y) (< (abs (- x y)) (* 4d-16 (abs y))))

(deftest atanh-real-outside-unit.large-double
  (list (%aro-close (realpart (atanh 1d10)) 1d-10)
        (%aro-close (realpart (atanh -1d300)) -1d-300)
        (%aro-close (realpart (atanh 1d300)) 1d-300)
        (imagpart (atanh 1d10))
        (imagpart (atanh -1d10)))
  (t t t 1.5707963267948966d0 1.5707963267948966d0))

(deftest atanh-real-outside-unit.moderate
  (list (%aro-close (realpart (atanh 2d0)) 0.5493061443340549d0)
        (%aro-close (realpart (atanh -2d0)) -0.5493061443340549d0))
  (t t))
