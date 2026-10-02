;;; (expt rational ratio) is a single-float, the float a rational turns into
;;; when no argument says otherwise (CLHS 12.1.4.1.1). It was a double-float.
;;; magicl's p-norm tests compare (norm x 2) of an integer vector with a
;;; single-float literal and saw 9.539392014169456d0 instead of 9.539392.

(deftest expt-rational-ratio.single
  (list (expt 4 1/2) (expt 8 2/3) (expt 1/4 1/2) (expt 1000 1/3))
  (2.0 4.0 0.5 10.0))

(deftest expt-rational-ratio.types
  (list (typep (expt 91 1/2) 'single-float)
        (typep (expt 91 0.5d0) 'double-float)
        (typep (expt 91d0 1/2) 'double-float)
        (typep (expt 91.0 1/2) 'single-float))
  (t t t t))

(deftest expt-rational-ratio.negative-base
  (let ((z (expt -8 1/3)))
    (list (typep z '(complex single-float))
          (< (abs (- z #c(1.0 1.7320508))) 1e-5)))
  (t t))

(deftest expt-rational-ratio.exact-stays-exact
  (list (expt 2 10) (expt 2/3 2) (expt 2 -2))
  (1024 4/9 1/4))
