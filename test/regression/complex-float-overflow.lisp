;;; A complex made of an integer or ratio too large for the float format of the other part signals
;;; FLOATING-POINT-OVERFLOW; the parts that fit give the complex they always did.

(defmacro %cfo-overflow-p (form)
  `(handler-case (progn ,form nil) (floating-point-overflow () t) (error () :other-error)))

(deftest complex-float-overflow.signals
  (list (%cfo-overflow-p (complex (expt 2 128) 1.0))
        (%cfo-overflow-p (complex 1.0 (expt 2 128)))
        (%cfo-overflow-p (complex (expt 2 1100) 1d0))
        (%cfo-overflow-p (complex 1d0 (expt 10 400)))
        (%cfo-overflow-p (complex (/ (expt 10 400) 3) 1.0f0))
        (%cfo-overflow-p (coerce (expt 2 128) '(complex single-float)))
        (%cfo-overflow-p (coerce (expt 10 400) '(complex double-float)))
        (%cfo-overflow-p (coerce (complex (expt 2 128) 1) '(complex single-float)))
        (%cfo-overflow-p (coerce (complex 1 (expt 10 400)) '(complex double-float)))
        (%cfo-overflow-p (* (complex (expt 2 128) 1) 1.0))
        (%cfo-overflow-p (+ (complex (expt 10 400) 1) 1d0)))
  (t t t t t t t t t t t))

(deftest complex-float-overflow.fits
  (list (complex (expt 2 100) 1.0)
        (complex 1.0 (/ 1 (expt 10 400)))
        (coerce (expt 2 128) '(complex double-float))
        (+ (complex 1 2) 1.5)
        (complex 1 2))
  (#C(1.2676506e30 1.0) #C(1.0 0.0) #C(3.402823669209385d38 0.0d0) #C(2.5 2.0) #C(1 2)))
