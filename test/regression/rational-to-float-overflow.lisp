;;; An integer or ratio too large for the float format signals FLOATING-POINT-OVERFLOW where it
;;; is converted (FLOAT, COERCE, the transcendental functions, mixed arithmetic, EXPT) instead of
;;; becoming an infinity or a NaN; a rational is taken to single-float first by SIN, COS, TAN, EXP
;;; and the like, and to double-float by SQRT and EXPT, as the same functions do in SBCL.

(defmacro %rfo-overflow-p (form)
  `(handler-case (progn ,form nil) (floating-point-overflow () t) (error () :other-error)))

(defun %rfo-big () (expt 10 400))
(defun %rfo-tiny-ratio () (/ 1 (expt 10 400)))
(defun %rfo-big-ratio () (/ (expt 10 400) 3))

(deftest rational-to-float-overflow.signals
  (list (%rfo-overflow-p (sqrt (%rfo-big)))
        (%rfo-overflow-p (exp (%rfo-big)))
        (%rfo-overflow-p (sin (%rfo-big)))
        (%rfo-overflow-p (cos (- (%rfo-big))))
        (%rfo-overflow-p (tan (%rfo-big-ratio)))
        (%rfo-overflow-p (atan (%rfo-big)))
        (%rfo-overflow-p (asin (%rfo-big)))
        (%rfo-overflow-p (sinh (%rfo-big)))
        (%rfo-overflow-p (tanh (%rfo-big)))
        (%rfo-overflow-p (float (%rfo-big)))
        (%rfo-overflow-p (float (- (%rfo-big)) 1d0))
        (%rfo-overflow-p (float (%rfo-big-ratio) 1f0))
        (%rfo-overflow-p (coerce (%rfo-big) 'double-float))
        (%rfo-overflow-p (coerce (%rfo-big-ratio) 'single-float))
        (%rfo-overflow-p (expt (%rfo-big) 1/3))
        (%rfo-overflow-p (expt (%rfo-big) 0.5))
        (%rfo-overflow-p (+ (%rfo-big) 1.5))
        (%rfo-overflow-p (* (%rfo-big) 1d0))
        (%rfo-overflow-p (/ (%rfo-big-ratio) 2.0))
        (%rfo-overflow-p (sqrt (- (%rfo-big)))))
  (t t t t t t t t t t t t t t t t t t t t))

(deftest rational-to-float-overflow.values-that-fit
  (list (sqrt (expt 10 40))
        (expt (expt 10 40) 1/3)
        (expt (expt 10 38) 1/3)
        (sin 1/3)
        (sin (expt 10 38))
        (cos (expt 10 38))
        (sqrt (/ 1 (expt 10 50)))
        (sqrt (- (%rfo-tiny-ratio)))
        (float (expt 10 38))
        (+ (expt 10 40) 1.5d0)
        (atan 1 (expt 10 38)))
  (1.0e20 2.1544347e13 4.641589e12 0.32719472 0.9891645 0.14681168 1.0e-25 #C(0.0 0.0) 1.0e38 1.0d40
   1.0000001e-38))

(deftest rational-to-float-overflow.log-of-a-huge-integer-is-fine
  (list (< 921.0 (log (%rfo-big)) 921.1) (< (log (%rfo-tiny-ratio)) -921.0) (< 399.9 (log (%rfo-big) 10) 400.1))
  (t t t))
