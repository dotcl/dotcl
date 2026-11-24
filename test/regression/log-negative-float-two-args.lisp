;;; (log x base) with float arguments, one of them negative, returns the complex logarithm quotient
;;; instead of NaN (the float-typed code path).

(defun %lnf-close (z re im)
  (and (complexp z) (< (abs (- (realpart z) re)) 1e-4) (< (abs (- (imagpart z) im)) 1e-4)))

(deftest log-negative-float-two-args.single
  (list (%lnf-close (log -2.5 0.5) -1.3219281 -4.53236)
        (%lnf-close (log -2.5 2.0) 1.3219281 4.53236)
        (%lnf-close (log 2.5 -0.5) -0.0613643 -0.278125)
        (%lnf-close (log -2.5 -0.5) 0.892215 -0.488519)
        (%lnf-close (log -1.0 0.5) 0.0 -4.53236))
  (t t t t t))

(deftest log-negative-float-two-args.double
  (list (%lnf-close (log -2.5d0 0.5d0) -1.3219280948873624d0 -4.532360141827194d0)
        (%lnf-close (log 2.5d0 -0.5d0) -0.0613643d0 -0.278125d0))
  (t t))

(deftest log-negative-float-two-args.unchanged
  (list (< (abs (- (log 8.0 2.0) 3.0)) 1e-5) (< (abs (- (log 2.5 0.5) -1.3219281)) 1e-5)
        (%lnf-close (log -8 2) 3.0 4.53236))
  (t t t))
