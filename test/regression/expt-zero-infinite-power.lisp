;;; (EXPT 0.0 +infinity) is 0.0, not NaN.

(deftest expt-zero-infinite-power.double
  (list (expt 0d0 dotcl:double-float-positive-infinity)
        (expt -0d0 dotcl:double-float-positive-infinity)
        (expt 0.0 dotcl:single-float-positive-infinity)
        (expt 0d0 2d0)
        (expt -0d0 3d0)
        (expt 0d0 0.5d0))
  (0d0 0d0 0.0 0d0 -0d0 0d0))

(deftest expt-zero-infinite-power.negative-infinity-power
  (let ((r (expt 0d0 dotcl:double-float-negative-infinity)))
    (and (floatp r) (> r most-positive-double-float)))
  t)
