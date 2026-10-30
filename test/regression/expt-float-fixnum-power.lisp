;;; EXPT of a float to a fixnum power takes a short path ahead of the general
;;; one; the results, the contagion and the overflow are the general path's:
;;; the IEEE infinity below (safety 3), FLOATING-POINT-OVERFLOW at it.

(defun expt-ffp (b e)
  (handler-case (expt b e)
    (floating-point-overflow () :overflow)))

(defun expt-ffp-3 (b e)
  (declare (optimize (safety 3)))
  (handler-case (expt b e)
    (floating-point-overflow () :overflow)))

(deftest expt-float-fixnum-power-values
  (mapcar (lambda (args) (apply #'expt-ffp args))
          '((1.5d0 2) (-2.0d0 3) (2.0d0 -2) (1.5f0 3) (-2.0f0 2) (0.5f0 -1)
            (10.0f0 39) (1.0f30 2) (10.0d0 309) (0.5d0 2000) (0.5f0 200)
            (2.0d0 0) (2.0f0 0) (0.0d0 2) (0.0f0 3)))
  (2.25d0 -8.0d0 0.25d0 3.375f0 4.0f0 2.0f0
   #.dotcl:single-float-positive-infinity #.dotcl:single-float-positive-infinity
   #.dotcl:double-float-positive-infinity 0.0d0 0.0f0
   1.0d0 1.0f0 0.0d0 0.0f0))

(deftest-compiled-only expt-float-fixnum-power-overflow-safety-3
  (mapcar (lambda (args) (apply #'expt-ffp-3 args))
          '((10.0f0 39) (1.0f30 2) (10.0d0 309) (1.5d0 2)))
  (:overflow :overflow :overflow 2.25d0))
