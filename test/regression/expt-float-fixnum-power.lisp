;;; EXPT of a float to a fixnum power takes a short path ahead of the general
;;; one; the results, the contagion and the overflow errors are the general
;;; path's.

(defun expt-ffp (b e)
  (handler-case (expt b e)
    (floating-point-overflow () :overflow)))

(deftest expt-float-fixnum-power-values
  (mapcar (lambda (args) (apply #'expt-ffp args))
          '((1.5d0 2) (-2.0d0 3) (2.0d0 -2) (1.5f0 3) (-2.0f0 2) (0.5f0 -1)
            (10.0f0 39) (1.0f30 2) (10.0d0 309) (0.5d0 2000) (0.5f0 200)
            (2.0d0 0) (2.0f0 0) (0.0d0 2) (0.0f0 3)))
  (2.25d0 -8.0d0 0.25d0 3.375f0 4.0f0 2.0f0
   :overflow :overflow :overflow 0.0d0 0.0f0
   1.0d0 1.0f0 0.0d0 0.0f0))
