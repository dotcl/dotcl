;;; RATIONALIZE of a subnormal float gives the simplest rational in the rounding interval, and
;;; converting a tiny ratio to a float rounds once (it used to round twice and could land on
;;; the neighbour of a value just below a tie).

(defun %rs-simplest-p (x)
  "True when RATIONALIZE of X converts back to X and has a smaller denominator than the exact fraction."
  (let ((r (rationalize x)))
    (and (= (float r x) x)
         (< (denominator r) (denominator (rational x))))))

(deftest rationalize-subnormal.simplest
  (list (%rs-simplest-p least-positive-double-float)
        (%rs-simplest-p (* 3 least-positive-double-float))
        (%rs-simplest-p 1d-310)
        (%rs-simplest-p least-positive-single-float)
        (%rs-simplest-p 1f-40)
        (integer-length (denominator (rationalize least-positive-double-float))))
  (t t t t t 1074))

(deftest rationalize-subnormal.ordinary-values-unchanged
  (list (rationalize 3.2d0) (rationalize 0.1d0) (rationalize 1d-300) (rationalize 3.2f0)
        (rationalize -0.1d0))
  (16/5 1/10 #.(rationalize 1d-300) 16/5 -1/10))

(deftest rationalize-subnormal.tiny-ratio-to-float
  (list (float (/ 1 (expt 2 1074)) 1d0)
        (float (/ 3 (expt 2 1075)) 1d0)
        (float (/ 1 (+ 1 (ceiling (* 2/3 (expt 2 1074))))) 1d0)
        (float (/ 1 (expt 2 1076)) 1d0)
        (float (/ 3 (expt 2 1076)) 1d0)
        (float (/ 1 (expt 2 149)) 1f0)
        (= (float (/ 3 (expt 2 150)) 1f0) (* 2 least-positive-single-float))
        (float (/ 1 (+ 1 (ceiling (* 2/3 (expt 2 149))))) 1f0)
        (float (/ -1 (expt 2 1074)) 1d0))
  (5.0d-324 1.0d-323 5.0d-324 0.0d0 5.0d-324 1.0e-45 t 1.0e-45 -5.0d-324))
