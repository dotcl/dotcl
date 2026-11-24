;;; A single-float literal was read through a double, so a decimal a hair above
;;; the halfway point between two floats was rounded to the tie first and then
;;; to even, which can be the wrong neighbour.

(deftest reader-single-float-rounding.above-tie
  (list (= (read-from-string "1.00000005960464478e0") (+ 1.0f0 (scale-float 1.0f0 -23)))
        (= (read-from-string "1.0000000596046448e0") (+ 1.0f0 (scale-float 1.0f0 -23))))
  (t t))

(deftest reader-single-float-rounding.exact-tie-and-plain
  (list (= (read-from-string "1.000000059604644775390625e0") 1.0f0)
        (= (read-from-string "16777217.0") 16777216.0f0)
        (= (read-from-string "0.1e0") (float 1/10 1.0f0))
        (typep (read-from-string "1.5") 'single-float))
  (t t t t))

(deftest reader-single-float-rounding.overflow-still-error
  (handler-case (progn (read-from-string "3.5e38") :no-error)
    (reader-error () :reader-error))
  :reader-error)
