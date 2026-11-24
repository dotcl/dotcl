;;; FLOOR / TRUNCATE / CEILING / ROUND of a declared double with a divisor are integer
;;; results divided by the divisor, also where the result feeds integer arithmetic. (The
;;; float-rounding lanes handled only the one-argument form; the divisor was dropped.)

(defun %fodd-plus (d) (declare (double-float d)) (+ 1 (floor d 2)))
(defun %fodd-trunc (d) (declare (double-float d)) (+ 1 (truncate d 4)))
(defun %fodd-ceil-round (d) (declare (double-float d)) (+ 1 (ceiling d 2) (round d 2)))
(defun %fodd-list (d) (declare (double-float d)) (list (floor d 2) (+ 1 (floor d 2)) (* 2 (floor d 2)) (+ 0 (floor d 2d0))))
(defun %fodd-nth (n)
  (let ((d (float n 1d0)))
    (declare (type double-float d))
    (+ (the fixnum (+ n 1)) (nth-value 0 (floor (the double-float (* d 3)) 2)))))
(defun %fodd-one-arg (d) (declare (double-float d)) (+ 1 (floor d) (truncate d) (ceiling d) (round d)))
(defun %fodd-single (x) (declare (single-float x)) (+ 1 (floor x 2)))

(deftest floor-of-double-with-divisor.integer-results
  (list (%fodd-plus 15d0) (%fodd-trunc 16d0) (%fodd-ceil-round 15d0) (%fodd-list 15d0) (%fodd-nth 5)
        (%fodd-single 15f0))
  (8 5 17 (7 8 14 7) 13 8))

(deftest floor-of-double-with-divisor.one-argument-unchanged
  (%fodd-one-arg 2.5d0)
  10)
