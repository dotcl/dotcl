;;; A range of REALs is not within RATIO or a rational type (it holds floats), and a FLOAT type is not
;;; within a range of rationals: SUBTYPEP compared these by their names only and answered T.

(defun %rr (a b) (multiple-value-list (subtypep a b)))

(deftest subtypep-real-range.not-ratio
  (list (%rr '(real (0) (1)) 'ratio) (%rr '(real (0) (1)) '(or integer ratio)) (%rr '(rational (0) (1)) 'ratio))
  ((nil t) (nil t) (t t)))

(deftest subtypep-real-range.float-types-not-in-rational-range
  (list (%rr 'double-float '(real 1/2 3/2)) (%rr 'single-float '(real 1/2 3/2)) (%rr 'float '(real 1/2 3/2))
        (%rr '(double-float 0d0 1d0) '(real 0 2)) (%rr 'double-float '(real * *)) (%rr '(integer 1 1) '(real 1/2 3/2)))
  ((nil t) (nil t) (nil t) (t t) (t t) (t t)))
