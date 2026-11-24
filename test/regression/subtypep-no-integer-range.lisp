;;; A range of rationals with no integer in it ((RATIONAL (0) (1))) shares nothing with the integers,
;;; so it is below (NOT INTEGER).

(defun %s28 (a b) (multiple-value-list (subtypep a b)))

(deftest subtypep-no-integer-range.below-not-integer
  (list (%s28 '(rational (0) (1)) '(not integer)) (%s28 '(rational (0) (1)) '(not (integer 0 10)))
        (%s28 '(rational 1/3 2/3) '(and rational (not integer))) (%s28 '(rational 0 1) '(not integer))
        (%s28 '(rational (1/2) (3/2)) '(not integer)) (%s28 '(rational (0) (1)) '(and real (not integer))))
  ((t t) (t t) (t t) (nil t) (nil t) (t t)))
