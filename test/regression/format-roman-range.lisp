;;; ~@R prints 1 to 3999 and ~:@R (old Roman numerals) 1 to 4999; a number outside
;;; that range is an error, as in SBCL and ECL, not the number in decimal.

(defun %frr (control n)
  (handler-case (format nil control n) (error () :error)))

(deftest format-roman-range.out-of-range
  (list (%frr "~@R" 4000) (%frr "~@R" 0) (%frr "~@R" -1) (%frr "~@R" (expt 10 30))
        (%frr "~:@R" 5000) (%frr "~:@R" 0))
  (:error :error :error :error :error :error))

(deftest format-roman-range.in-range
  (list (%frr "~@R" 1) (%frr "~@R" 3999) (%frr "~:@R" 4999) (%frr "~:@R" 4))
  ("I" "MMMCMXCIX" "MMMMDCCCCLXXXXVIIII" "IIII"))

(deftest format-roman-range.other-radix-unaffected
  (list (format nil "~R" 0) (format nil "~@R" 4) (format nil "~:R" 4))
  ("zero" "IV" "fourth"))
