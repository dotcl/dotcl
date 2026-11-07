;;; ~R without a radix prints English (or Roman) numerals and ignores the other
;;; parameters, as in SBCL and ECL: `~,,' ,3R` has a number where a comma character
;;; would go, and that is no error when the radix is not there to use it.

(deftest format-radix-omitted.english-with-other-parameters
  (list (format nil "~,,' ,3R" 12345)
        (format nil "~,,'x,3:R" 12345)
        (format nil "~,,,3R" 12))
  ("twelve thousand three hundred forty-five" "twelve thousand three hundred forty-fifth" "twelve"))

(deftest format-radix-omitted.radix-given-still-checked
  (handler-case (format nil "~10,,' ,3R" 12345)
    (type-error () :type-error))
  :type-error)

(deftest format-radix-omitted.radix-given-works
  (list (format nil "~16R" 255) (format nil "~2,8,'0R" 5) (format nil "~10,,,,3:R" 1234567))
  ("FF" "00000101" "1,234,567"))
