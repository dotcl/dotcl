;;; ~F ~E ~G ~$ coerce a rational argument (integer or ratio) to a single float
;;; (CLHS 22.3.3.1), so the digits are those of the single float.

(deftest format-float-rational-single.ratio
  (values (format nil "~F" 1/3) (format nil "~E" 1/3) (format nil "~G" 1/3)
          (format nil "~$" 1/3) (format nil "~F" -5/9) (format nil "~E" 2/3))
  "0.33333334" "3.3333334e-1" "0.33333334    " "0.33" "-0.5555556" "6.666667e-1")

(deftest format-float-rational-single.integer
  (values (format nil "~F" 16777217) (format nil "~F" 123456789) (format nil "~E" 123456789)
          (format nil "~F" 12345678901234567890) (format nil "~F" 1) (format nil "~,2F" 7))
  "16777216.0" "123456790.0" "1.2345679e+8" "12345679000000000000.0" "1.0" "7.00")

(deftest format-float-rational-single.floats-unchanged
  (values (format nil "~F" 0.5d0) (format nil "~F" (/ 1d0 3)) (format nil "~E" 1.5))
  "0.5" "0.3333333333333333" "1.5e+0")
