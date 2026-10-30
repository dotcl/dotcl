;;; ~E with w and no d: print the shortest digits the field allows and pad
;;; with spaces; it used to fill the width with trailing zeros.

(deftest format-e-width.literal-control
  (format nil "~8e" 42)
  "  4.2e+1")

(deftest format-e-width.variable-control
  (let ((control "~8e"))
    (format nil control 42))
  "  4.2e+1")

(deftest format-e-width.explicit-digits-still-pads
  (format nil "~10,2e" 42)
  "   4.20e+1")

(deftest format-e-width.exponent-digits
  (format nil "~8,,2e" 42)
  " 4.2e+01")

(deftest format-e-width.negative
  (format nil "~8e" -42)
  " -4.2e+1")

(deftest format-e-width.small
  (format nil "~8e" 0.0001234)
  "  1.0e-4")

(deftest format-e-width.small-wide
  (format nil "~30e" 1.234e-4)
  "                      1.234e-4")

(deftest format-e-width.large-does-not-fit
  (format nil "~8e" 1.23456789d300)
  "1.23456789d+300")

(deftest format-e-width.does-not-fit
  (format nil "~8e" 1.23456789d20)
  "1.23456789d+20")

(deftest format-e-width.tiny
  (format nil "~8e" 1.23456789d-20)
  "00000000000000000000.d-20")

(deftest format-e-width.tiny-wide
  (format nil "~30e" 1.23456789d-20)
  "                   1.23457d-20")

(deftest format-e-width.narrow-drops-trailing-zero
  (format nil "~5e" 1.0)
  "1.e+0")

(deftest format-e-width.rounding-carry
  (list (format nil "~10e" 9.999999d0)
        (format nil "~8e" 999999d0))
  ("9.999999d+0" "9.99999d+5"))
