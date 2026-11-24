;;; ~E with no digit count and a scale factor of 0 or less prints 0.0..d1d2... with the exponent raised to match.

(deftest format-exponent-nonpositive-scale.free-format
  (list (format nil "~,,,-1E" 0.1)
        (format nil "~,,,0E" 0.1)
        (format nil "~,,,-1E" 3.3524e9)
        (format nil "~,,,-2E" 123.456)
        (format nil "~,,,-1E" 1d-5)
        (format nil "~,,,0E" 1.0))
  ("0.01e+1" "0.1e+0" "0.033524e+11" "0.00123456e+5" "0.01d-3" "0.1e+1"))
