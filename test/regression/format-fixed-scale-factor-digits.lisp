;;; ~F with a scale factor and no width or digit count moves the decimal point in
;;; the shortest digits instead of multiplying the float.

(deftest format-fixed-scale-factor-digits.positive
  (list (format nil "~,,2F" 123.456)
        (format nil "~,,2F" 0.32919255)
        (format nil "~,,2F" 3.3524e9)
        (format nil "~,,1F" 1.3969908073771278d0)
        (format nil "~,,3F" 0.1))
  ("12345.6" "32.919255" "335240000000.0" "13.969908073771278" "100.0"))

(deftest format-fixed-scale-factor-digits.negative
  (list (format nil "~,,-2F" 3.3524e9)
        (format nil "~,,-2F" 0.32919255)
        (format nil "~,,-2F" 1.3969908073771278d0))
  ("33524000.0" "0.0032919255" "0.013969908073771278"))
