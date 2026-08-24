;;; ~G picks its digit count from the value, not from the float type.
;;;
;;; CLHS 22.3.3.3: with d omitted, "let q be the number of digits needed to print
;;; arg with no loss of information", then d = max(q, min(n, 7)). The digit count
;;; came from the type instead (7 for single, 15 for double), so every value was
;;; padded out with zeros it never had -- (format nil "~g" 1234.5) printed
;;; "1234.500", (format nil "~g" 1.5d0) printed "1.50000000000000" -- and large
;;; round numbers went to ~E where fixed notation was called for.
;;;
;;; q counts the digits of the FIXED-POINT text: 1.0d10 prints as "10000000000."
;;; and so needs 11, which is what keeps it in ~F. Every expectation here was
;;; checked against SBCL 2.6.6.

(deftest format-g-digits.plain-values
  (list (format nil "~g" 1234.5)
        (format nil "~g" 1.0)
        (format nil "~g" 0.5)
        (format nil "~g" -42.25)
        (format nil "~g" 1.5d0)
        (format nil "~g" 3.14159265358979d0))
  ("1234.5    " "1.    " "0.5    " "-42.25    " "1.5    " "3.14159265358979    "))

(deftest format-g-digits.zero
  (format nil "~g" 0.0)
  "0.    ")

;;; A round number keeps fixed notation: q counts its trailing zeros because the
;;; fixed text has them.
(deftest format-g-digits.large-round-numbers-stay-fixed
  (list (format nil "~g" 1.0d10)
        (format nil "~g" 10000000.0)
        (format nil "~g" 9999999.0))
  ("10000000000.    " "10000000.    " "9999999.    "))

;;; Below 10^-3 the exponential branch takes over, and it prints the digit count
;;; ~G computed -- not the "d omitted" default it would pick on its own.
(deftest format-g-digits.small-values-use-computed-d
  (list (format nil "~g" 0.0001)
        (format nil "~g" 0.001)
        (format nil "~g" 0.0009))
  ("1.0000e-4" "1.000e-3" "9.0000e-4"))

;;; Explicit parameters still drive the layout.
(deftest format-g-digits.explicit-parameters
  (list (format nil "~10g" 1234.5)
        (format nil "~,,2g" 1234.5)
        (format nil "~,,,3g" 1234.5))
  ("1234.5    " "1234.5    " "1234.5    "))
