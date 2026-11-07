;;; ~G with a width narrower than its exponent field pads or overflows as ~F / ~E do;
;;; it does not leak a .NET exception for a negative pad count.

(deftest format-g-narrow-width.pad
  (format nil "~1,,20,20,'0,' ,'d@G" 1.5)
  "0000000000000000000000")

(deftest format-g-narrow-width.spaces
  (format nil "~0,,3,2,' ,' G" 1.5)
  "     ")

(deftest format-g-narrow-width.overflow-char
  (format nil "~3,,2,,'*G" 1.5)
  "****")

(deftest format-g-narrow-width.ordinary
  (list (format nil "~10,,2G" 1.5) (format nil "~G" 1.5))
  ("   1.5    " "1.5    "))
