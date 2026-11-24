;;; ~F, ~E and ~G with a width that is one column short of the optional zeros.
;;; Expected strings are what SBCL, CLISP and ABCL all print for these forms
;;; (a tight field drops the optional zero rather than overflowing), except the
;;; two marked "suite": those keep the ansi-test expectation.

(deftest format-float-width.f-drops-trailing-zero
  (list (format nil "~3,,,F" 12.5d0)
        (format nil "~3,,,'*F" 12.5d0)
        (format nil "~4F" 123.5d0)
        (format nil "~3F" 12.4d0))
  ("13." "13." "124." "12."))

(deftest format-float-width.f-suite-zero-kept
  (list (format nil "~2f" 1.0d0)
        (format nil "~0,0f" 0.01d0))
  ("1.0" "0."))

(deftest format-float-width.e-overflowchar-drops-leading-zero
  (list (format nil "~5,1,,0,'*E" 0.5d0)
        (format nil "~5,1,,0,'*E" 1.0d0)
        (format nil "~5,1,,0,'*E" 12.5d0)
        (format nil "~8,3,,0,'*E" -0.25d0)
        (format nil "~5,1,,0,'*G" 12.5d0)
        (format nil "~5,1,,0,'*E" 1.0d-7)
        ;; a rounding carry keeps the overflow, as in SBCL and CLISP
        (format nil "~5,1,,0,'*E" 9.99d0)
        (format nil "~5,1,,0,'*E" 99.95d0))
  (".5d+0" ".1d+1" ".1d+2" "-.250d+0" ".1d+2" ".1d-6" "*****" "*****"))

(deftest format-float-width.g-drops-leading-zero-of-zero-digit-fraction
  (list (format nil "~3,0,,,G" -0.25d0)
        (format nil "~5,0,,0,G" -0.25d0))
  ("-.    " "-.    "))
