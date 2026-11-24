;;; ~E and ~G: the zero before the decimal point is left out when it makes the text wider than the
;;; field (with no overflow character, or with a negative k), and a negative k that is not greater
;;; than -d takes a larger d. SBCL and CLISP print the same strings.

(defparameter *fxe-cases*
  '(("~5,1,3,0,,'_@E" 0.0018d0) ("~5,1,3,0,,'_E" 0.0018d0) ("~5,1,,0E" 0.5d0) ("~0,1,1,-1E" -0.5d0)
    ("~5,2,1,-1E" 0.5d0) ("~0,2,1,-1E" 0.5d0) ("~0,3,2,-2E" 0.5d0) ("~7,2,2,-1E" 0.5d0) ("~0,2,1,-2E" 0.5d0)
    ("~5,1,3,0,,'_G" 0.0018d0) ("~8,2,,-2,'*@E" 0.5d0) ("~9,2,,-2,'*E" 0.5d0)
    ;; neighbours that do not change: the zero is printed when the width allows it
    ("~20,2,,-1E" 0.5d0) ("~,2,,-1E" 0.5d0) ("~12,2,,0E" 0.5d0) ("~,2,,0E" 0.5d0) ("~12,2,,-1E" 0.5d0)
    ("~4,1E" 0.5d0) ("~E" 0.5d0) ("~,3E" 1234.5d0) ("~10,2E" 0.05d0) ("~F" 0.5d0) ("~3,1F" 0.5d0) ("~5,1,,,'*E" 9.99d0)
    ("~5,1,,0,'*E" 9.99d0) ("~12,2,,0,'*E" 0.5d0) ("~G" 0.0018d0)))

(deftest format-exponent-zero-and-negative-k.cases
  (mapcar (lambda (c) (format nil (first c) (second c))) *fxe-cases*)
  ("+.2d-002" ".2d-002" ".5d+0" "-.05d+1" ".05d+1" ".05d+1" ".005d+02" ".05d+01" ".005d+2" ".2d-002" "+.005d+2" " 0.005d+2" "             0.05d+1" "0.05d+1" "     0.50d+0" "0.50d+0" "     0.05d+1" "5.0d-1" "5.0d-1" "1.235d+3" "   5.00d-2" "0.5" "0.5" "*****" "*****" "     0.50d+0" "1.8000d-3"))
