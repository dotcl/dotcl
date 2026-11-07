;;; ~E without a digit count prints a single-float with the digits PRIN1 would print
;;; (3.1415927e+0, not 3.141593e+0).

(deftest format-e-shortest-single.e-single-float
  (list (format nil "~e" 3.14159265358979)
        (format nil "~e" 1234567.89)
        (format nil "~e" 123456789.0)
        (format nil "~e" 1.5) (format nil "~e" 100.0) (format nil "~e" 0.001)
        (format nil "~e" 1.0e10) (format nil "~e" -2.5)
        (format nil "~e" 3.14159265358979d0)
        (format nil "~,3e" 3.14159265358979)
        (format nil "~10e" 3.14159265358979))
  ("3.1415927e+0" "1.2345679e+6" "1.2345679e+8" "1.5e+0" "1.0e+2" "1.0e-3" "1.0e+10" "-2.5e+0"
   "3.14159265358979d+0" "3.142e+0" "3.14159e+0"))
