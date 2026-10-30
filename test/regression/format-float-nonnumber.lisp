;;; ~F, ~E, ~G and ~$ print a non-number like ~wD instead of zero.
;;; Expected values are SBCL 2.6.6's.

(deftest format-float-nonnumber.fixed
  (list (format nil "~vf" 5 :total)
        (format nil "~8f" :total)
        (format nil "~8,2f" :total)
        (format nil "~8@f" :total)
        (format nil "~8,2,1f" :total)
        (format nil "~8f" "hi")
        (format nil "~8f" nil)
        (format nil "~8,,,,'*F" :total)
        (format nil "~8f" #C(1 2))
        (format nil "~8f" 3.5))
  ("TOTAL" "TOTAL   " "TOTAL   " "TOTAL   " "TOTAL   "
   "hi      " "NIL     " "TOTAL***" " #C(1 2)" "     3.5"))

(deftest format-float-nonnumber.exponential
  (list (format nil "~8e" :total)
        (format nil "~8,2e" :total)
        (format nil "~8@e" :total)
        (format nil "~8,,,,,'*E" :total)
        (format nil "~8,,,,,'*E" #C(1 2)))
  ("TOTAL   " "TOTAL   " "TOTAL   " "TOTAL***" "#C(1 2)*"))

(deftest format-float-nonnumber.general
  (list (format nil "~8g" :total)
        (format nil "~8,2g" :total)
        (format nil "~8@g" :total)
        (format nil "~8,,,,,'*G" :total)
        (format nil "~8,,,,,'*G" #C(1 2))
        (format nil "~8g" 42))
  ("TOTAL   " "TOTAL   " "TOTAL   " "TOTAL***" " #C(1 2)" " 42.    "))

(deftest format-float-nonnumber.dollar
  (list (format nil "~$" :total)
        (format nil "~8$" :total)
        (format nil "~2,3,8$" :total)
        (format nil "~2,3,8@$" :total)
        (format nil "~2,3,8:$" :total)
        (format nil "~2,3,8,'*$" :total)
        (format nil "~2,3,8,'*$" #C(1 2))
        (format nil "~8$" 42))
  ("TOTAL" "TOTAL" "   TOTAL" "   TOTAL" "   TOTAL" "   TOTAL"
   " #C(1 2)" "42.00000000"))

;;; The same directives with a control string that is not a literal at the
;;; call site go through the run-time FORMAT path.
(defun %format-nonnumber-with (control arg)
  (format nil control arg))

(deftest format-float-nonnumber.runtime-control-string
  (list (%format-nonnumber-with "~8f" :total)
        (%format-nonnumber-with "~8e" :total)
        (%format-nonnumber-with "~8g" :total)
        (%format-nonnumber-with "~2,3,8$" :total))
  ("TOTAL   " "TOTAL   " "TOTAL   " "   TOTAL"))
