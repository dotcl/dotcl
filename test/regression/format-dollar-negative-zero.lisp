;;; ~$ keeps the sign of a negative zero, as ~F and ~E do (SBCL writes -0.00).

(deftest format-dollar-negative-zero.plain
  (list (format nil "~$" -0.0)
        (format nil "~$" 0.0)
        (format nil "~$" -0.001)
        (format nil "~$" -0.0d0))
  ("-0.00" "0.00" "-0.00" "-0.00"))

(deftest format-dollar-negative-zero.modifiers
  (list (format nil "~@$" -0.0)
        (format nil "~@$" 0.0)
        (format nil "~8,3$" -0.0))
  ("-0.00" "+0.00" "-000.00000000"))
