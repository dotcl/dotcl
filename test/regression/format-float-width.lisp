;;; ~F with a width must keep the fraction, and ~G's fixed branch pads with
;;; spaces rather than its ~F padchar.

(deftest format-f-width.omitted-d-keeps-fraction
  (list (format nil "~0F" 1.5)
        (format nil "~0F" 3.14)
        (format nil "~0F" 1/2)
        (format nil "~1F" 1.5)
        (format nil "~0F" 1))
  ("1.5" "3.14" ".5" "1.5" "1."))

(deftest format-f-width.no-zero-fraction-when-width-forbids
  (list (format nil "~10F" 1.0d20)
        (format nil "~10,,2F" 1.0d20)
        (format nil "~G" 1.0e38))
  ("100000000000000000000."
   "10000000000000000000000."
   "100000000000000000000000000000000000000.    "))

(deftest format-g-width.fixed-branch-pads-with-spaces
  (list (format nil "~10,3,2,1,'*,'?G" 1)
        (format nil "~10,3,2,1,'*,'?G" 1/2))
  ("??1.00    " "?0.500    "))

(deftest format-g-width.zero-integer-separator
  (list (format nil "~10,3G" 0)
        (format nil "~10,3G" 0.0))
  ("  0.00    " "  0.00    "))
