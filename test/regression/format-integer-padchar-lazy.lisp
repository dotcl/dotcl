;;; The pad character of ~D ~B ~O ~X (and ~nR) is only checked when the field is padded:
;;; (format nil "~,8B" 5) puts 8 in the padchar slot with no mincol and prints "101", as SBCL
;;; does. A bad padchar with a mincol that pads is still an error.

(defmacro %fipl-error-p (form) `(handler-case (progn ,form nil) (error () t)))

(deftest format-integer-padchar-lazy.unused-padchar
  (list (format nil "~,8B" 5) (format nil "~,8,'0B" 5) (format nil "~,8D" 5)
        (format nil "~,8O" 8) (format nil "~,8X" 255) (format nil "~2,,8R" 5))
  ("101" "101" "5" "10" "FF" "101"))

(deftest format-integer-padchar-lazy.used-padchar-still-checked
  (list (%fipl-error-p (format nil "~5,8B" 5))
        (%fipl-error-p (format nil "~5,8D" 5))
        (%fipl-error-p (format nil "~2,5,8R" 5)))
  (t t t))

(deftest format-integer-padchar-lazy.valid-forms-unchanged
  (list (format nil "~8,'0B" 5) (format nil "~5,'*D" 42) (format nil "~,,'-:D" 5)
        (format nil "~3,'0X" 10) (format nil "~2,8,'*R" 5))
  ("00000101" "***42" "5" "00A" "*****101"))
