;;; ~mincolA / ~mincolS (any parameter given) print the object into a string of their own, which
;;; starts at column 0: a ~& or FRESH-LINE in its PRINT-OBJECT method does not see the text FORMAT
;;; wrote before the field. Without parameters the object is printed in place and ~& does start a
;;; new line. Expected values are SBCL's.

(defstruct fpf-obj)
(defmethod print-object ((o fpf-obj) s) (format s "~&P"))

(deftest format-padded-field-fresh-line.padded
  (list (format nil "x~10a|" (make-fpf-obj))
        (format nil "x~10@a|" (make-fpf-obj))
        (format nil "x~10,,,'.a|" (make-fpf-obj))
        (format nil "x~1a|" (make-fpf-obj))
        (format nil "x~,2a|" (make-fpf-obj))
        (format nil "x~10s|" (make-fpf-obj)))
  ("xP         |" "x         P|" "xP.........|" "xP|" "xP|" "xP         |"))

(deftest format-padded-field-fresh-line.in-place
  (list (format nil "x~a|" (make-fpf-obj))
        (format nil "x~s|" (make-fpf-obj))
        (format nil "~a|" (make-fpf-obj)))
  ("x
P|" "x
P|" "P|"))
