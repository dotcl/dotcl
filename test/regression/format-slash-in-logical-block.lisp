;;; A function called by ~/.../ inside a FORMAT logical block prints into that
;;; block's XP buffer. Expected values are SBCL 2.6.8's.

(defun %fslb-fill (stream object &optional colon at &rest parameters)
  (declare (ignore colon at parameters))
  (pprint-fill stream object))

(defun %fslb-linear (stream object &optional colon at &rest parameters)
  (declare (ignore colon at parameters))
  (pprint-linear stream object))

(deftest format-slash-in-logical-block.fill
  (let ((*print-right-margin* 8) (*print-pretty* t) (*print-miser-width* nil))
    (format nil "~@<prefix ~/%fslb-fill/~:>"
            '("string one" "string two" "string three")))
  #.(format nil "prefix (\"string one\"~%        \"string two\"~%        \"string three\")"))

(deftest format-slash-in-logical-block.linear
  (let ((*print-right-margin* 12) (*print-pretty* t) (*print-miser-width* nil))
    (format nil "~@<prefix ~/%fslb-linear/~:>"
            '("string one" "string two" "string three")))
  #.(format nil "prefix (\"string one\"~%        \"string two\"~%        \"string three\")"))
