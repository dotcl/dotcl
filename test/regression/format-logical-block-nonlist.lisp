;;; ~<...~:> on an argument that is not a list prints it as WRITE does,
;;; not as PRINC: a string keeps its quotes, a character its #\ prefix.

(deftest format-logical-block.nonlist-string
  (format nil "~<~a~:>" "abc")
  "\"abc\"")

(deftest format-logical-block.nonlist-character
  (format nil "~<~a~:>" #\a)
  "#\\a")

(deftest format-logical-block.nonlist-pathname
  (format nil "~<~a~:>" #p"foo")
  "#P\"foo\"")

(deftest format-logical-block.nonlist-respects-print-escape
  (let ((*print-escape* nil))
    (format nil "~<~a~:>" "abc"))
  "abc")

(deftest format-logical-block.list-body-unchanged
  (format nil "~<~a ~a~:>" '("x" "y"))
  "x y")
