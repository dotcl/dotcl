;;; ~<...~:> honours *PRINT-LENGTH* 0: nothing of the list is printed, only "..."
;;; between the prefix and suffix.

(deftest format-logical-block.print-length-0
  (let ((*print-length* 0))
    (format nil "~<~a~:>" '(a b)))
  "...")

(deftest format-logical-block.print-length-0-with-parens
  (let ((*print-length* 0))
    (format nil "~:<~a~:>" '(a b)))
  "(...)")

(deftest format-logical-block.print-length-0-empty-list-keeps-body
  (let ((*print-length* 0))
    (format nil "~:<x~:>" '()))
  "(x)")

(deftest format-logical-block.print-length-nil
  (let ((*print-length* nil))
    (format nil "~:<~a ~a~:>" '(a b)))
  "(A B)")

(deftest format-logical-block.print-length-3-short-list
  (let ((*print-length* 3))
    (format nil "~:<~a ~a~:>" '(a b)))
  "(A B)")
