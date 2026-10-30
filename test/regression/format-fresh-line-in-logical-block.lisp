;;; ~& inside a FORMAT logical block starts a new line when output that went
;;; straight to the block's stream (a ~/.../ function, a nested block, an
;;; object printed there) left the column past the start of the line.
;;; Expected values are SBCL 2.6.8's.

(defun %ffllb-pp (stream object &optional colon at &rest parameters)
  (declare (ignore object colon at parameters))
  (write-string "  :FOO 1" stream))

(deftest format-fresh-line-in-logical-block.slash
  (let ((*print-pretty* t))
    (format nil "~@<a~/%ffllb-pp/~&b~:>" 1))
  #.(format nil "a  :FOO 1~%b"))

(deftest format-fresh-line-in-logical-block.slash-count
  (let ((*print-pretty* t))
    (format nil "~@<a~/%ffllb-pp/~2&b~:>" 1))
  #.(format nil "a  :FOO 1~%~%b"))

(deftest format-fresh-line-in-logical-block.slash-first
  (let ((*print-pretty* t))
    (format nil "~@<~/%ffllb-pp/~&b~:>" 1))
  #.(format nil "  :FOO 1~%b"))

(deftest format-fresh-line-in-logical-block.after-prefix
  (let ((*print-pretty* t))
    (format nil "~<(~;~&b~;)~:>" nil))
  #.(format nil "(~%b)"))

(deftest format-fresh-line-in-logical-block.nested-block
  (let ((*print-pretty* t))
    (format nil "~@<a~@<x~:>~&b~:>" nil))
  #.(format nil "ax~%b"))

(deftest format-fresh-line-in-logical-block.object
  (let ((*print-pretty* t))
    (format nil "~@<a~S~&b~:>" '(1)))
  #.(format nil "a(1)~%b"))

;;; Already at the start of a line: no newline (CLHS 22.3.1.3). SBCL 2.6.8
;;; outputs one here anyway.
(deftest format-fresh-line-in-logical-block.at-line-start
  (let ((*print-pretty* t))
    (format nil "~@<a~%~&b~:>" nil))
  #.(format nil "a~%b"))
