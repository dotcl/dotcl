;;; ~<...~:> honours *PRINT-LEVEL*: a block nested deeper than the level prints
;;; as a single #, as PPRINT-LOGICAL-BLOCK does.

(deftest format-logical-block.print-level-0
  (let ((*print-level* 0))
    (format nil "~<~a~:>" '(a b)))
  "#")

(deftest format-logical-block.print-level-1-top-level-prints
  (let ((*print-level* 1))
    (format nil "~<~a~:>" '(a b)))
  "A")

(deftest format-logical-block.print-level-1-nested-block
  (let ((*print-level* 1))
    (format nil "~<[~@{~<~a~:>~^ ~}]~:>" '((a) (b))))
  "[# #]")

(deftest format-logical-block.print-level-nil
  (let ((*print-level* nil))
    (format nil "~<[~@{~<~a~:>~^ ~}]~:>" '((a) (b))))
  "[A B]")

(deftest format-logical-block.print-level-0-nonlist-argument
  (let ((*print-level* 0))
    (format nil "~<~a~:>" "abc"))
  "\"abc\"")
