;;; A logical block that starts after a relative tab (~n@T) keeps its later
;;; lines under the column it started at, including a block with a per-line
;;; prefix of its own inside it.

(deftest pprint-block-after-tab.mandatory-lines
  (format nil "~@<| ~@;Err ~@:_See:~@:_~2@T~<~{~a~^~@:_~}~:>~:>" (list '(1 2 3)))
  "| Err
| See:
|   1
|   2
|   3")

(deftest pprint-block-after-tab.no-prefix
  (format nil "~@<Err~@:_~2@T~<~{~a~^~@:_~}~:>~:>" (list '(1 2 3)))
  "Err
  1
  2
  3")

(deftest pprint-block-after-tab.own-prefix
  (format nil "~@<Error~@:_~2@T~@<Foo.~@:_ ~@<> ~@;a~@:_b~@:>~:>~:>")
  "Error
  Foo.
   > a
   > b")

(deftest pprint-block-after-tab.next-line-after-block
  (format nil "~@<| ~@;Err~@:_~2@T~a~@:_x~:>" 1)
  "| Err
|   1
| x")
