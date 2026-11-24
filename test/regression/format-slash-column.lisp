;;; A function called by ~/name/ writes to a stream that begins at the column FORMAT has
;;; reached, so a logical block with a per-line prefix that it opens puts that prefix at
;;; the same column on every line, not only on the first.

(defun fsc-block (s x &rest a)
  (declare (ignore x a))
  (pprint-logical-block (s nil :per-line-prefix "| ")
    (write-string "a" s) (format s "~%") (write-string "b" s)))

(defun fsc-mandatory (s x &rest a)
  (declare (ignore x a))
  (pprint-logical-block (s nil :per-line-prefix "| ")
    (write-string "a" s) (pprint-newline :mandatory s) (write-string "b" s)))

(defun fsc-run (control)
  (let ((*print-pretty* t)) (format nil control 0)))

(deftest format-slash-column.after-tab
  (fsc-run "~<~2@T~/fsc-block/~>")
  "  | a
  | b")

(deftest format-slash-column.after-spaces
  (fsc-run "  ~/fsc-block/")
  "  | a
  | b")

(deftest format-slash-column.after-text
  (fsc-run "xx~/fsc-block/")
  "xx| a
  | b")

(deftest format-slash-column.mandatory-newline
  (fsc-run "~<~2@T~/fsc-mandatory/~>")
  "  | a
  | b")

(deftest format-slash-column.at-line-start
  (fsc-run "~/fsc-block/")
  "| a
| b")

(deftest format-slash-column.second-line-of-the-text
  (fsc-run "line one~%  ~/fsc-block/")
  "line one
  | a
  | b")
