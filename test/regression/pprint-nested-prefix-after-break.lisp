;;; A nested block with a per-line prefix, after a fill newline broke the line before it: its
;;; later lines (a mandatory newline, a fill newline) are under the column the block starts at,
;;; not the column it had before the break. Expected strings are SBCL's.

(defun %g6b (margin plp-outer plp-inner n)
  (with-output-to-string (s)
    (let ((*print-pretty* t) (*print-right-margin* margin) (*print-miser-width* nil))
      (pprint-logical-block (s nil :per-line-prefix plp-outer)
        (pprint-indent :block n s)
        (write-string "dddd" s)
        (pprint-newline :fill s)
        (pprint-logical-block (s '(2 1 "ccc") :per-line-prefix plp-inner)
          (write (pprint-pop) :stream s) (write-char #\Space s)
          (pprint-newline :mandatory s)
          (write (pprint-pop) :stream s) (write-char #\Space s)
          (pprint-newline :fill s)
          (write (pprint-pop) :stream s))))))

(deftest pprint-nested-prefix-after-break.lines
  (mapcar (lambda (c) (apply #'%g6b c))
          '((4 "    " "  " 1) (4 "    " "> " 1) (4 "> " "  " 2) (4 "" "  " 0)
            (40 "    " "  " 1) (6 "  " "  " 3) (4 "    " "" 1)))
  ("    dddd
       2
       1
       \"ccc\"" "    dddd
     > 2
     > 1
     > \"ccc\"" "> dddd
>     2
>     1
>     \"ccc\"" "dddd
  2
  1
  \"ccc\"" "    dddd
       2
       1 \"ccc\"" "  dddd
       2
       1
       \"ccc\"" "    dddd
     2
     1
     \"ccc\""))
