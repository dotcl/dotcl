;;; PPRINT-INDENT :CURRENT is relative to where the text is once the line breaks are chosen, not
;;; to where it was when it was written. Expected strings are SBCL's.

(defun %g6 (margin kind n &key (miser nil) (pre "{"))
  (with-output-to-string (s)
    (let ((*print-pretty* t) (*print-right-margin* margin) (*print-miser-width* miser))
      (pprint-logical-block (s '("2" "ccc" "z") :prefix pre :suffix "")
        (write (pprint-pop) :stream s)
        (pprint-newline :linear s)
        (pprint-indent kind n s)
        (write (pprint-pop) :stream s)
        (pprint-newline :mandatory s)
        (write (pprint-pop) :stream s)))))
(defun %g6-nested ()
  (with-output-to-string (s)
    (let ((*print-pretty* t) (*print-right-margin* 8) (*print-miser-width* nil))
      (pprint-logical-block (s nil :prefix "(" :suffix ")")
        (write-string "aaa" s) (pprint-newline :fill s)
        (pprint-logical-block (s nil :prefix "[" :suffix "]")
          (write-string "bb" s) (pprint-newline :linear s)
          (pprint-indent :current 2 s)
          (write-string "cc" s) (pprint-newline :mandatory s) (write-string "dd" s))))))

(deftest pprint-indent-current.after-a-break
  (list (%g6 4 :current 3) (%g6 4 :current 0) (%g6 4 :block 3) (%g6 4 :block 0)
        (%g6 40 :current 3) (%g6 40 :block 3) (%g6 4 :current -1)
        (%g6 4 :current 3 :miser 40) (%g6-nested))
  ("{\"2\"
 \"ccc\"
    \"z\"" "{\"2\"
 \"ccc\"
 \"z\"" "{\"2\"
 \"ccc\"
    \"z\"" "{\"2\"
 \"ccc\"
 \"z\"" "{\"2\"
 \"ccc\"
    \"z\"" "{\"2\"
 \"ccc\"
    \"z\"" "{\"2\"
 \"ccc\"
\"z\"" "{\"2\"
 \"ccc\"
 \"z\"" "(aaa
 [bb
  cc
    dd])"))
