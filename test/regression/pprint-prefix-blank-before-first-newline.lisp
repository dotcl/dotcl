;;; A block that begins with a conditional newline ends the line before it, and the line end takes the
;;; blanks off, those of the block's own prefix ("> ", "[ ") too. A block that begins with a newline the
;;; program wrote keeps them. Expected strings are SBCL's.

(defun %pbp-block (pre suf plp body &key (margin 40) outer)
  (with-output-to-string (s)
    (let ((*print-pretty* t) (*print-right-margin* margin) (*print-miser-width* nil))
      (pprint-logical-block (s nil :prefix "{" :suffix "}")
        (when outer (write-string outer s))
        (if plp
            (pprint-logical-block (s nil :per-line-prefix plp :suffix suf) (funcall body s))
            (pprint-logical-block (s nil :prefix pre :suffix suf) (funcall body s)))))))
(defun %pbp-cases ()
  (list
   ;; a block whose first item is a conditional newline: the blanks of its prefix go
   (%pbp-block "" ")" "> " (lambda (s) (pprint-newline :fill s)) :margin 4)
   (%pbp-block "" ")" "  " (lambda (s) (pprint-newline :fill s)) :margin 4)
   (%pbp-block "[ " ")" nil (lambda (s) (pprint-newline :fill s)) :margin 4)
   (%pbp-block "" "" "> " (lambda (s) (pprint-newline :mandatory s)) :margin 14)
   (%pbp-block "[ " "" nil (lambda (s) (pprint-newline :mandatory s) (write-string "z" s)))
   (%pbp-block "(" ")" "> " (lambda (s) (pprint-newline :linear s) (write-string "z" s)) :margin 7)
   (%pbp-block "" "" "> " (lambda (s) (pprint-newline :fill s) (write-string "x" s)) :margin 6 :outer "z")
   ;; unchanged: text first, a newline the program wrote first, no blank in the prefix
   (%pbp-block "" ")" "> " (lambda (s) (write-string "a" s) (pprint-newline :fill s) (write-string "b" s)) :margin 4)
   (%pbp-block "" "" "> " (lambda (s) (terpri s) (write-string "z" s)) :margin 40)
   (%pbp-block "[ " "]" nil (lambda (s) (write-string "z" s)) :margin 40)
   (%pbp-block "[" "]" nil (lambda (s) (pprint-newline :mandatory s) (write-string "z" s)) :margin 40)))

(deftest pprint-prefix-blank-before-first-newline.cases
  (%pbp-cases)
  ("{>
 > )}" "{
   )}" "{[
   )}" "{>
 > }" "{[
   z}" "{> z)}" "{z> x}" "{> a
 > b)}" "{> 
 > z}" "{[ z]}" "{[
  z]}"))
