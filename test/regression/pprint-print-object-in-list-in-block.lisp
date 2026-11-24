;;; A list holding an object whose PRINT-OBJECT ends a line, princed inside a logical block after a
;;; fill newline: the fill newline breaks (the section after it holds the forced newline), and the
;;; list's lines follow the block's indentation.

(defstruct plb-pair a b)

(defmethod print-object ((n plb-pair) s)
  (pprint-logical-block (s nil :prefix "<" :suffix ">")
    (princ (plb-pair-a n) s)
    (pprint-newline :mandatory s)
    (princ (plb-pair-b n) s)))

(defun %plb (margin)
  (let ((*print-pretty* t) (*print-right-margin* margin))
    (with-output-to-string (o)
      (pprint-logical-block (o nil :prefix "[" :suffix "]")
        (princ "head " o)
        (pprint-newline :fill o)
        (princ (list 'zz (make-plb-pair :a "a" :b "b") 2) o)))))

(deftest pprint-print-object-in-list-in-block.fill-breaks
  (list (%plb 75) (%plb 20))
  ("[head
 (ZZ
  <a
   b>
  2)]"
   "[head
 (ZZ
  <a
   b>
  2)]"))

(deftest pprint-print-object-in-list-in-block.after-text
  (let ((*print-pretty* t) (*print-right-margin* 75))
    (with-output-to-string (o)
      (write-string "xxxx" o)
      (prin1 (list 1 (make-plb-pair :a "a" :b "b") (list 2 (make-plb-pair :a "c" :b "d"))) o)))
  "xxxx(1
     <a
      b>
     (2
      <c
       d>))")
