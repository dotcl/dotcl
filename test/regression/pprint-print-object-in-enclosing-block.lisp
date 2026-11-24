;;; An object printed with PRINC / PRIN1 inside a logical block, whose PRINT-OBJECT method opens a
;;; block and ends a line itself, is part of that block: the fill, linear and miser newlines before
;;; it see its forced newline (the section after them does not fit on one line), and its own block
;;; lines up where it starts.

(defstruct pob-pair a b)

(defmethod print-object ((n pob-pair) s)
  (pprint-logical-block (s nil :prefix "<" :suffix ">")
    (princ (pob-pair-a n) s)
    (pprint-newline :mandatory s)
    (princ (pob-pair-b n) s)))

(defun %pob (kind)
  (let ((*print-pretty* t) (*print-right-margin* 75))
    (with-output-to-string (o)
      (pprint-logical-block (o nil :prefix "[" :suffix "]")
        (princ "head " o)
        (pprint-newline kind o)
        (princ (make-pob-pair :a "a" :b "b") o)))))

(deftest pprint-print-object-in-enclosing-block.newline-before
  (list (%pob :fill) (%pob :linear) (%pob :miser))
  ("[head
 <a
  b>]"
   "[head
 <a
  b>]"
   "[head <a
       b>]"))

(deftest pprint-print-object-in-enclosing-block.nested-objects
  (let ((*print-pretty* t) (*print-right-margin* 75))
    (with-output-to-string (o)
      (pprint-logical-block (o nil :prefix "[" :suffix "]")
        (princ "head " o)
        (pprint-newline :fill o)
        (princ (make-pob-pair :a "a" :b (make-pob-pair :a 1 :b 2)) o))))
  "[head
 <a
  <1
   2>>]")
