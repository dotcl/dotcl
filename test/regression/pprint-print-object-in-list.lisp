;;; An object inside a list, vector or structure printed with the pretty printer, whose PRINT-OBJECT
;;; method opens a block and ends a line itself: the conditional newline before it breaks (the
;;; section after it holds a forced newline) and its later lines are indented to the column it starts
;;; at, as SBCL prints it.

(defstruct pol-pair a b)

(defmethod print-object ((n pol-pair) s)
  (pprint-logical-block (s nil :prefix "<" :suffix ">")
    (princ (pol-pair-a n) s)
    (pprint-newline :mandatory s)
    (princ (pol-pair-b n) s)))

(defun %pol (x)
  (let ((*print-pretty* t) (*print-right-margin* 75))
    (prin1-to-string x)))

(defun %pol-pair () (make-pol-pair :a "a" :b "b"))

(deftest pprint-print-object-in-list.lists
  (list (%pol (list 'zz (%pol-pair)))
        (%pol (list (%pol-pair)))
        (%pol (list 1 2 (%pol-pair) 3))
        (%pol (list :k (%pol-pair)))
        (%pol (list "s" (%pol-pair) (make-pol-pair :a "c" :b "d"))))
  ("(ZZ
 <a
  b>)"
   "(<a
  b>)"
   "(1 2
 <a
  b>
 3)"
   "(:K
 <a
  b>)"
   "(\"s\"
 <a
  b>
 <c
  d>)"))

(deftest pprint-print-object-in-list.vector-and-nested
  (list (%pol (vector 1 (%pol-pair)))
        (%pol (list 'let (list (list 'x (%pol-pair))) 'x)))
  ("#(1
  <a
   b>)"
   "(LET ((X
       <a
        b>))
  X)"))
