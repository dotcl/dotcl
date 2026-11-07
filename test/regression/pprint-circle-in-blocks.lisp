;;; *PRINT-CIRCLE* with PPRINT-FILL / LINEAR / PPRINT-LOGICAL-BLOCK / FORMAT ~< ~:>
;;; over lists that share structure, with and without *PRINT-LENGTH* / *PRINT-LEVEL*.
;;; Expected strings are the output of another implementation (SBCL).

(defparameter *%pc-obj* '((aaaa bbbb cccc) (dddd (eeee ffff) gggg) (hhhh iiii) jjjj kkkk))
(defparameter *%pc-shared* (let ((x (list 'x 'y))) (list x x (list x))))

(defun %pc-run (prim which level length margin)
  (let ((*print-pretty* t) (*print-level* level) (*print-length* length)
        (*print-lines* nil) (*print-right-margin* margin) (*print-circle* t)
        (*print-miser-width* 20)
        (o (if (eq which 'obj) *%pc-obj* *%pc-shared*)))
    (with-output-to-string (s)
      (ecase prim
        (write (write o :stream s))
        (plb (pprint-logical-block (s o :prefix "(" :suffix ")")
               (loop (pprint-exit-if-list-exhausted)
                     (write (pprint-pop) :stream s) (write-char #\Space s) (pprint-newline :fill s))))
        (fill (pprint-fill s o))
        (fill-nocolon (pprint-fill s o nil))
        (linear (pprint-linear s o))
        (fmt-block (format s "~<(~;~@{~S~^ ~:_~}~;)~:>" o))
        (fmt-nested (format s "~<(~;~@{~<[~;~@{~S~^ ~:_~}~;]~:>~^ ~_~}~;)~:>" o))))))

(defmacro %pc-cases (&rest cases)
  `(progn
     ,@(loop for (prim which level length margin expected) in cases
             for n from 1
             collect `(deftest ,(intern (format nil "PPRINT-CIRCLE.~A.~D" prim n))
                        (%pc-run ',prim ',which ,level ,length ,margin)
                        ,expected))))

(%pc-cases
  (fill obj 2 1 30 "((AAAA ...) ...)")
  (fill obj 2 3 12 "((AAAA
  BBBB
  CCCC)
 (DDDD
  #
  GGGG)
 (HHHH
  IIII)
 ...)")
  (fill obj 2 nil 12 "((AAAA
  BBBB
  CCCC)
 (DDDD
  #
  GGGG)
 (HHHH
  IIII)
 JJJJ
 KKKK)")
  (fill obj nil nil 12 "((AAAA
  BBBB
  CCCC)
 (DDDD
  (EEEE
   FFFF)
  GGGG)
 (HHHH
  IIII)
 JJJJ
 KKKK)")
  (fill shr 1 1 30 "(# ...)")
  (fill shr 1 3 12 "(#1=# #1# #)")
  (fill shr 1 nil 30 "(#1=# #1# #)")
  (fill shr 2 nil 30 "(#1=(X Y) #1# (#1#))")
  (fill shr nil 1 12 "((X ...)
 ...)")
  (fill shr nil 3 12 "(#1=(X Y)
 #1#
 (#1#))")
  (fill shr nil nil 12 "(#1=(X Y)
 #1#
 (#1#))")
  (fill-nocolon obj 1 1 12 "# ...")
  (fill-nocolon obj 2 3 30 "(AAAA BBBB CCCC)
(DDDD # GGGG) (HHHH IIII) ...")
  (fill-nocolon obj nil nil 12 "(AAAA
 BBBB
 CCCC)
(DDDD
 (EEEE
  FFFF)
 GGGG)
(HHHH IIII)
JJJJ
KKKK")
  (fill-nocolon shr 2 nil 12 "#1=(X Y)
#1#
(#1#)")
  (fill-nocolon shr 2 nil 30 "#1=(X Y) #1# (#1#)")
  (fill-nocolon shr nil 1 30 "(X ...) ...")
  (fill-nocolon shr nil nil 12 "#1=(X Y)
#1#
(#1#)")
  (fmt-block obj 1 nil 30 "(# # # JJJJ KKKK)")
  (fmt-block obj 2 1 30 "((AAAA ...) ...)")
  (fmt-block obj 2 3 12 "((AAAA
  BBBB
  CCCC)
 (DDDD
  #
  GGGG)
 (HHHH
  IIII)
 ...)")
  (fmt-block obj nil 3 12 "((AAAA
  BBBB
  CCCC)
 (DDDD
  (EEEE
   FFFF)
  GGGG)
 (HHHH
  IIII)
 ...)")
  (fmt-block obj nil 3 30 "((AAAA BBBB CCCC)
 (DDDD (EEEE FFFF) GGGG)
 (HHHH IIII) ...)")
  (fmt-block shr 2 3 12 "(#1=(X Y)
 #1#
 (#1#))")
  (fmt-block shr nil 1 30 "((X ...) ...)")
  (fmt-nested obj 1 1 12 "(# ...)")
  (fmt-nested obj 1 3 12 "(# # # ...)")
  (fmt-nested obj 1 nil 30 "(# # # JJJJ KKKK)")
  (fmt-nested obj 2 3 30 "([AAAA BBBB CCCC]
 [DDDD # GGGG]
 [HHHH IIII]
 ...)")
  (fmt-nested obj 2 nil 12 "([AAAA
  BBBB
  CCCC]
 [DDDD
  #
  GGGG]
 [HHHH
  IIII]
 JJJJ
 KKKK)")
  (fmt-nested obj nil nil 30 "([AAAA BBBB CCCC]
 [DDDD (EEEE FFFF) GGGG]
 [HHHH IIII]
 JJJJ
 KKKK)")
  (fmt-nested shr 2 1 30 "([X ...] ...)")
  (fmt-nested shr 2 3 12 "(#1=[X Y]
 #1#
 [#1#])")
  (linear obj 1 3 12 "(# # # ...)")
  (linear obj 2 1 12 "((AAAA ...)
 ...)")
  (linear obj nil 1 30 "((AAAA ...) ...)")
  (linear obj nil 3 12 "((AAAA
  BBBB
  CCCC)
 (DDDD
  (EEEE
   FFFF)
  GGGG)
 (HHHH
  IIII)
 ...)")
  (linear shr 2 3 30 "(#1=(X Y) #1# (#1#))")
  (plb obj 1 1 12 "(# ...)")
  (plb obj 1 nil 30 "(# # # JJJJ KKKK )")
  (plb obj 2 1 30 "((AAAA ...) ...)")
  (plb obj nil 3 30 "((AAAA BBBB CCCC)
 (DDDD (EEEE FFFF) GGGG)
 (HHHH IIII) ...)")
  (plb shr 1 1 12 "(# ...)")
  (plb shr 1 nil 12 "(#1=#
 #1#
 #
 )")
  (plb shr 2 1 12 "((X ...)
 ...)")
  (plb shr 2 nil 12 "(#1=(X Y)
 #1#
 (#1#)
 )")
  (plb shr nil nil 30 "(#1=(X Y) #1# (#1#) )")
  (write shr 1 3 12 "(#1=# #1# #)"))
