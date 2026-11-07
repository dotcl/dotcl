;;; PPRINT-FILL / PPRINT-LINEAR / PPRINT-LOGICAL-BLOCK / FORMAT ~< ~:> with *PRINT-LINES*,
;;; *PRINT-LEVEL* and *PRINT-LENGTH* over a nested list: depth x width x line limit.
;;; Expected strings are the output of another implementation (SBCL).

(defparameter *%pl2-obj* '((aaaa bbbb cccc) (dddd (eeee ffff) gggg) (hhhh iiii) jjjj kkkk))

(defun %pl2-run (prim level length lines margin)
  (let ((*print-pretty* t) (*print-level* level) (*print-length* length)
        (*print-lines* lines) (*print-right-margin* margin) (*print-circle* nil)
        (*print-miser-width* 20) (o *%pl2-obj*))
    (with-output-to-string (s)
      (ecase prim
        (plb (pprint-logical-block (s o :prefix "(" :suffix ")")
               (loop (pprint-exit-if-list-exhausted)
                     (write (pprint-pop) :stream s) (write-char #\Space s) (pprint-newline :fill s))))
        (plb-lin (pprint-logical-block (s o :prefix "[" :suffix "]")
                   (loop (pprint-exit-if-list-exhausted)
                         (write (pprint-pop) :stream s) (write-char #\Space s) (pprint-newline :linear s))))
        (fill (pprint-fill s o))
        (linear (pprint-linear s o))
        (fmt-block (format s "~<(~;~@{~S~^ ~:_~}~;)~:>" o))
        (fmt-nested (format s "~<(~;~@{~<[~;~@{~S~^ ~:_~}~;]~:>~^ ~_~}~;)~:>" o))))))

(defmacro %pl2-cases (&rest cases)
  `(progn
     ,@(loop for (prim level length lines margin expected) in cases
             for n from 1
             collect `(deftest ,(intern (format nil "PPRINT-LINES.~A.~D" prim n))
                        (%pl2-run ',prim ,level ,length ,lines ,margin)
                        ,expected))))

(%pl2-cases
  (fill 2 3 1 12 "((AAAA ..))")
  (fill 2 3 2 12 "((AAAA
  BBBB ..))")
  (fill 2 3 4 12 "((AAAA
  BBBB
  CCCC)
 (DDDD ..))")
  (fill 2 3 nil 30 "((AAAA BBBB CCCC)
 (DDDD # GGGG) (HHHH IIII)
 ...)")
  (fill 2 nil 2 30 "((AAAA BBBB CCCC)
 (DDDD # GGGG) (HHHH IIII) ..)")
  (fill 2 nil 4 30 "((AAAA BBBB CCCC)
 (DDDD # GGGG) (HHHH IIII)
 JJJJ KKKK)")
  (fill 2 nil nil 30 "((AAAA BBBB CCCC)
 (DDDD # GGGG) (HHHH IIII)
 JJJJ KKKK)")
  (fill nil 3 1 12 "((AAAA ..))")
  (fill nil 3 1 30 "((AAAA BBBB CCCC) ..)")
  (fill nil 3 2 12 "((AAAA
  BBBB ..))")
  (fill nil 3 nil 12 "((AAAA
  BBBB
  CCCC)
 (DDDD
  (EEEE
   FFFF)
  GGGG)
 (HHHH
  IIII)
 ...)")
  (fill nil nil 4 12 "((AAAA
  BBBB
  CCCC)
 (DDDD ..))")
  (fmt-block 2 3 4 12 "((AAAA
  BBBB
  CCCC)
 (DDDD ..))")
  (fmt-block 2 nil 1 12 "((AAAA ..))")
  (fmt-block 2 nil 2 30 "((AAAA BBBB CCCC)
 (DDDD # GGGG) (HHHH IIII) ..)")
  (fmt-block 2 nil nil 12 "((AAAA
  BBBB
  CCCC)
 (DDDD
  #
  GGGG)
 (HHHH
  IIII)
 JJJJ
 KKKK)")
  (fmt-block 2 nil nil 30 "((AAAA BBBB CCCC)
 (DDDD # GGGG) (HHHH IIII)
 JJJJ KKKK)")
  (fmt-block nil 3 2 12 "((AAAA
  BBBB ..))")
  (fmt-block nil 3 4 12 "((AAAA
  BBBB
  CCCC)
 (DDDD ..))")
  (fmt-block nil 3 4 30 "((AAAA BBBB CCCC)
 (DDDD (EEEE FFFF) GGGG)
 (HHHH IIII) ...)")
  (fmt-block nil nil 2 12 "((AAAA
  BBBB ..))")
  (fmt-block nil nil 4 12 "((AAAA
  BBBB
  CCCC)
 (DDDD ..))")
  (fmt-nested 2 nil 1 30 "([AAAA BBBB CCCC] ..)")
  (fmt-nested 2 nil 2 12 "([AAAA
  BBBB ..])")
  (fmt-nested 2 nil nil 12 "([AAAA
  BBBB
  CCCC]
 [DDDD
  #
  GGGG]
 [HHHH
  IIII]
 JJJJ
 KKKK)")
  (fmt-nested 2 nil nil 30 "([AAAA BBBB CCCC]
 [DDDD # GGGG]
 [HHHH IIII]
 JJJJ
 KKKK)")
  (fmt-nested nil 3 2 30 "([AAAA BBBB CCCC]
 [DDDD (EEEE FFFF) GGGG] ..)")
  (fmt-nested nil 3 4 12 "([AAAA
  BBBB
  CCCC]
 [DDDD ..])")
  (fmt-nested nil nil 1 30 "([AAAA BBBB CCCC] ..)")
  (linear 2 3 1 12 "((AAAA ..))")
  (linear 2 3 1 30 "((AAAA BBBB CCCC) ..)")
  (linear 2 3 2 12 "((AAAA
  BBBB ..))")
  (linear 2 3 2 30 "((AAAA BBBB CCCC)
 (DDDD # GGGG) ..)")
  (linear 2 3 nil 12 "((AAAA
  BBBB
  CCCC)
 (DDDD
  #
  GGGG)
 (HHHH
  IIII)
 ...)")
  (linear nil 3 4 12 "((AAAA
  BBBB
  CCCC)
 (DDDD ..))")
  (linear nil 3 4 30 "((AAAA BBBB CCCC)
 (DDDD (EEEE FFFF) GGGG)
 (HHHH IIII)
 ...)")
  (linear nil 3 nil 30 "((AAAA BBBB CCCC)
 (DDDD (EEEE FFFF) GGGG)
 (HHHH IIII)
 ...)")
  (linear nil nil 1 30 "((AAAA BBBB CCCC) ..)")
  (linear nil nil 2 12 "((AAAA
  BBBB ..))")
  (linear nil nil 4 12 "((AAAA
  BBBB
  CCCC)
 (DDDD ..))")
  (plb 2 3 1 30 "((AAAA BBBB CCCC) ..)")
  (plb 2 3 4 12 "((AAAA
  BBBB
  CCCC)
 (DDDD ..))")
  (plb 2 3 4 30 "((AAAA BBBB CCCC)
 (DDDD # GGGG) (HHHH IIII)
 ...)")
  (plb 2 nil 4 12 "((AAAA
  BBBB
  CCCC)
 (DDDD ..))")
  (plb nil 3 4 30 "((AAAA BBBB CCCC)
 (DDDD (EEEE FFFF) GGGG)
 (HHHH IIII) ...)")
  (plb nil 3 nil 12 "((AAAA
  BBBB
  CCCC)
 (DDDD
  (EEEE
   FFFF)
  GGGG)
 (HHHH
  IIII)
 ...)")
  (plb nil nil 1 12 "((AAAA ..))")
  (plb nil nil 1 30 "((AAAA BBBB CCCC) ..)")
  (plb nil nil 2 30 "((AAAA BBBB CCCC)
 (DDDD (EEEE FFFF) GGGG) ..)")
  (plb nil nil 4 12 "((AAAA
  BBBB
  CCCC)
 (DDDD ..))")
  (plb-lin 2 3 1 30 "[(AAAA BBBB CCCC) ..]")
  (plb-lin 2 nil 1 30 "[(AAAA BBBB CCCC) ..]")
  (plb-lin 2 nil 4 12 "[(AAAA
  BBBB
  CCCC)
 (DDDD ..)]")
  (plb-lin 2 nil 4 30 "[(AAAA BBBB CCCC)
 (DDDD # GGGG)
 (HHHH IIII)
 JJJJ ..]")
  (plb-lin nil 3 2 30 "[(AAAA BBBB CCCC)
 (DDDD (EEEE FFFF) GGGG) ..]")
  (plb-lin nil 3 4 12 "[(AAAA
  BBBB
  CCCC)
 (DDDD ..)]")
  (plb-lin nil 3 nil 30 "[(AAAA BBBB CCCC)
 (DDDD (EEEE FFFF) GGGG)
 (HHHH IIII)
 ...]")
  (plb-lin nil nil 2 12 "[(AAAA
  BBBB ..)]")
  (plb-lin nil nil 2 30 "[(AAAA BBBB CCCC)
 (DDDD (EEEE FFFF) GGGG) ..]")
  (plb-lin nil nil 4 30 "[(AAAA BBBB CCCC)
 (DDDD (EEEE FFFF) GGGG)
 (HHHH IIII)
 JJJJ ..]"))
