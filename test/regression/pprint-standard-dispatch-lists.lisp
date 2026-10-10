;;; The standard pprint dispatch table for lists and vectors: under
;;; *PRINT-PRETTY* a list longer than the right margin is broken (CLHS
;;; 22.2.1.4). A data list fills its lines; a call of a function puts its
;;; arguments after the operator. Expected values are SBCL 2.6.8's output.

(defun %psd-print (object margin &rest bindings)
  (let ((*print-right-margin* margin)
        (*print-pretty* t)
        (*print-escape* t)
        (*print-readably* nil)
        (*print-length* (getf bindings :length))
        (*print-level* (getf bindings :level))
        (*print-lines* (getf bindings :lines))
        (*print-circle* (getf bindings :circle))
        (*print-miser-width* nil))
    (prin1-to-string object)))

(defun %psd-lines (&rest lines)
  (format nil "~{~a~^~%~}" lines))

(deftest pprint-standard-dispatch.fill
  (%psd-print '(aaaa bbbb cccc dddd eeee ffff gggg hhhh iiii) 30)
  #.(%psd-lines "(AAAA BBBB CCCC DDDD EEEE"
                " FFFF GGGG HHHH IIII)"))

(deftest pprint-standard-dispatch.fits-on-one-line
  (%psd-print '(aaaa bbbb cccc) 30)
  "(AAAA BBBB CCCC)")

(deftest pprint-standard-dispatch.nested
  (%psd-print '(aaaa (bbbb cccc dddd eeee ffff) gggg
                (hhhh iiii jjjj kkkk llll mmmm nnnn) oooo)
              30)
  #.(%psd-lines "(AAAA"
                " (BBBB CCCC DDDD EEEE FFFF)"
                " GGGG"
                " (HHHH IIII JJJJ KKKK LLLL"
                "  MMMM NNNN)"
                " OOOO)"))

(deftest pprint-standard-dispatch.numbers
  (%psd-print (loop for i below 40 collect i) 40)
  #.(%psd-lines "(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15"
                " 16 17 18 19 20 21 22 23 24 25 26 27 28"
                " 29 30 31 32 33 34 35 36 37 38 39)"))

(deftest pprint-standard-dispatch.function-call
  (%psd-print '(car (cdr (list aaaaaaa bbbbbbbbb cccccccccc ddddddddd)
                     eeeeeeeeee ffffffffff)
                ggggggggg)
              40)
  #.(%psd-lines "(CAR"
                " (CDR"
                "  (LIST AAAAAAA BBBBBBBBB CCCCCCCCCC"
                "        DDDDDDDDD)"
                "  EEEEEEEEEE FFFFFFFFFF)"
                " GGGGGGGGG)"))

(deftest pprint-standard-dispatch.quote
  (%psd-print '(list 'aaaaaaaaa #'bbbbbbbbbb (quote ccccccc dddddd)
                '(eeeeeeee ffffffff gggggggg hhhhhhh))
              30)
  #.(%psd-lines "(LIST 'AAAAAAAAA #'BBBBBBBBBB"
                "      (QUOTE CCCCCCC DDDDDD)"
                "      '(EEEEEEEE FFFFFFFF"
                "        GGGGGGGG HHHHHHH))"))

(deftest pprint-standard-dispatch.vector
  (%psd-print #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 20)
  #.(%psd-lines "#(1 2 3 4 5 6 7 8 9"
                "  10 11 12 13 14 15"
                "  16 17 18 19 20)"))

(deftest pprint-standard-dispatch.dotted
  (%psd-print '(a b . c) 4)
  #.(%psd-lines "(A"
                " B"
                " . C)"))

(deftest pprint-standard-dispatch.print-length
  (%psd-print '(1 2 3 4 5 6 7 8) 8 :length 5)
  #.(%psd-lines "(1 2 3"
                " 4 5"
                " ...)"))

(deftest pprint-standard-dispatch.print-level
  (%psd-print '(a (b (c (d))) e) 80 :level 2)
  "(A (B #) E)")

(deftest pprint-standard-dispatch.print-lines
  (%psd-print '(aaaa (bbbb cccc dddd eeee ffff) gggg
                (hhhh iiii jjjj kkkk llll mmmm nnnn) oooo)
              20 :lines 2)
  #.(%psd-lines "(AAAA"
                " (BBBB CCCC DDDD ..))"))

(deftest pprint-standard-dispatch.print-circle
  (%psd-print (let ((x (list 'aaaaa 'bbbbb 'cccccc))) (list x x x x)) 20 :circle t)
  #.(%psd-lines "(#1=(AAAAA BBBBB"
                "     CCCCCC)"
                " #1# #1# #1#)"))

(deftest pprint-standard-dispatch.string-with-newline
  (%psd-print (list (format nil "multi~%line") 'aaaa 'bbbb) 80)
  #.(%psd-lines "(\"multi"
                "line\""
                " AAAA BBBB)"))

;; A trailing blank that belongs to the printed object (#\Space) is kept
;; when the line breaks after it.
(deftest pprint-standard-dispatch.significant-space
  (%psd-print '(#\a #\Space #\b #\c #\d #\e #\f #\g #\h #\i #\j) 20)
  #.(%psd-lines "(#\\a #\\  #\\b #\\c"
                " #\\d #\\e #\\f #\\g"
                " #\\h #\\i #\\j)"))

;; The column the object starts at counts.
(deftest pprint-standard-dispatch.start-column
  (let ((*print-right-margin* 30) (*print-pretty* t))
    (with-output-to-string (s)
      (write-string "value: " s)
      (prin1 '(aaaa bbbb cccc dddd eeee ffff gggg) s)))
  #.(%psd-lines "value: (AAAA BBBB CCCC DDDD"
                "        EEEE FFFF GGGG)"))

;; A user entry takes precedence over the standard ones, and the standard
;; entries are found by PPRINT-DISPATCH.
(deftest pprint-standard-dispatch.user-entry-first
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
        (*print-pretty* t))
    (set-pprint-dispatch '(cons (eql zork)) (lambda (s o) (declare (ignore o)) (write-string "<zork>" s)))
    (list (prin1-to-string '(a (zork 1 2) b))
          (nth-value 1 (pprint-dispatch '(a b)))
          (functionp (pprint-dispatch '(a b)))))
  ("(A <zork> B)" t t))

;; Without *PRINT-PRETTY* nothing breaks.
(deftest pprint-standard-dispatch.not-pretty
  (let ((*print-right-margin* 10) (*print-pretty* nil))
    (prin1-to-string '(aaaa bbbb cccc dddd)))
  "(AAAA BBBB CCCC DDDD)")
