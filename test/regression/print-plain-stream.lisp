;;; PRINC / PRIN1 / PRINT of a string, a fixnum or a symbol to a file or string stream
;;; write the text straight to the stream. Output, return values, the line-start flag
;;; and the errors are the same as through the printer; the cases cover the printer
;;; variables, a pprint dispatch entry, a logical block, composite and closed streams.
;;; The expected values are the ones of the build before the shortcut.

(defun to-str (fn) (with-output-to-string (o) (funcall fn o)))
(defpackage :ppx (:use) (:export "EXP"))
(defvar *u* (make-symbol "UNINT"))

(deftest print-plain-stream.1
  (handler-case (multiple-value-list (to-str (LAMBDA (O) (PRINC "ab" O) (PRINC 42 O) (PRINC (QUOTE FOO) O) (PRINC :KW O) (PRINC *U* O) (PRINC (QUOTE |a b|) O) (PRINC (QUOTE PPX:EXP) O)))) (error () (list :error)))
  ("ab42FOOKWUNINTa bEXP"))

(deftest print-plain-stream.2
  (handler-case (multiple-value-list (to-str (LAMBDA (O) (PRIN1 "a\"b\\c" O) (PRIN1 -7 O) (PRIN1 (QUOTE FOO) O) (PRIN1 :KW O) (PRIN1 *U* O) (PRIN1 (QUOTE |a b|) O) (PRIN1 (QUOTE PPX:EXP) O) (PRIN1 (QUOTE CAR) O)))) (error () (list :error)))
  ("\"a\\\"b\\\\c\"-7FOO:KW#:UNINT|a b|PPX:EXPCAR"))

(deftest print-plain-stream.3
  (handler-case (multiple-value-list (to-str (LAMBDA (O) (PRINT "x" O) (PRINT 5 O) (PRINT (QUOTE SYM) O) (PRINT :K O)))) (error () (list :error)))
  ("
\"x\" 
5 
SYM 
:K "))

(deftest print-plain-stream.4
  (handler-case (multiple-value-list (to-str (LAMBDA (O) (PRINC "ab" O) (FRESH-LINE O) (PRINC "c" O) (TERPRI O) (FRESH-LINE O) (PRINC "" O) (FRESH-LINE O) (PRINC "d
" O) (FRESH-LINE O)))) (error () (list :error)))
  ("ab
c
d
"))

(deftest print-plain-stream.5
  (handler-case (multiple-value-list (to-str (LAMBDA (O) (PRINT 1 O) (FRESH-LINE O) (PRIN1 "z
" O) (FRESH-LINE O)))) (error () (list :error)))
  ("
1 
\"z
\"
"))

(deftest print-plain-stream.6
  (handler-case (multiple-value-list (to-str (LAMBDA (O) (LET ((*PRINT-BASE* 16)) (PRINC 255 O) (PRIN1 255 O) (PRINT 255 O)) (LET ((*PRINT-BASE* 2) (*PRINT-RADIX* T)) (PRINC 5 O) (PRIN1 5 O))))) (error () (list :error)))
  ("FFFF
FF #b101#b101"))

(deftest print-plain-stream.7
  (handler-case (multiple-value-list (to-str (LAMBDA (O) (LET ((*PRINT-CASE* :DOWNCASE)) (PRINC (QUOTE FOO) O) (PRIN1 (QUOTE FOO) O) (PRINC :BAR O) (PRIN1 :BAR O) (PRINC (QUOTE |MiXed|) O) (PRIN1 (QUOTE |MiXed|) O)) (LET ((*PRINT-CASE* :CAPITALIZE)) (PRINC (QUOTE FOO-BAR) O) (PRIN1 (QUOTE FOO-BAR) O))))) (error () (list :error)))
  ("foofoobar:barmixed|MiXed|Foo-BarFoo-Bar"))

(deftest print-plain-stream.8
  (handler-case (multiple-value-list (to-str (LAMBDA (O) (LET ((*PRINT-READABLY* T)) (PRIN1 "s" O) (PRIN1 3 O) (PRIN1 (QUOTE FOO) O) (PRIN1 :K O))))) (error () (list :error)))
  ("\"s\"3FOO:K"))

(deftest print-plain-stream.9
  (handler-case (multiple-value-list (to-str (LAMBDA (O) (LET ((*PRINT-READABLY* T)) (PRINC "s" O) (PRINC 3 O) (PRINC (QUOTE FOO) O) (PRINC :K O))))) (error () (list :error)))
  ("s3FOOK"))

(deftest print-plain-stream.10
  (handler-case (multiple-value-list (to-str (LAMBDA (O) (LET ((*PRINT-ESCAPE* NIL)) (PRIN1 "s" O) (PRIN1 (QUOTE FOO) O)) (LET ((*PRINT-ESCAPE* T)) (PRINC "s" O) (PRINC (QUOTE FOO) O))))) (error () (list :error)))
  ("\"s\"FOOsFOO"))

(deftest print-plain-stream.11
  (handler-case (multiple-value-list (to-str (LAMBDA (O) (LET ((*PRINT-GENSYM* NIL)) (PRIN1 *U* O) (PRINC *U* O))))) (error () (list :error)))
  ("UNINTUNINT"))

(deftest print-plain-stream.12
  (handler-case (multiple-value-list (to-str (LAMBDA (O) (LET ((*PACKAGE* (FIND-PACKAGE :PPX))) (PRIN1 (QUOTE CAR) O) (PRIN1 (QUOTE PPX::ZZ) O) (PRIN1 (QUOTE EXP) O) (PRIN1 :KK O))))) (error () (list :error)))
  ("COMMON-LISP:CARZZCOMMON-LISP:EXP:KK"))

(deftest print-plain-stream.13
  (handler-case (multiple-value-list (to-str (LAMBDA (O) (LET ((*PRINT-PRETTY* T)) (PRINC "p" O) (PRIN1 (QUOTE Q) O) (PRINC 12 O))))) (error () (list :error)))
  ("pQ12"))

(deftest print-plain-stream.14
  (handler-case (multiple-value-list (to-str (LAMBDA (O) (LET ((*PRINT-PRETTY* T) (*PRINT-PPRINT-DISPATCH* (COPY-PPRINT-DISPATCH))) (SET-PPRINT-DISPATCH (QUOTE STRING) (LAMBDA (S X) (WRITE-STRING "<STR>" S))) (PRINC "p" O) (PRIN1 "q" O) (PRINT (QUOTE Q) O))))) (error () (list :error)))
  ("<STR><STR>
Q "))

(deftest print-plain-stream.15
  (handler-case (multiple-value-list (to-str (LAMBDA (O) (PPRINT-LOGICAL-BLOCK (O NIL :PREFIX "[" :SUFFIX "]") (PRINC "a" O) (PRIN1 (QUOTE B) O) (PRINC 1 O))))) (error () (list :error)))
  ("[aB1]"))

(deftest print-plain-stream.16
  (handler-case (multiple-value-list (to-str (LAMBDA (O) (PPRINT-LOGICAL-BLOCK (O NIL :PER-LINE-PREFIX ";") (PRINC "a" O) (PPRINT-NEWLINE :MANDATORY O) (PRINT (QUOTE B) O))))) (error () (list :error)))
  (";a
;
B "))

(deftest print-plain-stream.17
  (handler-case (multiple-value-list (LET ((*STANDARD-OUTPUT* (MAKE-STRING-OUTPUT-STREAM))) (PRINC "x") (PRIN1 (QUOTE Y)) (PRINT 3) (FRESH-LINE) (GET-OUTPUT-STREAM-STRING *STANDARD-OUTPUT*))) (error () (list :error)))
  ("xY
3 
"))

(deftest print-plain-stream.18
  (handler-case (multiple-value-list (WITH-OUTPUT-TO-STRING (*STANDARD-OUTPUT*) (PRINC "x") (PRIN1 (QUOTE Y)) (PRINT 3) (PRIN1 "q" NIL))) (error () (list :error)))
  ("xY
3 \"q\""))

(deftest print-plain-stream.19
  (handler-case (multiple-value-list (LIST (PRINC "r" (MAKE-BROADCAST-STREAM)) (PRIN1 4 (MAKE-BROADCAST-STREAM)) (PRINT (QUOTE A) (MAKE-BROADCAST-STREAM)))) (error () (list :error)))
  (("r" 4 A)))

(deftest print-plain-stream.20
  (handler-case (multiple-value-list (LET ((A (MAKE-STRING-OUTPUT-STREAM)) (B (MAKE-STRING-OUTPUT-STREAM))) (LET ((S (MAKE-BROADCAST-STREAM A B))) (PRINC "x" S) (PRIN1 (QUOTE Y) S) (PRINT 1 S)) (LIST (GET-OUTPUT-STREAM-STRING A) (GET-OUTPUT-STREAM-STRING B)))) (error () (list :error)))
  (("xY
1 " "xY
1 ")))

(deftest print-plain-stream.21
  (handler-case (multiple-value-list (LET ((O (MAKE-STRING-OUTPUT-STREAM))) (CLOSE O) (PRINC "x" O))) (error () (list :error)))
  (:ERROR))

(deftest print-plain-stream.22
  (handler-case (multiple-value-list (LET ((O (MAKE-STRING-OUTPUT-STREAM))) (CLOSE O) (PRIN1 (QUOTE X) O))) (error () (list :error)))
  (:ERROR))

(deftest print-plain-stream.23
  (handler-case (multiple-value-list (LET ((O (MAKE-STRING-OUTPUT-STREAM))) (CLOSE O) (PRINT 4 O))) (error () (list :error)))
  (:ERROR))

(deftest print-plain-stream.28
  (handler-case (multiple-value-list (LET ((O (MAKE-STRING-OUTPUT-STREAM))) (LIST (PRINC "a" O) (PRIN1 "a" O) (PRINT "a" O) (GET-OUTPUT-STREAM-STRING O)))) (error () (list :error)))
  (("a" "a" "a" "a\"a\"
\"a\" ")))

(deftest print-plain-stream.29
  (handler-case (multiple-value-list (LET ((O (MAKE-STRING-OUTPUT-STREAM :ELEMENT-TYPE (QUOTE BASE-CHAR)))) (PRINC "a" O) (PRIN1 (QUOTE BC) O) (GET-OUTPUT-STREAM-STRING O))) (error () (list :error)))
  ("aBC"))

(deftest print-plain-stream.30
  (handler-case (multiple-value-list (LET ((S (MAKE-ARRAY 3 :ELEMENT-TYPE (QUOTE CHARACTER) :FILL-POINTER 0 :ADJUSTABLE T))) (WITH-OUTPUT-TO-STRING (O S) (PRINC "ab" O) (PRIN1 (QUOTE C) O) (PRINT 1 O)) S)) (error () (list :error)))
  ("abC
1 "))

(deftest print-plain-stream.31
  (handler-case (multiple-value-list (LET ((O (MAKE-STRING-OUTPUT-STREAM))) (PRINC (coerce (list (code-char 20320) (code-char 22909)) (quote string)) O) (PRIN1 (string (code-char 233)) O) (GET-OUTPUT-STREAM-STRING O))) (error () (list :error)))
  (#.(concatenate (quote string) (list (code-char 20320) (code-char 22909) #\" (code-char 233) #\"))))

(deftest print-plain-stream.32
  (handler-case (multiple-value-list (LET ((O (MAKE-STRING-OUTPUT-STREAM))) (PRINC (MAKE-STRING 3 :INITIAL-ELEMENT #\a) O) (PRINC (COERCE "xy" (QUOTE SIMPLE-BASE-STRING)) O) (GET-OUTPUT-STREAM-STRING O))) (error () (list :error)))
  ("aaaxy"))

(deftest print-plain-stream.33
  (handler-case (multiple-value-list (LET ((O (MAKE-STRING-OUTPUT-STREAM))) (PRINC (MOST-POSITIVE-FIXNUM) O))) (error () (list :error)))
  (:ERROR))

(deftest print-plain-stream.34
  (handler-case (multiple-value-list (LET ((O (MAKE-STRING-OUTPUT-STREAM))) (PRINC MOST-POSITIVE-FIXNUM O) (PRINC MOST-NEGATIVE-FIXNUM O) (PRINC (1+ MOST-POSITIVE-FIXNUM) O) (GET-OUTPUT-STREAM-STRING O))) (error () (list :error)))
  ("9223372036854775807-92233720368547758089223372036854775808"))

(deftest print-plain-stream.35
  (handler-case (multiple-value-list (LET ((O (MAKE-STRING-OUTPUT-STREAM))) (PRINC NIL O) (PRINC T O) (PRIN1 NIL O) (PRIN1 T O) (PRINC (QUOTE NIL) O) (GET-OUTPUT-STREAM-STRING O))) (error () (list :error)))
  ("NILTNILTNIL"))

(deftest print-plain-stream.36
  (handler-case (multiple-value-list (LET ((O (MAKE-STRING-OUTPUT-STREAM))) (PRIN1 (QUOTE ||) O) (PRIN1 (QUOTE |1|) O) (PRIN1 (QUOTE |a.b|) O) (PRIN1 (QUOTE |:x|) O) (PRIN1 (INTERN "lower") O) (GET-OUTPUT-STREAM-STRING O))) (error () (list :error)))
  ("|||1||a.b||:x||lower|"))

(deftest print-plain-stream.37
  (handler-case (multiple-value-list (FORMAT NIL "~a ~a ~a ~s" (QUOTE FOO) :BAR "q" (QUOTE FOO))) (error () (list :error)))
  ("FOO BAR q FOO"))

(deftest print-plain-stream.38
  (handler-case (multiple-value-list (LET ((*PRINT-READABLY* T)) (FORMAT NIL "~a ~a" (QUOTE FOO) "s"))) (error () (list :error)))
  ("FOO s"))

(deftest print-plain-stream.39
  (handler-case (multiple-value-list (LET ((*PRINT-ESCAPE* T)) (FORMAT NIL "~a" (QUOTE FOO)))) (error () (list :error)))
  ("FOO"))

(deftest print-plain-stream.40
  (handler-case (multiple-value-list (LET ((*PRINT-CIRCLE* T)) (to-str (LAMBDA (O) (PRINC "a" O) (PRIN1 (QUOTE B) O) (PRINT *U* O))))) (error () (list :error)))
  ("aB
#:UNINT "))

(deftest print-plain-stream.41
  (handler-case (multiple-value-list (LET ((*PRINT-PRETTY* NIL)) (to-str (LAMBDA (O) (PRINC (QUOTE FOO) O) (PRINT (QUOTE BAR) O))))) (error () (list :error)))
  ("FOO
BAR "))

(deftest print-plain-stream.42
  (handler-case (multiple-value-list (LET ((*PRINT-LINES* 1)) (to-str (LAMBDA (O) (PRINC "a" O))))) (error () (list :error)))
  ("a"))

(deftest print-plain-stream.43
  (handler-case (multiple-value-list (LET ((*PRINT-LENGTH* 1) (*PRINT-LEVEL* 1)) (to-str (LAMBDA (O) (PRINC "abc" O) (PRIN1 (QUOTE SYM) O) (PRIN1 12345 O))))) (error () (list :error)))
  ("abcSYM12345"))


(deftest print-plain-stream.file
  (let ((path (regression-temp-file "pps-file")))
    (unwind-protect
         (progn
           (with-open-file (f path :direction :output :if-exists :supersede)
             (princ "ab" f) (prin1 'c f) (print 7 f) (princ "tail" f) (fresh-line f) (prin1 "x" f))
           (with-open-file (f path) (list (read-line f) (read-line f) (read-line f nil) (read-line f nil :eof))))
      (ignore-errors (delete-file path))))
  ("abC" "7 tail" "\"x\"" :eof))
