;;; ~A of a string or a fixnum skips the printer's state handling when no pretty-printer
;;; block is open and no pprint dispatch entry could apply. The results below are those
;;; of the full path (the build before the shortcut), for *print-base*, *print-radix*,
;;; *print-pretty*, user pprint dispatch entries, logical blocks and the ~A parameters.

(defparameter *fap-forms*
  '((format nil "~a" 42)
    (format nil "~a" -7)
    (format nil "~a" 12345678901234567890)
    (format nil "~a" "str")
    (format nil "[~a]" "")
    (format nil "~a" (format nil "a~%b"))
    (let ((*print-base* 16)) (format nil "~a" 255))
    (let ((*print-base* 2)) (format nil "~a ~a" 5 "x"))
    (let ((*print-radix* t)) (format nil "~a" 10))
    (let ((*print-radix* t) (*print-base* 8)) (format nil "~a" 10))
    (let ((*print-pretty* nil)) (format nil "~a ~a" 1 "z"))
    (let ((*print-readably* t)) (format nil "~a" "s"))
    (let ((*print-escape* t)) (format nil "~a" "s"))
    (format nil "~5a|" "ab")
    (format nil "~5@a|" 12)
    (format nil "~:a" "x")
    (format nil "~@a" "x")
    (format nil "~a~a" 1 2)
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch))) (set-pprint-dispatch (quote integer) (lambda (s o) (format s "<int ~d>" o))) (let ((*print-pretty* t)) (format nil "~a ~a" 5 "s")))
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch))) (set-pprint-dispatch (quote string) (lambda (s o) (format s "<str ~a>" (length o)))) (let ((*print-pretty* t)) (format nil "~a ~a" 5 "abc")))
    (let ((*print-pprint-dispatch* (copy-pprint-dispatch))) (set-pprint-dispatch (quote integer) (lambda (s o) (format s "<int ~d>" o))) (let ((*print-pretty* nil)) (format nil "~a" 5)))
    (with-output-to-string (o) (pprint-logical-block (o (quote (1 2))) (format o "~a-~a" 1 "x")))
    (with-output-to-string (o) (pprint-logical-block (o nil :prefix "(" :suffix ")") (format o "a~%~a" "b")))
    (format nil "~{~a~^ ~}" (quote (1 "two" 3)))
    (format nil "~a" (quote sym))
    (format nil "~a" nil)
    (let ((*print-circle* t)) (format nil "~a ~a" 1 "s"))
    (let ((*print-level* 0)) (format nil "~a" 5))
    (let ((*print-length* 0)) (format nil "~a" "abc"))
    (let ((*print-lines* 1) (*print-right-margin* 5)) (format nil "~a" "long string here"))
    (format nil "~a" (make-array 3 :element-type (quote character) :initial-element #\z))
    (format nil "~a" (make-array 3 :element-type (quote character) :adjustable t :fill-pointer 2 :initial-element #\z))
    (let ((s (make-string-output-stream))) (format s "~a/~a" 1 "q") (get-output-stream-string s))
    (format nil "~va" 4 "ab")
    (format nil "~a" most-positive-fixnum)
    (format nil "~a" (1+ most-positive-fixnum))
    ))

(deftest format-a-shortcut.cases
  (mapcar (lambda (f) (handler-case (eval f) (error (e) (list :error (type-of e))))) *fap-forms*)
  ("42"
   "-7"
   "12345678901234567890"
   "str"
   "[]"
   "a
b"
   "FF"
   "101 x"
   "10."
   "#o12"
   "1 z"
   "s"
   "s"
   "ab   |"
   "   12|"
   "x"
   "x"
   "12"
   "<int 5> s"
   "5 <str 3>"
   "5"
   "1-x"
   "(a
b)"
   "1 two 3"
   "SYM"
   "NIL"
   "1 s"
   "5"
   "abc"
   "long string here"
   "zzz"
   "zz"
   "1/q"
   "ab  "
   "9223372036854775807"
   "9223372036854775808"
   ))
