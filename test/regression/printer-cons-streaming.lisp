;;; Printing a list appends into one builder: a nested list goes straight into
;;; the parent's builder instead of producing its own string, and the runaway
;;; guard for a circular CDR chain is two pointers rather than a per-list table.
;;; The shapes below are what those two changes can break.

(deftest printer-cons.basic
  (list (prin1-to-string '(1 2 3))
        (prin1-to-string '(1 (2 (3 (4)))))
        (prin1-to-string '(1 2 . 3))
        (prin1-to-string '(()))
        (prin1-to-string nil))
  ("(1 2 3)" "(1 (2 (3 (4))))" "(1 2 . 3)" "(NIL)" "NIL"))

(deftest printer-cons.dotted-nested
  (prin1-to-string '((a . b) (c . (d . e))))
  "((A . B) (C D . E))")

(deftest printer-cons.print-length
  (let ((*print-length* 3)) (prin1-to-string '(1 2 3 4 5)))
  "(1 2 3 ...)")

(deftest printer-cons.print-length-nested
  (let ((*print-length* 2)) (prin1-to-string '((1 2 3) (4 5 6) (7 8 9))))
  "((1 2 ...) (4 5 ...) ...)")

(deftest printer-cons.print-level
  (let ((*print-level* 2)) (prin1-to-string '(1 (2 (3 (4))))))
  "(1 (2 #))")

(deftest printer-cons.print-level-and-length
  (let ((*print-level* 2) (*print-length* 2)) (prin1-to-string '(1 (2 (3 4) 5) 6 7)))
  "(1 (2 # ...) ...)")

(deftest printer-cons.escape-off
  (list (princ-to-string '("a" #\b)) (prin1-to-string '("a" #\b)))
  ("(a b)" "(\"a\" #\\b)"))

;;; *print-circle*: the table, not the runaway guard, has to terminate the walk
;;; -- including a cycle that closes after a single element.
(deftest printer-cons.circle-self-cdr
  (let ((*print-readably* nil))
    (write-to-string (let ((a (list 17 nil))) (setf (cdr a) a) a)
                     :circle t :pretty nil :escape nil))
  "#1=(17 . #1#)")

(deftest printer-cons.circle-longer-cycle
  (let ((x (list 1 2 3)) (*print-circle* t))
    (setf (cdr (last x)) x)
    (prin1-to-string x))
  "#1=(1 2 3 . #1#)")

(deftest printer-cons.circle-shared-substructure
  (let* ((tail (list 3 4)) (x (list 1 tail)) (y (list 2 tail)) (*print-circle* t))
    (prin1-to-string (list x y)))
  "((1 #1=(3 4)) (2 #1#))")

;;; With *print-circle* off, printing a circular list is outside the standard;
;;; what has to hold is that it terminates.
(deftest printer-cons.circular-without-print-circle-terminates
  (let ((x (list 1 2 3)) (*print-circle* nil) (*print-length* nil))
    (setf (cdr (last x)) x)
    (let ((s (prin1-to-string x)))
      (list (<= (length s) 40) (search "..." s) (char s 0))))
  (t 11 #\())

(deftest printer-cons.deep-nesting-terminates
  ;; 400 deep, past the printer's depth guard.
  (let ((x 'leaf))
    (dotimes (i 400) (setq x (list x)))
    (let ((s (prin1-to-string x)))
      (and (stringp s) (> (length s) 100))))
  t)

;;; *print-case* conversion returns the input string when nothing changes (the
;;; default: names are stored upcased and *print-case* is :upcase). These pin the
;;; cases where it must still convert.

(deftest printer-case.upcase-default
  (list (prin1-to-string 'foo) (princ-to-string 'foo))
  ("FOO" "FOO"))

(deftest printer-case.downcase
  (let ((*print-case* :downcase))
    (list (prin1-to-string 'foo-bar) (princ-to-string 'foo-bar)))
  ("foo-bar" "foo-bar"))

(deftest printer-case.capitalize
  (let ((*print-case* :capitalize)) (prin1-to-string 'foo-bar))
  "Foo-Bar")

(deftest printer-case.mixed-case-name-escapes
  ;; A name that is not all-upcase needs |...| under the standard readtable,
  ;; whatever *print-case* says.
  (let ((s (intern "MiXeD")))
    (list (prin1-to-string s)
          (let ((*print-case* :downcase)) (prin1-to-string s))
          (princ-to-string s)))
  ("|MiXeD|" "|MiXeD|" "MiXeD"))

(deftest printer-case.keyword-and-uninterned
  (list (prin1-to-string :kw)
        (princ-to-string :kw)
        (let ((*print-gensym* t)) (prin1-to-string (make-symbol "G"))))
  (":KW" "KW" "#:G"))

(deftest printer-case.readtable-case-downcase
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :downcase)
    (prin1-to-string 'foo))
  "|FOO|")

(deftest printer-case.digits-and-punctuation-unchanged
  (list (prin1-to-string 'abcdef12) (prin1-to-string '|123abc|))
  ("ABCDEF12" "|123abc|"))
