;;; A pprint dispatch function's output goes through the table too.
;;;
;;; A dispatch function prints the parts of its object with WRITE or PRINC, and
;;; those parts have to reach the table: a table with an entry for CONS and one
;;; for STRING must apply the STRING entry to the strings inside the cons.
;;; Dispatch used to be switched off for everything printed inside a dispatch
;;; function, so such a table printed its own top level and then fell back to
;;; the ordinary printer underneath.
;;;
;;; Results are collected first and printed afterwards on purpose: printing them
;;; while the table is still bound would send the result strings through it too.

(defun pdn-table ()
  (let ((table (copy-pprint-dispatch)))
    (set-pprint-dispatch 'cons
                         (lambda (stream object)
                           (write-string "[" stream)
                           (princ (car object) stream)
                           (write-string "|" stream)
                           (princ (cdr object) stream)
                           (write-string "]" stream))
                         1 table)
    (set-pprint-dispatch 'string
                         (lambda (stream object)
                           (declare (ignore object))
                           (write-string "<STR>" stream))
                         2 table)
    table))

(defun pdn-print (object)
  (let ((*print-pprint-dispatch* (pdn-table))
        (*print-pretty* t))
    (prin1-to-string object)))

(deftest pdn-entry-applies-to-the-object-itself
  (pdn-print "abc")
  "<STR>")

;;; The strings inside the cons reach the string entry.
(deftest pdn-entry-applies-inside-another-entry
  (pdn-print (cons "a" "b"))
  "[<STR>|<STR>]")

;;; And the cons entry applies to a nested cons.
(deftest pdn-entry-recurses-into-nested-conses
  (pdn-print (cons "a" (cons "b" nil)))
  "[<STR>|[<STR>|NIL]]")

;;; A dispatch function that prints its own object does not re-enter itself:
;;; that guard is what the nesting fix had to preserve.
(deftest pdn-same-object-does-not-re-enter
  (let ((table (copy-pprint-dispatch)))
    (set-pprint-dispatch 'string
                         (lambda (stream object)
                           (write-string "<" stream)
                           (prin1 object stream)
                           (write-string ">" stream))
                         2 table)
    (let ((*print-pprint-dispatch* table)
          (*print-pretty* t))
      (prin1-to-string "x")))
  "<\"x\">")

;;; Objects with no entry print as usual.
(deftest pdn-unmatched-object-prints-normally
  (pdn-print 42)
  "42")

;;; The table is consulted only while *print-pretty* is true.
(deftest pdn-not-pretty-means-no-dispatch
  (let ((*print-pprint-dispatch* (pdn-table))
        (*print-pretty* nil))
    (prin1-to-string (cons "a" "b")))
  "(\"a\" . \"b\")")
