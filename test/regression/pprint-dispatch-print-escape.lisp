;;; A pprint dispatch function sees *PRINT-ESCAPE* as the printing function
;;; set it: NIL under PRINC, PRINC-TO-STRING and ~A, T under PRIN1 and ~S.
;;;
;;; The printer carries the escape mode as a parameter and used to call a
;;; dispatch function without binding the variable, so a function that asks
;;; *PRINT-ESCAPE* (swank's backtrace table prints strings that way) took
;;; the escaping branch under PRINC and ~A as well: the debugger showed
;;; ("FOO" 4) for a frame printed with "(~A~{ ~S~})".
;;;
;;; Results are collected first and printed afterwards: printing them while
;;; the table is still bound would send the result strings through it too.

(defun ppe-table ()
  (let ((table (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'string
                         (lambda (stream object)
                           (declare (ignore object))
                           (write-string (if *print-escape* "<esc>" "<noesc>")
                                         stream))
                         0 table)
    table))

(defmacro ppe-with-table (&body body)
  `(let ((*print-pprint-dispatch* (ppe-table))
         (*print-pretty* t)
         (*print-escape* t))
     ,@body))

(deftest ppe-princ-to-string
  (ppe-with-table (princ-to-string "x"))
  "<noesc>")

(deftest ppe-prin1-to-string
  (ppe-with-table (let ((*print-escape* nil)) (prin1-to-string "x")))
  "<esc>")

(deftest ppe-format-a-and-s
  (ppe-with-table (format nil "~A ~S" "x" "x"))
  "<noesc> <esc>")

;;; Inside a list the elements are printed by the pretty printer's own walk.
(deftest ppe-format-a-list
  (ppe-with-table (format nil "~A ~S" (list "x") (list "x")))
  "(<noesc>) (<esc>)")

(deftest ppe-princ-to-stream
  (ppe-with-table (with-output-to-string (s) (princ "x" s)))
  "<noesc>")

;;; The binding ends with the dispatch function.
(deftest ppe-binding-restored
  (ppe-with-table
    (princ-to-string "x")
    *print-escape*)
  t)
