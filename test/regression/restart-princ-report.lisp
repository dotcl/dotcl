;;; A restart printed with *print-escape* nil prints its report (CLHS
;;; RESTART-CASE :report, WITH-SIMPLE-RESTART): the report function's output,
;;; the :report string, or with neither the restart's name as PRIN1 writes it.
;;; With *print-escape* t it stays #<RESTART ...>.
;;;
;;; WITH-SIMPLE-RESTART used to drop its format control, so PRINC of its
;;; restart gave "#<RESTART FOO>" instead of the text, and the debugger's
;;; restart list showed the bare name.
;;;
;;; Each case runs on both evaluator paths.

(defun %rpr (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (princ-to-string e))))))

(defmacro %rpr-both (name form expected)
  (let ((c (intern (format nil "~a-COMPILE" name)))
        (i (intern (format nil "~a-INTERPRET" name))))
    `(progn
       (deftest ,c (%rpr :compile ',form) ,expected)
       (deftest ,i (%rpr :interpret ',form) ,expected))))

;;; with-simple-restart: format control and arguments are the report.
(%rpr-both restart-princ-report.simple
  (with-simple-restart (foo "Skip item ~a" 1)
    (princ-to-string (find-restart 'foo)))
  "Skip item 1")

;;; The arguments are read when the report runs, in the restart's scope.
(%rpr-both restart-princ-report.simple-args
  (let ((n 7))
    (with-simple-restart (foo "~a of ~a" n "items")
      (princ-to-string (find-restart 'foo))))
  "7 of items")

;;; Anonymous with-simple-restart still reports its text.
(%rpr-both restart-princ-report.simple-anonymous
  (with-simple-restart (nil "Anonymous ~s" :x)
    (princ-to-string (first (compute-restarts))))
  "Anonymous :X")

;;; restart-case :report string.
(%rpr-both restart-princ-report.case-string
  (restart-case (princ-to-string (find-restart 'bar))
    (bar () :report "Use the bar" nil))
  "Use the bar")

;;; restart-case :report lambda.
(%rpr-both restart-princ-report.case-lambda
  (restart-case (princ-to-string (find-restart 'bar))
    (bar () :report (lambda (s) (write-string "From lambda" s)) nil))
  "From lambda")

;;; No report: the name, as PRIN1 prints it.
(%rpr-both restart-princ-report.no-report
  (restart-case (princ-to-string (find-restart 'bar))
    (bar () nil))
  "BAR")

(%rpr-both restart-princ-report.no-report-keyword
  (restart-case (princ-to-string (find-restart :kw))
    (:kw () nil))
  ":KW")

;;; *print-escape* t keeps the unreadable form, report or not.
(%rpr-both restart-princ-report.escape
  (with-simple-restart (foo "Skip item ~a" 1)
    (let ((s (prin1-to-string (find-restart 'foo))))
      (and (>= (length s) 10) (string= "#<RESTART " s :end2 10))))
  t)

;;; FORMAT ~A goes through the same printer.
(%rpr-both restart-princ-report.format-a
  (with-simple-restart (foo "Retry ~d" 3)
    (format nil "[~a]" (find-restart 'foo)))
  "[Retry 3]")
