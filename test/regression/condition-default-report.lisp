;;; A condition of a user class with no :REPORT prints, under PRINC, the default
;;; report SBCL gives -- "Condition PACKAGE::NAME was signalled." (CLHS leaves the text
;;; to the implementation) -- instead of #<NAME>. A :report, or format control, wins.

(define-condition cdr-plain (error) ())
(define-condition cdr-warn (warning) ())
(define-condition cdr-reported (error) () (:report "reported text"))
(define-condition cdr-slots (error) ((x :initarg :x)))

(deftest condition-default-report.plain
  (princ-to-string (make-condition 'cdr-plain))
  "Condition COMMON-LISP-USER::CDR-PLAIN was signalled.")

(deftest condition-default-report.warning-and-slots
  (list (princ-to-string (make-condition 'cdr-warn))
        (princ-to-string (make-condition 'cdr-slots :x 1)))
  ("Condition COMMON-LISP-USER::CDR-WARN was signalled."
   "Condition COMMON-LISP-USER::CDR-SLOTS was signalled."))

(deftest condition-default-report.report-option-wins
  (princ-to-string (make-condition 'cdr-reported))
  "reported text")

(deftest condition-default-report.format-control-wins
  (princ-to-string (make-condition 'simple-error :format-control "x ~A" :format-arguments '(1)))
  "x 1")

(deftest condition-default-report.handler-case-message
  (handler-case (error 'cdr-plain) (error (c) (princ-to-string c)))
  "Condition COMMON-LISP-USER::CDR-PLAIN was signalled.")
