;;; CAR / CDR type errors: the offending value, and nothing about the runtime.
;;;
;;; CAR's report used to carry eight .NET frames after the message --
;;;
;;;   CAR: not a list (got Fixnum: 7)
;;;     at: .toplevel -> CilAssembler.AssembleAndRunSingle -> ... -> Thread.StartCallback
;;;
;;; -- a diagnostic added while bringing up the SBCL cross-compile in April and
;;; never taken out. It named runtime internals rather than the caller's code,
;;; captured a stack trace on every occurrence, and rode along in every PRINC of
;;; the condition. CDR meanwhile said only "CDR: not a list", without the value.
;;;
;;; Both now report the value the same way NTH and LAST do, and the condition
;;; carries it as the type-error datum for anything that wants more.

(defun %ccte-report (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (error (e) (princ-to-string e))))

(deftest car-cdr-type-error.reports-the-value
  (list (%ccte-report (lambda () (car 7)))
        (%ccte-report (lambda () (cdr 7)))
        (%ccte-report (lambda () (cdr "ab"))))
  ("CAR: not a list: 7" "CDR: not a list: 7" "CDR: not a list: \"ab\""))

;;; No runtime frames in the report: what "at:" used to introduce, and the
;;; internal type names that went with it.
(deftest car-cdr-type-error.no-runtime-internals
  (let ((r (%ccte-report (lambda () (car 7)))))
    (list (and (search "at:" r) t)
          (and (search "CilAssembler" r) t)
          (and (search "Fixnum" r) t)))
  (nil nil nil))

(deftest car-cdr-type-error.datum-and-expected-type
  (handler-case (car "abc")
    (type-error (e) (list (type-error-datum e) (type-error-expected-type e))))
  ("abc" list))

;;; NIL still walks, and a cons still works.
(deftest car-cdr-type-error.valid-uses-unchanged
  (list (car nil) (cdr nil) (car (cons 1 2)) (cdr (cons 1 2)))
  (nil nil 1 2))
