;;; A HANDLER-CASE :NO-ERROR clause runs after the handlers are gone.
;;;
;;; CLHS: by the time the :no-error body runs, this HANDLER-CASE's handlers are no
;;; longer active, so an error it signals must reach the NEXT OUTER handler.
;;;
;;; The C# HANDLER-CASE expansion wrapped the protected form as
;;; (multiple-value-call (lambda ll . nbody) EXPR), which ran the :no-error body
;;; INSIDE the HANDLER-BIND — so this form's own error clause caught it:
;;;
;;;   (handler-case (handler-case (values)
;;;                   (error () 'bad)
;;;                   (:no-error () (error "foo")))
;;;     (error () 'good))          ; => BAD, expected GOOD
;;;
;;; COMPILE-HANDLER-CASE got this right, so it only showed through EVAL
;;; (ansi-test HANDLER-CASE.25).
;;;
;;; The fix stashes EXPR's values, leaves the HANDLER-BIND by GO, and applies the
;;; :no-error function from a tagbody segment — the same place the error clause
;;; bodies run, which is by construction outside the handlers.

(defun %hc (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (multiple-value-list (eval form))
      (error (e) (list :error (type-of e))))))

;;; --- the point: an error from :no-error is caught by the OUTER handler-case
;;; (ansi HANDLER-CASE.25)

(defparameter %hc-no-error-signals
  '(handler-case
    (handler-case (values)
      (error () 'bad)
      (:no-error () (error "foo")))
    (error () 'good)))

(deftest interp-handler-case-no-error.escapes-own-handlers-compile
  (%hc :compile %hc-no-error-signals)
  (good))

(deftest interp-handler-case-no-error.escapes-own-handlers-interpret
  (%hc :interpret %hc-no-error-signals)
  (good))

;;; --- over-fix guards ---------------------------------------------------
;;; :no-error runs only when the protected form completes normally, and it
;;; receives that form's values. A fix that merely moves it outside breaks one of
;;; the cases below.

;;; the values reach the lambda list (the shape of ansi HANDLER-CASE.26)
(deftest interp-handler-case-no-error.receives-values-interpret
  (%hc :interpret '(handler-case (values 1 'a 1.0)
                    (error () 'bad)
                    (:no-error (x y z) (list z y x))))
  ((1.0 a 1)))

;;; it runs for zero values too
(deftest interp-handler-case-no-error.zero-values-interpret
  (%hc :interpret '(handler-case (values) (error () 'bad) (:no-error () :ok)))
  (:ok))

;;; :no-error does not run when an error was signalled
(deftest interp-handler-case-no-error.not-run-on-error-interpret
  (%hc :interpret '(handler-case (error "x") (error () 'caught) (:no-error () 'bad)))
  (caught))

;;; values returned by :no-error become the values of the HANDLER-CASE
(deftest interp-handler-case-no-error.propagates-values-interpret
  (%hc :interpret '(handler-case (values 1 2 3)
                    (error () 'bad)
                    (:no-error (&rest r) (values-list r))))
  (1 2 3))

;;; a plain HANDLER-CASE without :no-error still works (the expansion branched)
(deftest interp-handler-case-no-error.plain-normal-interpret
  (%hc :interpret '(handler-case (values 1 2 3) (error () 'bad)))
  (1 2 3))

(deftest interp-handler-case-no-error.plain-error-interpret
  (%hc :interpret '(handler-case (error "x") (error (c) (princ-to-string c))))
  ("x"))
