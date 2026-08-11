;;; RESTART-CASE / RESTART-BIND options (:report, :test, :interactive) must reach
;;; the established restart on BOTH evaluator paths.
;;;
;;; RESTART-CASE macroexpands into RESTART-BIND, which macroexpands into
;;; %PUSH-RESTART-CLUSTER — and that expansion used to DROP the options, on the
;;; premise that "restart options are kept by the compile-form handler; this
;;; expansion is for macroexpand-1 / walkers". That premise does not hold for the
;;; tree-walk interpreter, which takes the macro path as its real evaluation
;;; route (and is the only evaluator on emit-free builds). The result was a
;;; restart that could not report itself and whose :TEST never ran, so
;;; FIND-RESTART handed back a restart its own test excludes.
;;; (ansi-test RESTART-CASE.19 / .20 / .21.)
;;;
;;; Each case asserts both paths by binding dotcl:*evaluator-mode* around the
;;; EVAL, so this runs under the ordinary compiled harness.

(defun %ro (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (princ-to-string e))))))

;;; --- :test excludes a restart from FIND-RESTART / INVOKE-RESTART (ansi RESTART-CASE.19)

(defparameter %ro-test-form
  '(restart-case (invoke-restart 'foo)
     (foo () :test (lambda (c) (declare (ignore c)) nil) 'bad)
     (foo () 'good)))

(deftest interp-restart-options.test-compile
  (%ro :compile %ro-test-form)
  good)

(deftest interp-restart-options.test-interpret
  (%ro :interpret %ro-test-form)
  good)

;;; --- :report as a string becomes the restart's report (ansi RESTART-CASE.20)

(defparameter %ro-report-string
  '(with-output-to-string (s)
     (restart-case (let ((r (find-restart 'foo)) (*print-escape* nil))
                     (format s "~A" r))
       (foo () :report "A report"))))

(deftest interp-restart-options.report-string-compile
  (%ro :compile %ro-report-string)
  "A report")

(deftest interp-restart-options.report-string-interpret
  (%ro :interpret %ro-report-string)
  "A report")

;;; --- :report as a function, closing over the establishing scope
;;; (ansi RESTART-CASE.21 — the report function is an FLET)

(defparameter %ro-report-fn
  '(with-output-to-string (s)
     (flet ((%f (s2) (format s2 "A report")))
       (restart-case (let ((r (find-restart 'foo)) (*print-escape* nil))
                       (format s "~A" r))
         (foo () :report %f)))))

(deftest interp-restart-options.report-function-compile
  (%ro :compile %ro-report-fn)
  "A report")

(deftest interp-restart-options.report-function-interpret
  (%ro :interpret %ro-report-fn)
  "A report")

;;; --- RESTART-BIND directly, with :report-function

(defparameter %ro-bind-report
  '(with-output-to-string (s)
     (restart-bind ((bar (lambda () :ran)
                         :report-function (lambda (s2) (format s2 "bar report"))))
       (let ((r (find-restart 'bar)) (*print-escape* nil))
         (format s "~A" r)))))

(deftest interp-restart-options.restart-bind-report-compile
  (%ro :compile %ro-bind-report)
  "bar report")

(deftest interp-restart-options.restart-bind-report-interpret
  (%ro :interpret %ro-bind-report)
  "bar report")

;;; --- a :test that ACCEPTS must not exclude the restart (guard against a fix
;;; that simply drops every restart carrying a test)

(defparameter %ro-test-true
  '(restart-case (invoke-restart 'foo)
     (foo () :test (lambda (c) (declare (ignore c)) t) 'first)
     (foo () 'second)))

(deftest interp-restart-options.test-accepting-interpret
  (%ro :interpret %ro-test-true)
  first)

;;; --- a restart with no options still works, and reports the default way

(deftest interp-restart-options.no-options-interpret
  (%ro :interpret '(restart-case (invoke-restart 'plain) (plain () :ok)))
  :ok)
