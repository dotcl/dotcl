;;; The ansi-test "beyond-ansi" tests: behaviour the standard does not require
;;; (an error for a malformed special form, an invalid declaration or type
;;; specifier). gclload2.lsp does not load them, so the ANSI runs never see them.
;;; The failure count is a measurement, not a gate: SBCL fails some of the same
;;; tests. Usage: make test-ansi-beyond

(load "test/test-ansi-cat.lisp")

(in-package :cl-user)
(load "ansi-test/beyond-ansi/ba-test-package.lsp")
(load "ansi-test/beyond-ansi/ba-aux.lsp")
(dolist (f '("errors-eval-compile" "errors-types-and-class"
             "errors-data-and-control-flow-1" "errors-data-and-control-flow-2"
             "errors-data-and-control-flow-3" "errors-iteration" "errors-loop"))
  (format t "=== ~a ===~%" f)
  (handler-case (load (format nil "ansi-test/beyond-ansi/~a.lsp" f))
    (error (c) (format t "LOAD FAILED ~a: ~a~%" f c))))

(in-package :cl-test)
(let ((*load-pathname* nil) (*load-truename* nil))
  (rt:do-tests))
