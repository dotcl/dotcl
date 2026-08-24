(defpackage :app (:use :cl) (:export #:app-main))
(in-package :app)

;;; Entry point called from Program.cs. Build in Debug and press F5, then set a
;;; breakpoint on one of the lines below to step through Lisp in the debugger and
;;; inspect locals (e.g. GREETING) in the Locals window.
(defun app-main ()
  (let ((greeting "Hello from dotcl"))
    (format t "~a~%" greeting)
    greeting))
