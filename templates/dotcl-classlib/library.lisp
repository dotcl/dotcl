(defpackage :lib (:use :cl))
(in-package :lib)

;;; The library's Lisp side. Everything here is compiled into a fasl at build
;;; time; the C# facade in Library.cs loads that fasl and calls these functions.
;;; Build in Debug and set a breakpoint on a line below to step through Lisp
;;; from a consuming app.

(defun greet (name)
  (format nil "Hello, ~a, from dotcl" name))

;;; An ordinary function taking a sequence. Scalars marshal between .NET and
;;; Lisp automatically; a .NET collection is handed over explicitly with
;;; DotclHost.ToLispList (see Lisp.cs), so this stays plain Lisp.
(defun sum-of-squares (numbers)
  (reduce #'+ numbers :key (lambda (n) (* n n))))
