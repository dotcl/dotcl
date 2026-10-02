;;; A package that swaps one of its symbols for another package's symbol of
;;; the same name (UNEXPORT, SHADOWING-IMPORT, EXPORT) after a fasl naming
;;; that symbol was loaded. hu.dwim.stefil does this with -BODY-: its
;;; DEFIXTURE template binds -BODY- with FLET and calls it, and the swap
;;; happens between loading the macro and expanding it.
;;;
;;; The fasl resolves package-qualified symbols by name on first use through a
;;; process-wide (name, package) cache. The cache kept the symbol the package
;;; no longer had, while other references of the same template were resolved
;;; afresh, so the FLET binding and the call named different symbols and the
;;; call hit "Undefined function". The cache is now invalidated whenever a
;;; package stops mapping a name to the symbol it mapped it to before.
;;;
;;; SHADOWING-IMPORT also uninterns the symbol it replaces (CLHS), which
;;; clears that symbol's home package when it was this one.

(defpackage :fsasi-a (:use :cl) (:export #:body #:with-b #:*aliases*))
(defpackage :fsasi-b (:use :cl) (:export #:body))

(deftest-compiled-only fasl-symbol-after-shadowing-import.flet-template
  (let ((src "fsasi-tmp.lisp")
        (fasl "fsasi-tmp.fasl"))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (write-line "(in-package :fsasi-a)" s)
             (write-line "(defvar *aliases* '())" s)
             (write-line "(defmacro with-b (&body forms)
  `(flet ((body () 42))
     (flet (,@(mapcar (lambda (a) `(,a () (body))) *aliases*))
       ,@forms)))" s))
           (compile-file src :output-file fasl)
           (load fasl)
           (let ((old (find-symbol "BODY" :fsasi-a)))
             (unexport old :fsasi-a)
             (shadowing-import (find-symbol "BODY" :fsasi-b) :fsasi-a)
             (export (find-symbol "BODY" :fsasi-b) :fsasi-a)
             (push (find-symbol "BODY" :fsasi-b)
                   (symbol-value (find-symbol "*ALIASES*" :fsasi-a)))
             (list (handler-case
                       (funcall (compile nil `(lambda ()
                                                (,(find-symbol "WITH-B" :fsasi-a)
                                                 (,(find-symbol "BODY" :fsasi-b))))))
                     (error (e) (list :error (princ-to-string e))))
                   (symbol-package old))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  (42 nil))

(deftest fasl-symbol-after-shadowing-import.old-symbol-loses-home
  (let* ((p (make-package "FSASI-C" :use nil))
         (old (intern "X" p)))
    (unwind-protect
         (progn
           (shadowing-import (make-symbol "X") p)
           (symbol-package old))
      (delete-package p)))
  nil)
