;;; A user-defined function named UNQUOTE (or any name the implementation
;;; happens to intern in the CL package for its own use) must be callable.
;;;
;;; Bug: the backquote markers UNQUOTE / QUASIQUOTE / UNQUOTE-SPLICING are
;;; interned in the CL package by the reader, and Startup.SymFn (the resolver
;;; for unqualified compiled call sites) returned any CL symbol it found —
;;; even one with no function — so a call to crypto::unquote in ironclad
;;; resolved to the unbound CL::UNQUOTE and signaled UNDEFINED-FUNCTION
;;; despite (fboundp 'crypto::unquote) => T.
;;;
;;; Fix: a CL symbol wins in SymFn only when it is actually callable;
;;; otherwise resolution falls through to the caller's own package.

(defpackage #:uq-shadow-test (:use #:cl) (:export #:unquote #:uq-caller))

;;; The call site must be compiled with *package* = UQ-SHADOW-TEST so it is an
;;; unqualified call (the :load-sym-fn path), which is what SymFn resolves.
(let ((*package* (find-package "UQ-SHADOW-TEST")))
  (eval (read-from-string
         "(progn (defun unquote (x) (list :myuq x))
                 (defun uq-caller (x) (unquote x)))")))

(deftest unquote-user-function.unqualified-call-not-hijacked
  (uq-shadow-test:uq-caller 5)
  (:myuq 5))

(deftest unquote-user-function.fboundp
  (fboundp 'uq-shadow-test:unquote)
  t)

;;; Backquote itself must keep working (the markers stay identity-based).
(deftest unquote-user-function.backquote-still-works
  (let ((x 42) (y '(1 2)))
    `(a ,x ,@y))
  (a 42 1 2))
