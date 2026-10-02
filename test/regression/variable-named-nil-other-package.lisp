;;; A variable or symbol macro named NIL in a package of its own (not
;;; COMMON-LISP:NIL; Coalton's empty list is one), referenced from a package
;;; that inherits it, is that variable. The compiler looks a variable up among
;;; the locals by name as well as by identity, and the name lookup took its "no
;;; match" NIL for the name "NIL", so such a reference read the first local of
;;; another package instead: (list a b nil) in a two-argument function gave
;;; (1 2 1).

(defpackage :vnn-home (:use) (:shadow #:nil) (:export #:nil))
(cl:defvar vnn-home::*box* (cl:list :home-nil))
(cl:define-symbol-macro vnn-home:nil (cl:car vnn-home::*box*))
(defpackage :vnn-user (:use :vnn-home))

(cl:in-package :vnn-user)

(cl:defun vnn-args (a b) (cl:list a b nil))
(cl:defun vnn-let (a) (cl:let ((b 2)) (cl:list a b nil)))

(cl:in-package :cl-user)

(deftest variable-named-nil-other-package.symbol-macro
  (list (vnn-user::vnn-args 1 2) (vnn-user::vnn-let 1))
  ((1 2 :home-nil) (1 2 :home-nil)))

(deftest variable-named-nil-other-package.eval
  (funcall (eval '(lambda (x) (list x vnn-home:nil))) 0)
  (0 :home-nil))
