;;; dotnet:define-class must bind the method receiver `self' as a symbol interned
;;; in the CALLER's package, so a bare `self' written in a method body from a user
;;; package resolves at call time. The macro was unhygienic -- it baked `self'
;;; from its own compile-time package (typically CL-USER) -- which a package-aware
;;; variable lookup then exposed as "Unhandled error in foreign callback:
;;; UNBOUND-VARIABLE: SELF" for any caller compiling in a different package.
;;;
;;; The existing net-class tests use the low-level DOTNET:%DEFINE-CLASS with
;;; hand-written lambdas, so they never exercised the macro's `self' injection.

;; Load the macro from source so this test does not depend on a prebuilt fasl.
(load "contrib/dotnet-class/dotnet-class.lisp")

(defpackage :dc57-test (:use :cl))
(in-package :dc57-test)

;; `self' here is read as DC57-TEST::SELF, a different symbol from the macro's
;; own package. With the fix the emitted lambda parameter matches it.
(dotnet:define-class "Dc57Test.SelfCheck" ("System.Object")
  (:methods ("ToString" () :returns "System.String" :override t
              (format nil "self-ok-~A" (dotnet:invoke self "GetHashCode")))))

(cl:in-package :cl-user)

(deftest issue57-define-class-self-resolves-in-user-package
  (let ((o (dotnet:new "Dc57Test.SelfCheck")))
    (and (search "self-ok-" (dotnet:invoke o "ToString")) t))
  t)
