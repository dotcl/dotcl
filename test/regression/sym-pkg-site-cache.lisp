;;; A package-qualified symbol reference (LOAD-SYM-PKG: special variables, quoted
;;; symbols) resolves once per site instead of running a string-keyed package
;;; lookup on every execution. The symbol is interned and immortal, so the JIT
;;; path shares one constant-pool slot per symbol -- giving each site its own
;;; slot regrew the pool on every redefinition.

(defpackage :sps-test (:use :cl) (:export #:sps-value #:bump))
(in-package :sps-test)
(defvar sps-value 41)
(defun bump () (incf sps-value))
(in-package :cl-user)

(deftest sym-pkg.special-read-and-write
  (list sps-test::sps-value (sps-test:bump) sps-test::sps-value)
  (41 42 42))

(deftest sym-pkg.let-binding-of-special
  (list (let ((sps-test::sps-value 5)) (list sps-test::sps-value (sps-test:bump) sps-test::sps-value))
        sps-test::sps-value)
  ((5 6 6) 42))

(deftest sym-pkg.quoted-symbol-identity
  (list (eq 'sps-test::sps-value 'sps-test::sps-value)
        (eq 'sps-test::sps-value (intern "SPS-VALUE" :sps-test))
        (eq (symbol-package 'sps-test::sps-value) (find-package :sps-test)))
  (t t t))

(deftest sym-pkg.symbol-in-a-loop-is-the-same-object
  ;; The cached slot must not hand out a different symbol on later executions.
  (let ((seen nil))
    (dotimes (i 3) (push 'sps-test::sps-value seen))
    (list (length seen) (and (eq (first seen) (second seen))
                             (eq (second seen) (third seen)))))
  (3 t))

(deftest sym-pkg.uninterned-symbols-keep-identity
  (let ((g (gensym))) (list (eq g g) (null (symbol-package g))))
  (t t))

(deftest sym-pkg.keyword-and-cl-symbols
  (list (eq :kw :kw) (eq 'car 'cl:car) (eq (symbol-package 'car) (find-package "COMMON-LISP")))
  (t t t))

;; A package created by an earlier top-level form, referenced by a later one:
;; the reader interns the symbol before the referencing form is compiled, which
;; is what makes resolving at assembly time safe.
(defpackage :sps-late (:use :cl))
(defvar sps-late::sps-y 9)
(defun sps-late::g () (incf sps-late::sps-y))

(deftest sym-pkg.package-defined-in-an-earlier-form
  (list sps-late::sps-y (sps-late::g) sps-late::sps-y (eq 'sps-late::sps-y (intern "SPS-Y" :sps-late)))
  (9 10 10 t))

;; The pool guard for this change is the existing
;; DEFUN-REDEFINE-DOES-NOT-LEAK-CONSTANTS in recent-fixes.lisp: its literal-free
;; defun body makes the defun's own LOAD-SYM-PKG registration the only
;; per-definition constant, so it fails the moment those stop sharing a slot
;; (which is exactly what happened when this change first landed without the
;; dedup). A test with a quoted symbol in the body measures the separate
;; per-compilation constant for that literal too, and says nothing clearly.

;;; Reading a special variable skips the dynamic-binding stack until the symbol
;;; has actually been bound (the flag is one-way: set on the first PUSH, never
;;; cleared). Everything below has to behave the same on both sides of that
;;; transition, so each shape is exercised before and after a binding exists.

(defvar *dyn-a* 1)

(deftest dynbind.read-and-setq-without-any-binding
  (list *dyn-a* (setq *dyn-a* 2) *dyn-a*)
  (1 2 2))

(deftest dynbind.let-rebinding
  (list (let ((*dyn-a* 10)) *dyn-a*) *dyn-a*)
  (10 2))

(deftest dynbind.setq-inside-a-binding-does-not-escape
  (list (let ((*dyn-a* 10)) (setq *dyn-a* 11) *dyn-a*) *dyn-a*)
  (11 2))

(deftest dynbind.reads-after-the-flag-is-set
  ;; Same reads as the first test, but now the symbol has been bound before.
  (list *dyn-a* (let ((*dyn-a* 3)) *dyn-a*) *dyn-a*)
  (2 3 2))

(deftest dynbind.nested-bindings
  (let ((*dyn-a* 3)) (list *dyn-a* (let ((*dyn-a* 4)) *dyn-a*) *dyn-a*))
  (3 4 3))

(deftest dynbind.unwinds-on-error
  (list (ignore-errors (let ((*dyn-a* 9)) (error "x"))) *dyn-a*)
  (nil 2))

(defvar *dyn-b* :global)

(deftest dynbind.progv
  (list (progv '(*dyn-b*) '(77) *dyn-b*) *dyn-b*)
  (77 :global))

(deftest dynbind.set-and-symbol-value-under-a-binding
  (list (let ((*dyn-b* 1)) (set '*dyn-b* 42) (list *dyn-b* (symbol-value '*dyn-b*)))
        *dyn-b*)
  ((42 42) :global))

(defvar *dyn-c*)

(deftest dynbind.unbound-bound-unbound
  (list (handler-case *dyn-c* (unbound-variable () :unbound))
        (let ((*dyn-c* 5)) *dyn-c*)
        (handler-case *dyn-c* (unbound-variable () :unbound-again))
        (boundp '*dyn-c*))
  (:unbound 5 :unbound-again nil))

(deftest dynbind.makunbound-of-a-never-bound-symbol
  (let ((s (gensym)))
    (set s 1)
    (list (symbol-value s)
          (progn (makunbound s)
                 (handler-case (symbol-value s) (unbound-variable () :unbound)))))
  (1 :unbound))

(deftest dynbind.a-new-thread-does-not-inherit-bindings
  (let ((*dyn-b* :bound-in-main))
    (let* ((res nil)
           (th (dotcl:make-thread (lambda () (setq res *dyn-b*)))))
      (dotcl:thread-join th)
      (list *dyn-b* res)))
  (:bound-in-main :global))
