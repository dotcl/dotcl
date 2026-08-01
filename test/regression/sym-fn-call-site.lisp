;;; Regression tests for the PER-CALL-SITE function-symbol cache.
;;;
;;; A call to a global function used to resolve the callee's name through
;;; Startup.SymFn on EVERY call. SymFn memoizes, but its key is
;;; name + "\0" + package — a string allocated and hashed per call. Each emitted
;;; call site now carries its own cache cell, filled on first execution.
;;;
;;; Contract: resolution still happens at RUNTIME and a MISS IS NOT PINNED.
;;; Concretely — a call site compiled before its callee exists must pick the
;;; callee up once it is defined, and redefinition must always be observed.

;;; ---- a miss is not pinned ----

;; Compiled while %SFC-LATE does not exist yet.
(defun %sfc-early-caller () (%sfc-late 3))

(deftest sfc-undefined-before-definition
  (handler-case (%sfc-early-caller)
    (undefined-function () :undefined)
    (error () :some-other-error))
  :undefined)

;; The condition names the symbol the source actually named.
(deftest sfc-undefined-names-the-symbol
  (handler-case (%sfc-early-caller)
    (undefined-function (c) (symbol-name (cell-error-name c))))
  "%SFC-LATE")

;; Same call site, now that the callee exists: the failed resolution must not
;; have been cached.
(defun %sfc-late (n) (* n 10))

(deftest sfc-resolves-after-definition
  (%sfc-early-caller)
  30)

;; And repeatedly (the cell is filled now).
(deftest sfc-resolves-repeatedly
  (list (%sfc-early-caller) (%sfc-early-caller) (%sfc-early-caller))
  (30 30 30))

;;; ---- redefinition is always observed ----

(defun %sfc-target () :first)
(defun %sfc-caller () (%sfc-target))

(deftest sfc-redef-first
  (%sfc-caller)
  :first)

(defun %sfc-target () :second)

(deftest sfc-redef-second
  (%sfc-caller)
  :second)

;; Redefinition with a different arity, through the same call site.
(defun %sfc-arity (a) a)
(defun %sfc-arity-caller (a) (%sfc-arity a))

(deftest sfc-arity-first
  (%sfc-arity-caller 5)
  5)

(defun %sfc-arity (a) (list a a))

(deftest sfc-arity-second
  (%sfc-arity-caller 5)
  (5 5))

;;; ---- fmakunbound goes back to undefined, then rebinds ----

(defun %sfc-unbindable () :alive)
(defun %sfc-unbindable-caller () (%sfc-unbindable))

(deftest sfc-unbind-before
  (%sfc-unbindable-caller)
  :alive)

(deftest sfc-unbind-signals
  (progn
    (fmakunbound '%sfc-unbindable)
    (handler-case (%sfc-unbindable-caller)
      (undefined-function () :undefined)))
  :undefined)

(deftest sfc-unbind-rebinds
  (progn
    (defun %sfc-unbindable () :again)
    (%sfc-unbindable-caller))
  :again)

;;; ---- setf functions go through the same site ----

(defvar *sfc-place* nil)

(defun (setf %sfc-slot) (v) (setq *sfc-place* v))
(defun %sfc-setf-caller (v) (setf (%sfc-slot) v))

(deftest sfc-setf-first
  (progn (%sfc-setf-caller 1) *sfc-place*)
  1)

(defun (setf %sfc-slot) (v) (setq *sfc-place* (list :wrapped v)))

(deftest sfc-setf-redefined
  (progn (%sfc-setf-caller 2) *sfc-place*)
  (:wrapped 2))

;;; ---- same name in two packages resolves per call site ----

(defpackage :sfc-pkg-a (:use :cl) (:export #:who))
(defpackage :sfc-pkg-b (:use :cl) (:export #:who))

(in-package :sfc-pkg-a)
(defun who () :a)
(defun ask () (who))

(in-package :sfc-pkg-b)
(defun who () :b)
(defun ask () (who))

(in-package :cl-user)

(deftest sfc-per-package-resolution
  (list (sfc-pkg-a::ask) (sfc-pkg-b::ask))
  (:a :b))

;; Redefining one package's function does not disturb the other's call site.
(deftest sfc-per-package-redefinition
  (progn
    (setf (symbol-function 'sfc-pkg-a::who) (lambda () :a2))
    (list (sfc-pkg-a::ask) (sfc-pkg-b::ask)))
  (:a2 :b))

;;; ---- a call site inside a closure, and one reached only via eval ----

(defun %sfc-closure-caller ()
  (funcall (lambda () (%sfc-target))))

(deftest sfc-closure-site
  (%sfc-closure-caller)
  :second)

(deftest sfc-eval-site
  (eval '(%sfc-target))
  :second)

;; A site compiled by EVAL after the callee was redefined still sees the current
;; definition, and keeps seeing it after another redefinition.
(deftest sfc-eval-site-after-redef
  (let ((f (eval '(lambda () (%sfc-target)))))
    (let ((before (funcall f)))
      (defun %sfc-target () :third)
      (list before (funcall f))))
  (:second :third))
