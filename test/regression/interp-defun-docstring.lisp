;;; An interpreted DEFUN must record its docstring.
;;;
;;; COMPILE-DEFUN emits (setf documentation) for it; the %MINI-EVAL DEFUN case did
;;; not. %MINI-FN-LAMBDA hoists the string out of the implicit block, but hoisting
;;; only MOVES it — as a form in the lambda body it is evaluated and discarded, so
;;; (documentation 'f 'function) answered NIL for anything defined through EVAL.
;;; Same shape as the DEFVAR / DEFPARAMETER / DEFCONSTANT family.
;;;
;;; On an emit-free build every DEFUN takes this path, so no function had
;;; documentation at all there.
;;;
;;; Each mode uses its OWN symbol. Sharing one makes the interpreted case pass
;;; vacuously: the compiled run goes first and registers the value.

(defun %ddoc (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (type-of e))))))

(deftest interp-defun-docstring.records-compile
  (%ddoc :compile '(progn (defun %ddoc-c1 (x) "doc-c" (1+ x))
                    (list (%ddoc-c1 5) (documentation '%ddoc-c1 'function))))
  (6 "doc-c"))

(deftest interp-defun-docstring.records-interpret
  (%ddoc :interpret '(progn (defun %ddoc-i1 (x) "doc-i" (1+ x))
                      (list (%ddoc-i1 5) (documentation '%ddoc-i1 'function))))
  (6 "doc-i"))

;;; --- over-fix guards ---------------------------------------------------

;;; no docstring stays NIL
(deftest interp-defun-docstring.absent-interpret
  (%ddoc :interpret '(progn (defun %ddoc-i2 (x) (1+ x))
                      (documentation '%ddoc-i2 'function)))
  nil)

;;; a lone string IS the body, not a docstring: it must be returned, not recorded
(deftest interp-defun-docstring.string-body-interpret
  (%ddoc :interpret '(progn (defun %ddoc-i3 () "only-body")
                      (list (%ddoc-i3) (documentation '%ddoc-i3 'function))))
  ("only-body" nil))

;;; declarations may follow the docstring
(deftest interp-defun-docstring.with-declarations-interpret
  (%ddoc :interpret '(progn (defun %ddoc-i4 (x y) "doc-d" (declare (ignorable y)) (1+ x))
                      (list (%ddoc-i4 5 9) (documentation '%ddoc-i4 'function))))
  (6 "doc-d"))

;;; (setf name) function names carry documentation too
(deftest interp-defun-docstring.setf-name-interpret
  (%ddoc :interpret '(progn (defun (setf %ddoc-i5) (v o) "doc-s" (setf (car o) v))
                      (documentation '(setf %ddoc-i5) 'function)))
  "doc-s")

;;; redefinition replaces the documentation
(deftest interp-defun-docstring.redefinition-interpret
  (%ddoc :interpret '(progn (defun %ddoc-i6 () "first" :a)
                      (defun %ddoc-i6 () "second" :b)
                      (list (%ddoc-i6) (documentation '%ddoc-i6 'function))))
  (:b "second"))
