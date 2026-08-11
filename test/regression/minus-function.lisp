;;; #'- with zero arguments, and the unary case (the DEFUN in cil-stdlib.lisp).
;;;
;;; The definition read:
;;;   (defun - (&rest args)
;;;     (if (null args) 0
;;;         (if (null (cdr args)) (- 0 (car args)) ...)))
;;;
;;; Both halves contradict CLHS:
;;;   * (-) is a program-error, not 0 (ansi MINUS.ERROR.1)
;;;   * the unary case is a sign flip, not "subtract from 0". 0.0 - 0.0 is +0.0, so
;;;     (- 0.0) produced 0.0 and stopped being EQL to -0.0 (ansi READ-FLOAT.1).
;;;     compile-sub uses Runtime.Negate for exactly this reason; only this DEFUN
;;;     was left behind.
;;;
;;; A literal (- 0.0) is inlined by the compiler and is correct, so the bug showed
;;; ONLY through FUNCALL / APPLY / REDUCE, where the DEFUN is what runs. It is
;;; therefore not an interpreter-only problem — the compiled cases below also fail
;;; before the fix.

(defun %mf (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (type-of e))))))

(defun %mf-err (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (progn (eval form) :no-error)
      (program-error () :program-error)
      (error (e) (list :other (type-of e))))))

;;; --- (-) is a program-error

(deftest minus-function.no-args-compile
  (%mf-err :compile '(-))
  :program-error)

(deftest minus-function.no-args-interpret
  (%mf-err :interpret '(-))
  :program-error)

(deftest minus-function.apply-no-args-interpret
  (%mf-err :interpret '(apply #'- '()))
  :program-error)

;;; --- the unary case is a sign flip: signed zero survives through #'- too

(deftest minus-function.negate-zero-single-compile
  (%mf :compile '(eql -0.0 (funcall #'- 0.0)))
  t)

(deftest minus-function.negate-zero-single-interpret
  (%mf :interpret '(eql -0.0 (funcall #'- 0.0)))
  t)

(deftest minus-function.negate-zero-double-interpret
  (%mf :interpret '(eql -0.0d0 (funcall #'- 0.0d0)))
  t)

(deftest minus-function.negate-zero-inline-interpret
  (%mf :interpret '(eql -0.0 (- 0.0)))
  t)

;;; --- ordinary subtraction still works. This also covers "does not fall into
;;;     self-recursion": an infinite loop here would simply never return.

(deftest minus-function.ordinary-still-works
  (%mf :interpret '(list (- 5) (- 3 1) (- 1 2 3)
                    (funcall #'- 5) (funcall #'- 1 2 3)
                    (reduce #'- '(10 1 2)) (1- 5)))
  (-5 2 -4 -5 -4 7 4))
