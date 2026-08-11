;;; (declare (special V)) in a body makes references to V within that body
;;; dynamic (CLHS 3.3.4). WHICH binding that reaches depends on whether the
;;; enclosing form bound V:
;;;
;;;   bound here  — the enclosing form's own binding becomes dynamic
;;;   free here   — the reference must reach an OUTER special binding, and the
;;;                 declaration must NOT rebind anything
;;;
;;; The interpreter took the value out of the lexical alist unconditionally,
;;; which gets the free case exactly backwards: it re-bound V to the *shadowing
;;; lexical* value, so
;;;
;;;   (let ((x :good)) (declare (special x))
;;;     (let ((x :bad)) (locally (declare (special x)) x)))
;;;
;;; answered :BAD. And because the lexical entry was left in place, reads never
;;; consulted the dynamic value at all, so a function called from the body could
;;; not see a body-declared special either (ansi-test DO.14/17/19).
;;;
;;; Each case asserts both evaluator paths by binding dotcl:*evaluator-mode*
;;; around the EVAL, so this runs under the ordinary compiled harness.

(defun %sd (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (princ-to-string e))))))

;;; --- free reference: reaches the OUTER special binding, not the shadowing let

(defparameter %sd-locally
  '(let ((x :good)) (declare (special x))
     (let ((x :bad)) (locally (declare (special x)) x))))

(deftest interp-special-decl.locally-free-compile
  (%sd :compile %sd-locally)
  :good)

(deftest interp-special-decl.locally-free-interpret
  (%sd :interpret %sd-locally)
  :good)

;;; ansi-test DO.19 shape — the declaration is in a DO body that binds nothing.
(defparameter %sd-do-free
  '(let ((x :good)) (declare (special x))
     (let ((x :bad)) (do nil (t x) (declare (special x))))))

(deftest interp-special-decl.do-free-compile
  (%sd :compile %sd-do-free)
  :good)

(deftest interp-special-decl.do-free-interpret
  (%sd :interpret %sd-do-free)
  :good)

;;; --- bound reference: the enclosing form's own binding becomes dynamic, so a
;;; function called from the body sees it (ansi-test DO.14).

(defparameter %sd-do-bound
  '(let ((x 0))
     (flet ((%f () (locally (declare (special i)) (incf x i))))
       (do ((i 0 (1+ i))) ((>= i 10) x) (declare (special i)) (%f)))))

(deftest interp-special-decl.do-bound-compile
  (%sd :compile %sd-do-bound)
  45)

(deftest interp-special-decl.do-bound-interpret
  (%sd :interpret %sd-do-bound)
  45)

;;; A LET binding declared special in its own body is dynamic for callees too.
(defparameter %sd-let-bound
  '(let ((r nil))
     (flet ((peek () (locally (declare (special v)) v)))
       (let ((v :dynamic)) (declare (special v)) (setq r (peek))))
     r))

(deftest interp-special-decl.let-bound-visible-to-callee-compile
  (%sd :compile %sd-let-bound)
  :dynamic)

(deftest interp-special-decl.let-bound-visible-to-callee-interpret
  (%sd :interpret %sd-let-bound)
  :dynamic)

;;; A lambda parameter declared special in the body is likewise dynamic.
(defparameter %sd-param-bound
  '(flet ((peek () (locally (declare (special p)) p)))
     (funcall (lambda (p) (declare (special p)) (peek)) :from-param)))

(deftest interp-special-decl.param-bound-interpret
  (%sd :interpret %sd-param-bound)
  :from-param)

;;; --- the plain cases must be unchanged (guard against over-shadowing)

(deftest interp-special-decl.let-special-reads-own-value-interpret
  (%sd :interpret '(let ((x 5)) (declare (special x)) x))
  5)

(deftest interp-special-decl.undeclared-lexical-still-lexical-interpret
  (%sd :interpret '(let ((x :outer))
                     (declare (special x))
                     (let ((x :inner)) x)))
  :inner)

(deftest interp-special-decl.let*-bound-special-interpret
  (%sd :interpret '(let* ((a 1) (x (+ a 1))) (declare (special x)) x))
  2)
