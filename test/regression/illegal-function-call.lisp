;;; A compound form whose operator is neither a symbol nor a cons — (0 1 2) —
;;; is a program error, not a compiler crash.
;;;
;;; Bug: compile-expr fell through the cons-op cases into the string=-based
;;; dispatch chain, which calls SYMBOL-NAME on the operator, so such a form
;;; aborted compilation with an opaque "SYMBOL-NAME: 0 is not of type SYMBOL"
;;; type error. compile-file then gave up on the whole file. It is easy to hit
;;; by accident: compiling a file that uses a macro whose definition was not
;;; loaded turns the macro's unquoted literal arguments into call forms.
;;;
;;; Fix: emit a deferred PROGRAM-ERROR naming the form (the
;;; compile-static-program-error idiom), so compilation of the rest of the file
;;; proceeds and running the form reports what is wrong — as SBCL does.

(defun ifc-error-of (form)
  "Compile and run FORM, returning (type-of condition) or :no-error."
  (handler-case (progn (funcall (compile nil `(lambda () ,form))) :no-error)
    (error (e) (type-of e))))

(defun ifc-message-of (form)
  (handler-case (progn (funcall (compile nil `(lambda () ,form))) "")
    (error (e) (princ-to-string e))))

(deftest illegal-function-call.number-operator
  (ifc-error-of '(0 1 2))
  program-error)

(deftest illegal-function-call.string-operator
  (ifc-error-of '("nope" 1))
  program-error)

(deftest illegal-function-call.message-names-the-form
  (let ((msg (ifc-message-of '(0 1 2))))
    (and (search "Illegal function call" msg)
         (search "(0 1 2)" msg)
         t))
  t)

;;; The error is deferred to run time: compiling the form must not signal, and
;;; forms after it in the same compilation unit still compile.
(deftest illegal-function-call.compiles-without-signaling
  (progn (compile nil '(lambda () (0 1 2))) :compiled)
  :compiled)

(deftest illegal-function-call.unreached-branch-is-harmless
  (funcall (compile nil '(lambda (flag) (if flag (0 1 2) :fine))) nil)
  :fine)

;;; Legitimate non-symbol operators must keep working.
(deftest illegal-function-call.lambda-operator-still-works
  ((lambda (x) (* x 2)) 21)
  42)

(deftest illegal-function-call.quoted-designator-still-works
  (funcall (compile nil '(lambda () ((quote list) 1 2))))
  (1 2))
