;;; A MACROLET expander's &ENVIRONMENT must let MACROEXPAND see an enclosing
;;; SYMBOL-MACROLET (CLHS 3.1.1.1):
;;;
;;;   (symbol-macrolet ((a b))
;;;     (macrolet ((foo (x &environment env)
;;;                  (let ((y (macroexpand x env))) (if (eq y 'a) 1 2))))
;;;       (foo a)))                                          ; => 2, not 1
;;;
;;; The &ENVIRONMENT object is built by %MACROLET-EXPANDER-FORM as
;;;   (cons *macros* <hash built from *symbol-macros*>)
;;; and that builder is deliberately SHARED by the compiler and the interpreter
;;; so the two cannot drift. The drift was in who fills in what it reads: the
;;; interpreter pushed symbol macros onto its own alist only and never bound
;;; *SYMBOL-MACROS*, so the expander got an environment with no symbol macros in
;;; it and MACROEXPAND returned the symbol unchanged.
;;; (ansi-test MACROLET.13 / .14 / .15.)
;;;
;;; Both evaluator paths are asserted by binding dotcl:*evaluator-mode* around
;;; the EVAL, so this runs under the ordinary compiled harness.

(defun %mle (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (princ-to-string e))))))

;;; ansi-test MACROLET.13 (MACROEXPAND) and .14 (MACROEXPAND-1).
(defparameter %mle-expand
  '(symbol-macrolet ((a b))
     (macrolet ((foo (x &environment env)
                  (let ((y (macroexpand x env))) (if (eq y 'a) 1 2))))
       (foo a))))

(deftest interp-macrolet-environment.macroexpand-compile
  (%mle :compile %mle-expand)
  2)

(deftest interp-macrolet-environment.macroexpand-interpret
  (%mle :interpret %mle-expand)
  2)

(defparameter %mle-expand-1
  '(symbol-macrolet ((a b))
     (macrolet ((foo (x &environment env)
                  (let ((y (macroexpand-1 x env))) (if (eq y 'a) 1 2))))
       (foo a))))

(deftest interp-macrolet-environment.macroexpand-1-compile
  (%mle :compile %mle-expand-1)
  2)

(deftest interp-macrolet-environment.macroexpand-1-interpret
  (%mle :interpret %mle-expand-1)
  2)

;;; The expansion actually reached must be the symbol macro's, not merely
;;; "something other than A" — a fix that returns garbage would also pass above.
(defparameter %mle-value
  '(symbol-macrolet ((a b))
     (macrolet ((foo (x &environment env) `',(macroexpand x env)))
       (foo a))))

(deftest interp-macrolet-environment.expansion-value-compile
  (%mle :compile %mle-value)
  b)

(deftest interp-macrolet-environment.expansion-value-interpret
  (%mle :interpret %mle-value)
  b)

;;; A symbol with no symbol-macro binding must come back unchanged.
(deftest interp-macrolet-environment.unbound-symbol-unchanged-interpret
  (%mle :interpret '(symbol-macrolet ((a b))
                      (macrolet ((foo (x &environment env) `',(macroexpand x env)))
                        (foo zzz-not-a-symbol-macro))))
  zzz-not-a-symbol-macro)

;;; The symbol-macrolet must still work as a plain variable reference, and must
;;; not leak past its own body.
(deftest interp-macrolet-environment.still-expands-as-variable-interpret
  (%mle :interpret '(let ((b 7)) (symbol-macrolet ((a b)) a)))
  7)

(deftest interp-macrolet-environment.does-not-leak-interpret
  (%mle :interpret '(progn (symbol-macrolet ((a b)) :ignored)
                           (macrolet ((foo (x &environment env) `',(macroexpand x env)))
                             (foo a))))
  a)
