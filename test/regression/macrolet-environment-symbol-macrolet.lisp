;;; A MACROLET expander's &ENVIRONMENT holds the SYMBOL-MACROLET bindings around
;;; the macro call, also when the call is inside a SYMBOL-MACROLET that the
;;; MACROLET form itself is outside of.
;;;
;;; The compiler makes MACROLET expanders with its tree-walk evaluator, whose
;;; closures rebind the symbol-macro scope to the one where they were made (the
;;; MACROLET form). An expander that built its &ENVIRONMENT from that scope saw
;;; none of the bindings at the call, so (MACROEXPAND X ENV) gave X back. ironclad's
;;; Keccak code (DOTIMES-UNROLLED binds X with SYMBOL-MACROLET, a local macro
;;; evaluates (MACROEXPAND X ENV)) then failed to compile with "Unbound variable: X".
;;;
;;; About the compiler's expanders, so not on the emit-free build (which has no
;;; compiler; its evaluator makes MACROLET expanders its own way).

(defmacro mesm-unroll ((var n) &body body)
  `(progn ,@(loop for i below n
                  collect `(symbol-macrolet ((,var ,i)) ,@body))))

(defun mesm-f (v)
  (macrolet ((ref (x &environment env)
               `(aref v ,(eval (macroexpand x env)))))
    (mesm-unroll (x 2) (setf (ref x) (* 10 (ref x))))
    v))

(deftest-emitting-only macrolet-environment-symbol-macrolet.setf-place
  (coerce (mesm-f (vector 1 2)) 'list)
  (10 20))

(deftest-emitting-only macrolet-environment-symbol-macrolet.expansion
  (funcall (compile nil '(lambda ()
                          (macrolet ((m (s &environment env)
                                       `',(multiple-value-list (macroexpand s env))))
                            (symbol-macrolet ((x 0)) (m x))))))
  (0 t))
