;;; The &ENVIRONMENT a MACROLET expander receives is the one at the macro call,
;;; so it holds the SYMBOL-MACROLET bindings made between the MACROLET form and
;;; the call. (ironclad's sha3 unrolls a loop with SYMBOL-MACROLET and indexes a
;;; MACROLET place with (EVAL (MACROEXPAND index env)).)

(deftest macrolet-expander-env-sees-inner-symbol-macrolet
  (funcall (eval '(lambda ()
                   (macrolet ((m (x &environment e) `',(macroexpand x e)))
                     (symbol-macrolet ((y 0)) (m y))))))
  0)

(defvar *mlcs-vec* (vector 1 2 3))

;; COMPILE, not EVAL: the tree-walk evaluator does not expand a MACROLET place
;; under SETF at all (a separate gap). COMPILE needs an emitter.
(deftest-emitting-only macrolet-expander-env-setf-place
  (funcall (compile nil '(lambda ()
                   (let ((v (copy-seq *mlcs-vec*)))
                     (macrolet ((at (i &environment e)
                                  `(aref v ,(eval (macroexpand i e)))))
                       (symbol-macrolet ((k 1))
                         (setf (at k) 9))
                       (symbol-macrolet ((k 2))
                         (list (at k) (coerce v 'list))))))))
  (3 (1 9 3)))

;; COMPILE needs an emitter.
(deftest-emitting-only macrolet-expander-env-compiled
  (funcall (compile nil '(lambda ()
                          (macrolet ((m (x &environment e) `',(macroexpand x e)))
                            (let ((q 1))
                              (declare (ignorable q))
                              (symbol-macrolet ((y :inner)) (m y)))))))
  :inner)
