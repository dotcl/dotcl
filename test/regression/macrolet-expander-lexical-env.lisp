;;; A MACROLET expander is defined in the lexical environment of the MACROLET
;;; form (CLHS MACROLET): enclosing MACROLET / SYMBOL-MACROLET bindings are
;;; visible in its body, also inside a closure the expander returns and that
;;; runs after the expansion is over. And a lexical variable shadows a symbol
;;; macro of the same name in the &ENVIRONMENT an expander is handed
;;; (MACROEXPAND of that name does not expand it).

(define-symbol-macro mlenv-global "global")

(defmacro mlenv-expand-global (&environment e)
  `',(multiple-value-list (macroexpand-1 'mlenv-global e)))

(deftest macrolet-expander-closure-sees-outer-macrolet
  (funcall (first (eval '(macrolet ((local-wrap (form) `(list :wrap ,form)))
                          (macrolet ((expand ()
                                       `',(list (lambda (x) (local-wrap (* 2 x))))))
                            (expand)))))
           3)
  (:wrap 6))

(deftest macrolet-expander-closure-sees-outer-symbol-macrolet
  (funcall (first (eval '(symbol-macrolet ((delta 15))
                          (macrolet ((expand ()
                                       `',(list (lambda (x) (+ delta x)))))
                            (expand)))))
           1)
  16)

;; COMPILE needs an emitter.
(deftest-emitting-only macrolet-expander-closure-compiled
  (funcall (first (funcall (compile nil '(lambda ()
                                           (macrolet ((local-wrap (form) `(list :in ,form)))
                                             (macrolet ((expand ()
                                                          `',(list (lambda (x) (local-wrap x)))))
                                               (expand)))))))
           :a)
  (:in :a))

(deftest let-shadows-global-symbol-macro-in-environment
  (let ((mlenv-global 2))
    (declare (ignorable mlenv-global))
    (mlenv-expand-global))
  (mlenv-global nil))

(deftest global-symbol-macro-in-environment-unshadowed
  (mlenv-expand-global)
  ("global" t))

(deftest let-shadows-global-symbol-macro-eval
  (eval '(let ((mlenv-global 2))
          (declare (ignorable mlenv-global))
          (mlenv-expand-global)))
  (mlenv-global nil))

(deftest let-shadows-symbol-macrolet-in-environment
  (symbol-macrolet ((mlenv-sm 1))
    (let ((mlenv-sm 2))
      (declare (ignorable mlenv-sm))
      (macrolet ((q (&environment e)
                   `',(multiple-value-list (macroexpand 'mlenv-sm e))))
        (q))))
  (mlenv-sm nil))

(deftest symbol-macrolet-shadows-let-in-environment
  (let ((mlenv-sm 2))
    (declare (ignorable mlenv-sm))
    (symbol-macrolet ((mlenv-sm 1))
      (macrolet ((q (&environment e)
                   `',(multiple-value-list (macroexpand 'mlenv-sm e))))
        (q))))
  (1 t))

(deftest let-shadow-in-lambda-body-in-environment
  (funcall (lambda (mlenv-global)
             (declare (ignorable mlenv-global))
             (mlenv-expand-global))
           5)
  (mlenv-global nil))

;; The expander is made while the compiler walks the lambda body; also when
;; EVAL interprets, its &ENVIRONMENT must show the SYMBOL-MACROLET around it.
(deftest-emitting-only symbol-macrolet-in-lambda-seen-by-expander
  (funcall (compile nil '(lambda ()
                          (funcall (lambda ()
                                     (symbol-macrolet ((mlenv-sm 1))
                                       (macrolet ((q (&environment e)
                                                    `',(multiple-value-list
                                                        (macroexpand 'mlenv-sm e))))
                                         (q))))))))
  (1 t))
