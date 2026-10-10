;;; A macro that asks its &ENVIRONMENT about a symbol macro is expanded first by
;;; the boxing analysis, which does not see every variable binding in the form
;;; it walks (a lambda list, for one). The compiler reuses that expansion only
;;; where the symbol macro names resolve the same way; where a variable hides
;;; one of them, the macro is expanded again and must see the variable.

(defmacro expansion-memo-probe (name &environment env)
  `',(multiple-value-list (macroexpand-1 name env)))

(define-symbol-macro expansion-memo-global :global)

;;; A SYMBOL-MACROLET name hidden by a parameter, inside a LET (the LET makes
;;; the analysis walk its body before the compiler binds the parameter). At
;;; top level, so that no enclosing function is analysed first.
(defparameter *expansion-memo-flet*
  (symbol-macrolet ((expansion-memo-x :lexical))
    (let ((n 0))
      (flet ((g (expansion-memo-x)
               (incf n)
               (list expansion-memo-x (expansion-memo-probe expansion-memo-x))))
        (g 1)))))

(deftest expansion-memo-lexical-hidden-by-flet-parameter
  *expansion-memo-flet*
  (1 (expansion-memo-x nil)))

(defparameter *expansion-memo-labels*
  (symbol-macrolet ((expansion-memo-x :lexical))
    (let ((n 0))
      (labels ((g (expansion-memo-x)
                 (incf n)
                 (list expansion-memo-x (expansion-memo-probe expansion-memo-x))))
        (g 1)))))

(deftest expansion-memo-lexical-hidden-by-labels-parameter
  *expansion-memo-labels*
  (1 (expansion-memo-x nil)))

(defparameter *expansion-memo-lambda*
  (symbol-macrolet ((expansion-memo-x :lexical))
    (let ((n 0))
      (incf n)
      (funcall (lambda (expansion-memo-x)
                 (list expansion-memo-x (expansion-memo-probe expansion-memo-x)))
               1))))

(deftest expansion-memo-lexical-hidden-by-lambda-parameter
  *expansion-memo-lambda*
  (1 (expansion-memo-x nil)))

;;; The same for a global symbol macro.
(deftest expansion-memo-global-hidden-by-parameter
  (let ((f (lambda (expansion-memo-global)
             (list expansion-memo-global
                   (expansion-memo-probe expansion-memo-global)))))
    (funcall f 2))
  (2 (expansion-memo-global nil)))

;;; Not hidden: the expansion the analysis made is the right one.
(deftest expansion-memo-lexical-visible
  (symbol-macrolet ((expansion-memo-x :lexical))
    (let ((n 0))
      (let ((f (lambda () (incf n) (expansion-memo-probe expansion-memo-x))))
        (list (funcall f) (funcall f) n))))
  ((:lexical t) (:lexical t) 2))

(deftest expansion-memo-global-visible
  (let ((n 0))
    (let ((f (lambda () (incf n) (expansion-memo-probe expansion-memo-global))))
      (list (funcall f) n)))
  ((:global t) 1))

;;; One (EQ) form placed where the name is a symbol macro and where a parameter
;;; hides it.
(defmacro expansion-memo-twice (form)
  `(list (symbol-macrolet ((expansion-memo-y :outer)) ,form)
         (funcall (lambda (expansion-memo-y) (declare (ignorable expansion-memo-y)) ,form)
                  :param)))

(deftest expansion-memo-shared-form
  (let ((n 0))
    (let ((f (lambda () (incf n) (expansion-memo-twice (expansion-memo-probe expansion-memo-y)))))
      (funcall f)))
  ((:outer t) (expansion-memo-y nil)))
