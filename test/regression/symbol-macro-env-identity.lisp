;;; The symbol-macro side of an &ENVIRONMENT object is keyed by the symbol: a
;;; SYMBOL-MACROLET of LIST does not make the keyword :LIST, or a symbol LIST of
;;; another package, a symbol macro for MACROEXPAND. (A code walker that expands
;;; every atom with the environment looped on :LIST forever.)

(defpackage :smenv-other (:use))

(defmacro smenv-expand-1 (form &environment e)
  `',(multiple-value-list (macroexpand-1 form e)))

(deftest symbol-macro-env-keyword-not-expanded
  (symbol-macrolet ((list 1)) (smenv-expand-1 :list))
  (:list nil))

(deftest symbol-macro-env-other-package-not-expanded
  (symbol-macrolet ((list 1)) (smenv-expand-1 smenv-other::list))
  (smenv-other::list nil))

(deftest symbol-macro-env-same-symbol-expanded
  (symbol-macrolet ((list 1)) (smenv-expand-1 list))
  (1 t))

(deftest symbol-macro-env-macroexpand-keyword
  (symbol-macrolet ((list 1))
    (macrolet ((q (&environment e) `',(multiple-value-list (macroexpand :list e))))
      (q)))
  (:list nil))

(define-symbol-macro smenv-global 42)

;;; MACROEXPAND-ALL: a LET over a global symbol macro hides it too.
(deftest symbol-macro-env-mea-let-hides-global
  (dotcl-cltl2:macroexpand-all '(list smenv-global (let ((smenv-global 1)) smenv-global)))
  (list 42 (let ((smenv-global 1)) smenv-global)))

(deftest symbol-macro-env-mea-symbol-macrolet-keyed-by-symbol
  (dotcl-cltl2:macroexpand-all '(symbol-macrolet ((list 1)) (f :list list smenv-other::list)))
  (f :list 1 smenv-other::list))
