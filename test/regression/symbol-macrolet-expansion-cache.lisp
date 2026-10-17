;;; A macro form under SYMBOL-MACROLET is expanded once per compile, however
;;; deeply such forms nest inside closures.

(defvar *sm-cache-count* 0)

(defmacro sm-cache-wrap (x)
  (incf *sm-cache-count*)
  `(symbol-macrolet ((sm-cache-unused 0))
     (let ((r nil)) (lambda () (setq r ,x) r))))

(defun sm-cache-nest (n)
  (if (= n 0) 1 `(sm-cache-wrap ,(sm-cache-nest (1- n)))))

(defun sm-cache-expansions (n)
  (setq *sm-cache-count* 0)
  (compile nil `(lambda () ,(sm-cache-nest n)))
  *sm-cache-count*)

(deftest-compiled-only symbol-macrolet-expansion-cache-linear
  (list (sm-cache-expansions 2) (sm-cache-expansions 8))
  (2 8))

(deftest-compiled-only symbol-macrolet-expansion-cache-value
  (let ((f (funcall (compile nil `(lambda () ,(sm-cache-nest 3))))))
    (funcall (funcall (funcall f))))
  1)
