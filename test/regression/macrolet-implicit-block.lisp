;;; The expander of a MACROLET macro is in an implicit block named by the macro.

(deftest macrolet-implicit-block.return-from
  (macrolet ((m (x) (when (consp x) (return-from m `(quote (:early ,x)))) `(quote (:late ,x))))
    (list (m (a b)) (m c)))
  ((:early (a b)) (:late c)))

(deftest macrolet-implicit-block.environment-and-optional
  (macrolet ((m (x &optional (y 7) &environment env)
               (declare (ignorable env))
               (if (consp x) (return-from m `(quote (,x ,y))) 0)))
    (list (m (a b)) (m (c) 8) (m d)))
  (((a b) 7) ((c) 8) 0))

(deftest macrolet-implicit-block.interpreted
  (eval '(macrolet ((m (x) (when (consp x) (return-from m ''early)) ''late))
          (list (m (a)) (m b))))
  (early late))

(deftest macrolet-implicit-block.in-function
  (progn
    (defun macrolet-implicit-block-fn ()
      (macrolet ((m (x) (return-from m `(quote ,x))))
        (m (p q))))
    (macrolet-implicit-block-fn))
  (p q))
