;; test14.lisp — eval-when tests
(progn
  (eval-when (:compile-toplevel :load-toplevel)
    (defun ct-square (n) (* n n)))
  (defmacro square-literal (n)
    (ct-square n))
  (print (square-literal 10))       ;; => 100

  (defmacro swap! (a b)
    (let ((tmp (gensym)))
      `(let ((,tmp ,a))
         (setq ,a ,b)
         (setq ,b ,tmp))))
  (let ((x 1) (y 2))
    (swap! x y)
    (print (+ x y))))               ;; => 3
