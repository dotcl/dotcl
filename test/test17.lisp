;; test17.lisp — multiple-value-bind, flet, labels
(progn
  ;; multiple-value-bind
  (multiple-value-bind (a b c)
      (values 10 20 30)
    (print (+ a b c)))               ;; => 60

  ;; multiple-value-list
  (print (multiple-value-list (values 1 2 3))) ;; => (1 2 3)

  ;; flet — local function
  (flet ((double (x) (* x 2))
         (triple (x) (* x 3)))
    (print (+ (double 5) (triple 5)))) ;; => 25

  ;; flet — shadowing global
  (defun my-add (a b) (+ a b))
  (print (flet ((my-add (a b) (* a b)))
           (my-add 3 4)))            ;; => 12

  ;; labels — mutual recursion
  (labels ((even-p (n)
             (if (= n 0) t (odd-p (1- n))))
           (odd-p (n)
             (if (= n 0) nil (even-p (1- n)))))
    (print (even-p 4))               ;; => T
    (print (odd-p 3))))              ;; => T
