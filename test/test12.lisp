;; test12.lisp — &rest parameter tests
(progn
  (defun my-list (&rest args) args)
  (print (my-list 1 2 3))         ;; => (1 2 3)
  (defun rest-after (x &rest more) more)
  (print (rest-after 1 2 3))      ;; => (2 3)
  (defun make-appender (prefix)
    (lambda (&rest items) (append prefix items)))
  (let ((f (make-appender '(a))))
    (print (funcall f 'b 'c))))   ;; => (A B C)
