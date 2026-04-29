;; test19.lisp — edge case regression tests for bug fixes
(progn
  ;; Multi-pair setq
  (let ((a 0) (b 0))
    (setq a 10 b 20)
    (print (+ a b)))                    ;; => 30

  ;; flet function captured by closure
  (flet ((add1 (x) (+ x 1)))
    (print (funcall (lambda (n) (add1 n)) 41)))  ;; => 42

  ;; &optional with default referencing captured var (lambda, not defun)
  (let ((base 100))
    (print (funcall (lambda (x &optional (offset base)) (+ x offset)) 5)))  ;; => 105

  ;; labels mutual recursion captured by closure
  (labels ((even-p (n)
             (if (= n 0) t (odd-p (1- n))))
           (odd-p (n)
             (if (= n 0) nil (even-p (1- n)))))
    (let ((checker (lambda (n) (even-p n))))
      (print (funcall checker 6))))     ;; => T

  ;; setf returns the value (not the cons)
  (let ((pair (cons 1 2)))
    (print (setf (car pair) 99)))       ;; => 99

  ;; nested closures with optional params
  (print (funcall
          (lambda (a &optional (b 10))
            (funcall (lambda () (+ a b))))
          5 20)))                        ;; => 25
