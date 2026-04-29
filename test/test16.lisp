;; test16.lisp — do, dolist, dotimes
(progn
  ;; do — factorial
  (defun factorial (n)
    (do ((i 1 (1+ i))
         (result 1 (* result i)))
        ((> i n) result)))
  (print (factorial 5))              ;; => 120

  ;; do — collect reverse
  (print
    (do ((lst '(1 2 3 4 5) (cdr lst))
         (acc nil (cons (car lst) acc)))
        ((null lst) acc)))           ;; => (5 4 3 2 1)

  ;; dolist
  (let ((sum 0))
    (dolist (x '(1 2 3 4 5))
      (incf sum x))
    (print sum))                     ;; => 15

  ;; dotimes
  (let ((sum 0))
    (dotimes (i 10)
      (incf sum i))
    (print sum))                     ;; => 45

  ;; do* — sequential binding
  (print
    (do* ((x 1 (1+ x))
          (y (* x 2) (* x 2)))
         ((> x 3) y))))              ;; => 8 (x=4, y=4*2=8)
