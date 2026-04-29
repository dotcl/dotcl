;; test18.lisp — &optional, &key
(progn
  ;; &optional — basic
  (defun greet (name &optional (greeting "Hello"))
    (list greeting name))
  (print (greet "World"))            ;; => (Hello World)
  (print (greet "World" "Hi"))       ;; => (Hi World)

  ;; &optional — multiple
  (defun opt-test (a &optional (b 10) (c 20))
    (+ a b c))
  (print (opt-test 1))               ;; => 31
  (print (opt-test 1 2))             ;; => 23
  (print (opt-test 1 2 3))           ;; => 6

  ;; &key — basic
  (defun make-point (&key (x 0) (y 0))
    (list x y))
  (print (make-point))               ;; => (0 0)
  (print (make-point :x 3 :y 4))    ;; => (3 4)
  (print (make-point :y 7))          ;; => (0 7)

  ;; &optional in lambda
  (print (funcall (lambda (a &optional (b 100)) (+ a b)) 1))  ;; => 101
  (print (funcall (lambda (a &optional (b 100)) (+ a b)) 1 2))) ;; => 3
