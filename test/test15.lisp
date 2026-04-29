;; test15.lisp — setf, push/pop, incf/decf, function, list*
(progn
  ;; setf on symbol
  (let ((x 10))
    (setf x 42)
    (print x))                        ;; => 42

  ;; setf on car/cdr
  (let ((pair (cons 1 2)))
    (setf (car pair) 'a)
    (setf (cdr pair) 'b)
    (print pair))                     ;; => (A . B)

  ;; push/pop
  (let ((stack nil))
    (push 1 stack)
    (push 2 stack)
    (push 3 stack)
    (print stack)                     ;; => (3 2 1)
    (print (pop stack))               ;; => 3
    (print stack))                    ;; => (2 1)

  ;; incf/decf
  (let ((n 10))
    (incf n)
    (print n)                         ;; => 11
    (decf n 5)
    (print n))                        ;; => 6

  ;; function + funcall
  ;; Note: cannot use #'+ here because test-a2 cross-compiles standalone
  ;; without stdlib, so + isn't registered as a callable function.
  (defun test15-add (a b) (+ a b))
  (print (funcall (function test15-add) 1 2)) ;; => 3

  ;; list*
  (print (list* 1 2 3 '(4 5))))     ;; => (1 2 3 4 5)
