;; test13.lisp — defmacro tests
(progn
  (defmacro my-when (test &body body)
    `(if ,test (progn ,@body)))
  (print (my-when t 42))           ;; => 42

  (defmacro with-value ((var val) &body body)
    `(let ((,var ,val)) ,@body))
  (with-value (x 'got-it)
    (print x))                      ;; => GOT-IT

  (defmacro my-list-macro (&rest items)
    `(list ,@items))
  (print (my-list-macro 1 2 3)))   ;; => (1 2 3)
