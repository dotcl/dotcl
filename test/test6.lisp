;;; test6: Literals and quote
;;; Expected output: (1 "hello" :FOO)
(progn
  (defun test-literals ()
    (list 1 "hello" :foo))
  (print (test-literals)))
