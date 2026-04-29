;;; test10: special (dynamic) variables
;;; Expected output: 42
(progn
  (defvar *my-var*)
  (defun get-dyn ()
    *my-var*)
  (defun test-special ()
    (let ((*my-var* 42))
      (get-dyn)))
  (print (test-special)))
