;;; test9: unwind-protect + return-from
;;; Expected output: (2 1)
(progn
  (defun test-unwind ()
    (let ((log nil))
      (block outer
        (unwind-protect
            (progn
              (setq log (cons 1 log))
              (return-from outer nil))
          (setq log (cons 2 log))))
      log))
  (print (test-unwind)))
