;;; test7: block/return-from
;;; Expected output: 42
(progn
  (defun test-block ()
    (block done
      (+ 1 (return-from done 42))
      99))
  (print (test-block)))
