;;; test11: cond, when, and, or
;;; Expected output: "medium"
(progn
  (defun classify (x)
    (cond
      ((< x 0) "negative")
      ((= x 0) "zero")
      ((< x 10) "small")
      ((< x 100) "medium")
      (t "large")))
  (defun test-logic ()
    (when (and (> 50 0) (or nil 50))
      (classify 50)))
  (print (test-logic)))
