(progn
  (defun make-counter ()
    (let ((n 0))
      (lambda () (setq n (+ n 1)))))
  (let ((counter (make-counter)))
    (progn
      (funcall counter)
      (funcall counter)
      (funcall counter))))
