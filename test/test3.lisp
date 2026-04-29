(progn
  (defun myabs (x)
    (if (> x 0) x (- 0 x)))
  (myabs -42))
