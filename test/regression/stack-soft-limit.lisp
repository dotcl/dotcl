;;; A runaway recursion is stopped at a depth limit (144 MB of stack by default) rather than at
;;; the end of the 256 MB thread stack: unwinding two million frames took seconds. A
;;; recursion a program really needs still gets far deeper than SBCL's 2 MB stack allows.

(defvar *ssl-depth* 0)
(defun ssl-runaway (n) (setq *ssl-depth* n) (+ 1 (ssl-runaway (+ n 1))))
(defun ssl-deep (n) (if (= n 0) 0 (+ 1 (ssl-deep (- n 1)))))

(deftest stack-soft-limit.runaway-stops-at-the-limit
  (progn
    (setq *ssl-depth* 0)
    (handler-case (ssl-runaway 0)
      (storage-condition () nil))
    (list (> *ssl-depth* 500) (< *ssl-depth* 1700000)))
  (t t))

(deftest stack-soft-limit.deep-recursion-still-works
  (ssl-deep 1000)
  1000)
