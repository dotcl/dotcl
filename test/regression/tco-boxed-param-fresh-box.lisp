;;; A self tail call rebinds an assigned-and-captured parameter to a fresh cell, so a closure made
;;; by an earlier iteration keeps the value it captured.
(defun %tbp-chain (k n)
  (if (= n 0)
      (funcall k 0)
      (progn (setq k k)
             (%tbp-chain (lambda (r) (funcall k (1+ r))) (1- n)))))

(defun %tbp-collect (acc n)
  (if (= n 0)
      (mapcar #'funcall (reverse acc))
      (progn (setq acc acc)
             (%tbp-collect (cons (lambda () (list n (length acc))) acc) (1- n)))))

(deftest tco-boxed-param.continuation-chain
  (list (%tbp-chain #'identity 3) (%tbp-chain #'identity 50))
  (3 50))

(deftest tco-boxed-param.closures-keep-captured-value
  (%tbp-collect nil 3)
  ((3 0) (2 1) (1 2)))
