;;; A &rest function called with up to four extra arguments takes them as real
;;; parameters: no argument array is built, only the rest list.

(defun %rda-len (&rest xs) (let ((l xs)) (length l)))
(defun %rda-sum (a &rest xs) (+ a (apply #'+ xs)))
(defun %rda-xs (&rest xs) xs)

(defun %rda-loop3 ()
  (dotimes (i 100000) (%rda-len 1 2 3))
  nil)

(defun %rda-loop4 ()
  (dotimes (i 100000) (%rda-len 1 2 3 4))
  nil)

(deftest-emitting-only rest-direct-arity.no-array
  (progn
    (%rda-loop3) (%rda-loop4)
    (list (< (min-allocated-bytes #'%rda-loop3) 10400000)
          (< (min-allocated-bytes #'%rda-loop4) 13600000)))
  (t t))

(deftest rest-direct-arity.values
  (list (%rda-len) (%rda-len 1) (%rda-len 1 2 3) (%rda-len 1 2 3 4) (%rda-len 1 2 3 4 5)
        (%rda-sum 1) (%rda-sum 1 2 3 4 5) (%rda-sum 1 2 3 4 5 6 7)
        (%rda-xs 1 2 3 4) (%rda-xs) (apply #'%rda-xs '(1 2 3 4))
        (funcall #'%rda-xs 1 2 3))
  (0 1 3 4 5 1 15 28 (1 2 3 4) nil (1 2 3 4) (1 2 3)))
