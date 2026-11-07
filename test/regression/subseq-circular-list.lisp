;;; (subseq circular-list start end) walks only as far as END: it is how a few elements are taken
;;; from a circular list. A proper list's bounds errors are the same as before.

(deftest subseq-circular-list.takes-a-prefix
  (let ((c (list 1 2 3)))
    (setf (cdr (last c)) c)
    (list (subseq c 0 3) (subseq c 1 6) (subseq c 2 2) (subseq c 0 0)))
  ((1 2 3) (2 3 1 2 3) nil nil))

(deftest subseq-circular-list.proper-list-errors-unchanged
  (list (handler-case (subseq (list 1 2 3) 0 5) (error () :error))
        (handler-case (subseq (list 1 2 3) 4 4) (error () :error))
        (handler-case (subseq (list 1 2 3) 2 1) (error () :error))
        (subseq (list 1 2 3) 1)
        (subseq (list 1 2 3) 1 3)
        (subseq nil 0 0))
  (:error :error :error (2 3) (2 3) nil))
