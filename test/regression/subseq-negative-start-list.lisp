;;; A negative START is an error for SUBSEQ on a list, with or without END, as it is for vectors and strings.

(deftest subseq-negative-start.list-with-end
  (list (handler-case (subseq (list 1 2 3) -1 2) (type-error () :type-error))
        (handler-case (subseq (list 1 2 3) -1) (type-error () :type-error))
        (handler-case (subseq (vector 1 2 3) -1 2) (type-error () :type-error))
        (handler-case (subseq "abc" -1 2) (type-error () :type-error)))
  (:type-error :type-error :type-error :type-error))

(deftest subseq-negative-start.valid-calls-unchanged
  (list (subseq (list 1 2 3) 1 3) (subseq (list 1 2 3) 0 0) (subseq (list 1 2 3) 3))
  ((2 3) nil nil))
