;;; COUNT-IF, FIND-IF, POSITION-IF, REMOVE-IF, ... with #'EVENP / #'ODDP decide a fixnum element
;;; without calling the function. A bignum, a non-integer and a :KEY still take the general path.

(deftest seq-pred-evenp-oddp.vector-and-list
  (list (count-if #'evenp #(1 2 3 4 5 6 -2 -3 0))
        (count-if #'oddp '(1 2 3 4 5 6 -2 -3 0))
        (find-if #'evenp #(1 3 5 8 9))
        (find-if #'oddp '(2 4 7 8))
        (position-if #'evenp #(1 3 5 8 9))
        (position-if #'oddp '(2 4 7 8) :from-end t)
        (coerce (remove-if #'evenp #(1 2 3 4 5)) 'list)
        (remove-if-not #'evenp '(1 2 3 4 5))
        (count-if-not #'evenp #(1 2 3 4 5))
        (find-if-not #'oddp #(1 3 4 5)))
  (5 4 8 7 3 2 (1 3 5) (2 4) 3 4))

(deftest seq-pred-evenp-oddp.bounds-key-bignum
  (list (count-if #'evenp #(1 2 3 4 5 6) :start 1 :end 4)
        (count-if #'evenp '((1) (2) (4)) :key #'car)
        (find-if #'oddp #(2 4 5 7) :from-end t)
        (count-if #'evenp (list (expt 10 30) (1+ (expt 10 30)) 2))
        (position-if #'oddp (list (expt 10 30) (1+ (expt 10 30)) 2))
        (find-if #'evenp (make-array 5 :element-type 'fixnum :initial-contents '(1 3 6 7 8))))
  (2 2 7 2 1 6))

(deftest seq-pred-evenp-oddp.non-integer-signals
  (list (handler-case (count-if #'evenp #(1 2.5 3)) (error () :error))
        (handler-case (find-if #'oddp '(2 a)) (error () :error))
        (handler-case (remove-if #'evenp #(1 "x")) (error () :error)))
  (:error :error :error))
