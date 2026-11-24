;;; SUBSEQ of a list with an END far past its length signals at the list's end; it does not
;;; allocate END - START cells first.

(defun %slhe (thunk) (handler-case (progn (funcall thunk) :no-error) (type-error () :type-error)))

(deftest subseq-list-huge-end.error
  (list (%slhe (lambda () (subseq (list 1 2 3 4 5) 5 2147483648)))
        (%slhe (lambda () (subseq (list 1 2 3 4 5) 1 4294967296)))
        (%slhe (lambda () (subseq (list 1 2 3) 0 (expt 2 62))))
        (%slhe (lambda () (subseq (list 1 2 3) 0 100))))
  (:type-error :type-error :type-error :type-error))

(deftest subseq-list-huge-end.valid
  (list (subseq (list 1 2 3 4 5) 1 4) (subseq (list 1 2 3) 3 3) (subseq (list 1 2 3) 0 3)
        (let ((c (list 1 2 3))) (setf (cdddr c) c) (subseq c 2 7)))
  ((2 3 4) nil (1 2 3) (3 1 2 3 1)))
