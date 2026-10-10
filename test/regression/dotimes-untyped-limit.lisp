;;; DOTIMES over a count that is not statically a fixnum compares the counter
;;; with the limit raw. The count is clamped into int64 once, before the loop
;;; (%DOTIMES-LIMIT): a fixnum is itself, a bignum beyond int64 the largest
;;; int64 or 0, a non-integer real its ceiling. Before, every iteration called
;;; a helper comparing the raw counter with the boxed count and tested the
;;; boolean it returned, which cost more than the body of a short loop.
;;;
;;; What the tests pin: the shape (no per-iteration helper, the limit in an
;;; Int64 slot) and the answers for every kind of count, including the value of
;;; the counter in the result form, in all evaluators.

(setf dotcl:*save-sil* t)

(defun %dul-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %dul-sum (n)
  (let ((acc 0)) (declare (fixnum acc)) (dotimes (i n acc) (setq acc (+ acc i)))))
(defun %dul-count (n)
  (let ((k 0)) (dotimes (i n) (incf k)) k))
(defun %dul-last (n) (dotimes (i n i)))

(deftest-emitting-only dotimes-untyped-limit.shape
  (let ((s (%dul-sil #'%dul-sum)))
    (list (and (search "FixnumGeObject" s) t)
          (and (search "IsTrueGe" s) t)
          (and (search "DotimesLimit" s) t)))
  (nil nil t))

(deftest dotimes-untyped-limit.values
  (list (%dul-sum 0) (%dul-sum 10) (%dul-sum -3) (%dul-sum 100000)
        (%dul-count 7/2) (%dul-count 2.5d0) (%dul-count 3.0) (%dul-count -1/2)
        (%dul-count (- (expt 2 80)))
        (%dul-last 4) (%dul-last 7/2) (%dul-last -5) (%dul-last (- (expt 2 70))))
  (0 45 0 4999950000 4 3 3 0 0 4 4 0 0))

(deftest dotimes-untyped-limit.not-a-number
  (handler-case (progn (%dul-count "abc") :no-error)
    (error () :error))
  :error)
