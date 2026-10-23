;;; COND clauses with a body lower their test the way IF does (native
;;; comparisons, NOT, AND / OR chains). The values must not change.

(defun %ctl-cmp (x y)
  (declare (fixnum x y))
  (cond ((not (< y x)) :ge)
        ((and (> x 10) (not (= y 0))) :big)
        ((or (= x 5) (null y)) :five)
        (t :other)))

(deftest cond-test-lowering.comparisons
  (list (%ctl-cmp 1 2) (%ctl-cmp 2 2) (%ctl-cmp 20 3) (%ctl-cmp 20 0) (%ctl-cmp 5 1) (%ctl-cmp 7 1))
  (:ge :ge :big :other :five :other))

(defun %ctl-mixed (x)
  (cond ((member x '(a b)))
        ((not (numberp x)) (values :not-number x))
        ((evenp x) (values :even (/ x 2)))))

(deftest cond-test-lowering.values-and-test-only-clause
  (list (multiple-value-list (%ctl-mixed 'b))
        (multiple-value-list (%ctl-mixed "s"))
        (multiple-value-list (%ctl-mixed 8))
        (multiple-value-list (%ctl-mixed 7)))
  (((b)) (:not-number "s") (:even 4) (nil)))

(defun %ctl-count (n acc)
  (declare (fixnum n acc))
  (cond ((not (> n 0)) acc)
        (t (%ctl-count (1- n) (+ acc 1)))))

(deftest cond-test-lowering.tail-call-in-clause
  (%ctl-count 100000 0)
  100000)
