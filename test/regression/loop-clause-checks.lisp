;;; LOOP accumulation and clause-order checks (CLHS 6.1.3, 6.1.1.7, loop syntax).
;;; SUM and COUNT share one accumulator, whose default type is the union of
;;; their defaults; a variable bound twice is an error; and a variable clause
;;; may not follow return.

(defun %lcc-expansion-error (form)
  (handler-case (progn (macroexpand form) :no-error)
    (program-error () :program-error)
    (error (c) (type-of c))))

(deftest loop-clause-checks.count-and-sum
  (list (loop for i from 0 below 5
              sum i into s
              count (oddp i) into s
              finally (return s))
        (loop for i from 0 below 3
              count t into s of-type number
              finally (return s)))
  (12 3))

(deftest loop-clause-checks.sum-after-count
  (list (loop for i from 0 below 3
              count t into s
              sum (* i (expt 10 40)) into s
              finally (return s))
        (loop for i from 0 below 3
              count t into s
              sum 1/2 into s
              finally (return s))
        (loop for i from 0 below 3
              count t into s
              sum 0.5 into s
              finally (return s))
        (loop for i from 0 below 3
              count t into s
              sum 0.5d0 into s
              finally (return s)))
  (30000000000000000000000000000000000000003 9/2 4.5 4.5d0))

(deftest loop-clause-checks.duplicate-destructuring-with
  (list (%lcc-expansion-error
         '(loop for a in '(1 2)
                with (a b) = '(3 4)
                collect (list a b)))
        (%lcc-expansion-error
         '(loop for (a b) = '(1 2) then '(3 4)
                with (a b) = '(5 6)
                collect (list a b))))
  (:program-error :program-error))

(deftest loop-clause-checks.return-before-for
  (list (%lcc-expansion-error '(loop return 1 for i from 0 below 2))
        (loop return 1))
  (:program-error 1))
