;;; #'+ #'- #'* #'/ carry a 2-argument direct delegate, so handing them to
;;; REDUCE / MAPCAR / SORT does not build an args array and a &rest list per
;;; call. Only the 2-argument shape is attached; every other arity still runs
;;; the n-ary wrapper, and both must agree.

(deftest arith-fast.plus-arities
  (list (funcall #'+) (funcall #'+ 5) (funcall #'+ 1 2)
        (funcall #'+ 1 2 3 4) (apply #'+ '(1 2 3)))
  (0 5 3 10 6))

(deftest arith-fast.minus-arities
  (list (funcall #'- 5) (funcall #'- 10 3) (funcall #'- 10 3 2))
  (-5 7 5))

(deftest arith-fast.times-arities
  (list (funcall #'*) (funcall #'* 5) (funcall #'* 2 3) (funcall #'* 2 3 4))
  (1 5 6 24))

(deftest arith-fast.divide-arities
  (list (funcall #'/ 4) (funcall #'/ 10 4) (funcall #'/ 24 2 3))
  (1/4 5/2 4))

(deftest arith-fast.unary-minus-keeps-negative-zero
  ;; (- 0.0) is a sign flip, not (- 0 0.0) -- the unary case must stay on the
  ;; wrapper, which is why only the 2-argument delegate is attached.
  (list (funcall #'- 0.0) (funcall #'+ -0.0))
  (-0.0 0.0))

(deftest arith-fast.number-types
  (list (funcall #'+ 1.5 2)
        (funcall #'* (expt 2 70) 2)
        (funcall #'+ 1/3 1/6)
        (funcall #'- 1/2 1/2)
        (funcall #'+ #c(1 2) 1)
        (funcall #'* #c(0 1) #c(0 1)))
  (3.5 2361183241434822606848 1/2 0 #c(2 2) -1))

(deftest arith-fast.errors
  (list (handler-case (funcall #'+ 1 'a) (type-error () :te) (error () :err))
        (handler-case (funcall #'/ 1 0) (division-by-zero () :dbz) (error () :err)))
  (:te :dbz))

(deftest arith-fast.through-sequence-functions
  (list (mapcar #'+ '(1 2) '(10 20))
        (reduce #'+ '(1 2 3 4))
        (reduce #'* '(1 2 3 4))
        (reduce #'- '(10 1 2))
        (sort (list 3 1 2) #'<))
  ((11 22) 10 24 7 (1 2 3)))

;;; REDUCE folds a list without materialising it. The shapes that path has to
;;; keep: no initial value, initial value, :key, empty sequence, one element,
;;; and the cases that still take the array path (:from-end, :start, :end).

(deftest reduce-list.no-initial-value
  (list (reduce #'list '(1 2 3 4)) (reduce #'+ '(7)) (reduce #'+ '()))
  ((((1 2) 3) 4) 7 0))

(deftest reduce-list.initial-value
  (list (reduce #'+ '(1 2 3) :initial-value 10)
        (reduce #'+ '() :initial-value 5)
        (reduce #'list '(1 2) :initial-value :seed))
  (16 5 ((:seed 1) 2)))

(deftest reduce-list.key
  (list (reduce #'+ '((1) (2) (3)) :key #'car)
        (reduce #'+ '((1) (2)) :key #'car :initial-value 100))
  (6 103))

(deftest reduce-list.from-end
  (list (reduce #'cons '(1 2 3) :from-end t :initial-value nil)
        (reduce #'list '(1 2 3) :from-end t))
  ((1 2 3) (1 (2 3))))

(deftest reduce-list.start-end
  (list (reduce #'+ '(1 2 3 4 5) :start 1 :end 4)
        (reduce #'+ '(1 2 3 4 5) :start 2))
  (9 12))

(deftest reduce-list.nil-elements-are-values-not-end
  (reduce (lambda (a b) (list a b)) '(nil nil))
  (nil nil))

(deftest reduce-list.vector-and-string-unchanged
  (list (reduce #'+ #(1 2 3))
        (reduce (lambda (a b) (format nil "~A~A" a b)) "abc"))
  (6 "abc"))
