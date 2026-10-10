;;; REDUCE calls KEY on each element just before the function takes it, in the
;;; order the fold visits the elements: from the end under :FROM-END. KEY was
;;; called on every element first, in forward order. bit-smasher numbers a bit
;;; vector's places with (incf place) inside its key under :FROM-END, and read
;;; #*0010 as 4 instead of 2, so its bit-vector arithmetic was wrong (7 checks).
;;; Expected values are SBCL 2.6.8's answers.

(deftest reduce-key-order.from-end-vector
  (let ((place -1))
    (reduce #'+ #*0010 :key (lambda (d) (ash d (incf place))) :from-end t))
  2)

(deftest reduce-key-order.from-end-list
  (let ((seen '()))
    (reduce #'list '(a b c) :key (lambda (x) (push x seen) x) :from-end t)
    (nreverse seen))
  (c b a))

(deftest reduce-key-order.forward-vector
  (let ((seen '()))
    (reduce #'list #(a b c) :key (lambda (x) (push x seen) x))
    (nreverse seen))
  (a b c))

;; KEY and the function alternate: KEY runs for an element only when the fold
;; reaches it.
(deftest reduce-key-order.interleaved
  (let ((log '()))
    (reduce (lambda (a b) (push (list :f a b) log) (+ a b))
            #(1 2 3) :key (lambda (x) (push (list :k x) log) x) :from-end t)
    (nreverse log))
  ((:k 3) (:k 2) (:f 2 3) (:k 1) (:f 1 5)))

(deftest reduce-key-order.initial-value-from-end
  (let ((seen '()))
    (list (reduce #'+ "abc" :key (lambda (c) (push c seen) (char-code c))
                            :from-end t :initial-value 0 :start 1)
          (nreverse seen)))
  (197 (#\c #\b)))
