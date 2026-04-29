;;; sequence.lisp — ANSI tests for sequence operations

;;; --- find ---

(deftest find.1
  (find 3 '(1 2 3 4 5))
  3)

(deftest find.2
  (find 6 '(1 2 3 4 5))
  nil)

(deftest find.empty
  (find 1 '())
  nil)

(deftest find.key
  (find 2 '((1 a) (2 b) (3 c)) :key #'car)
  (2 b))

(deftest find.test
  (find "hello" '("hi" "hello" "bye") :test #'equal)
  "hello")

(deftest find-if.1
  (find-if #'evenp '(1 2 3 4 5))
  2)

(deftest find-if.2
  (find-if #'evenp '(1 3 5))
  nil)

(deftest find-if.empty
  (find-if #'evenp '())
  nil)

(deftest find-if.key
  (find-if #'evenp '((1 a) (2 b) (3 c)) :key #'car)
  (2 b))

(deftest find-if-not.1
  (find-if-not #'evenp '(2 4 5 6))
  5)

(deftest find-if-not.empty
  (find-if-not #'evenp '())
  nil)

;;; --- remove ---

(deftest remove.1
  (remove 3 '(1 2 3 4 3 5))
  (1 2 4 5))

(deftest remove.empty
  (remove 1 '())
  nil)

(deftest remove.none
  (remove 9 '(1 2 3))
  (1 2 3))

(deftest remove.all
  (remove 1 '(1 1 1))
  nil)

(deftest remove.key
  (remove 2 '((1 a) (2 b) (3 c)) :key #'car)
  ((1 a) (3 c)))

(deftest remove-if.1
  (remove-if #'evenp '(1 2 3 4 5))
  (1 3 5))

(deftest remove-if.empty
  (remove-if #'evenp '())
  nil)

(deftest remove-if-not.1
  (remove-if-not #'evenp '(1 2 3 4 5))
  (2 4))

(deftest remove-if-not.empty
  (remove-if-not #'evenp '())
  nil)

;;; --- count ---

(deftest count.1
  (count 3 '(1 3 2 3 4 3))
  3)

(deftest count.empty
  (count 1 '())
  0)

(deftest count.none
  (count 9 '(1 2 3))
  0)

(deftest count-if.1
  (count-if #'evenp '(1 2 3 4 5 6))
  3)

(deftest count-if.empty
  (count-if #'evenp '())
  0)

(deftest count-if-not.1
  (count-if-not #'evenp '(1 2 3 4 5 6))
  3)

(deftest count-if-not.empty
  (count-if-not #'evenp '())
  0)

(deftest count-if-not.all
  (count-if-not #'evenp '(2 4 6))
  0)

;;; --- position ---

(deftest position.1
  (position 3 '(1 2 3 4 5))
  2)

(deftest position.2
  (position 6 '(1 2 3 4 5))
  nil)

(deftest position.empty
  (position 1 '())
  nil)

(deftest position.first
  (position 1 '(1 2 3))
  0)

(deftest position-if.1
  (position-if #'evenp '(1 2 3 4))
  1)

(deftest position-if.empty
  (position-if #'evenp '())
  nil)

;;; --- reduce ---

(deftest reduce.1
  (reduce #'+ '(1 2 3 4 5))
  15)

(deftest reduce.2
  (reduce #'+ '(1 2 3) :initial-value 10)
  16)

(deftest reduce.single
  (reduce #'+ '(42))
  42)

(deftest reduce.initial-only
  (reduce #'+ '() :initial-value 99)
  99)

;;; --- every / some ---

(deftest every.1
  (notnot (every #'numberp '(1 2 3)))
  t)

(deftest every.2
  (every #'numberp '(1 "a" 3))
  nil)

(deftest every.empty
  (notnot (every #'evenp '()))
  t)

(deftest some.1
  (notnot (some #'evenp '(1 2 3)))
  t)

(deftest some.2
  (some #'evenp '(1 3 5))
  nil)

(deftest some.empty
  (some #'evenp '())
  nil)

(deftest notevery.1
  (notnot (notevery #'evenp '(1 2 3)))
  t)

(deftest notevery.all-match
  (notevery #'evenp '(2 4 6))
  nil)

(deftest notany.1
  (notnot (notany #'evenp '(1 3 5)))
  t)

(deftest notany.has-match
  (notany #'evenp '(1 2 3))
  nil)

;;; --- set operations ---

(deftest union.1
  (let ((u (union '(1 2 3) '(2 3 4))))
    (and (find 1 u) (find 2 u) (find 3 u) (find 4 u)
         (= (length u) 4)))
  t)

(deftest union.empty-left
  (union '() '(1 2 3))
  (1 2 3))

(deftest union.empty-both
  (union '() '())
  nil)

(deftest intersection.1
  (intersection '(1 2 3 4) '(2 4 6))
  (2 4))

(deftest intersection.empty
  (intersection '(1 2 3) '(4 5 6))
  nil)

(deftest intersection.empty-left
  (intersection '() '(1 2 3))
  nil)

(deftest set-difference.1
  (set-difference '(1 2 3 4) '(2 4))
  (1 3))

(deftest set-difference.empty
  (set-difference '(1 2 3) '(1 2 3))
  nil)

(deftest set-difference.none-removed
  (set-difference '(1 2 3) '(4 5 6))
  (1 2 3))

(deftest subsetp.1
  (notnot (subsetp '(1 2) '(1 2 3 4)))
  t)

(deftest subsetp.2
  (subsetp '(1 5) '(1 2 3 4))
  nil)

(deftest subsetp.empty
  (notnot (subsetp '() '(1 2 3)))
  t)

;;; --- elt ---

(deftest elt.1
  (elt '(a b c) 1)
  b)

(deftest elt.2
  (elt "hello" 1)
  #\e)

(deftest elt.first
  (elt '(x y z) 0)
  x)

;;; --- subseq ---

(deftest subseq.1
  (subseq "hello" 1 3)
  "el")

(deftest subseq.2
  (subseq '(a b c d e) 1 3)
  (b c))

(deftest subseq.to-end
  (subseq "hello" 2)
  "llo")

(deftest subseq.list-to-end
  (subseq '(a b c d e) 3)
  (d e))

(deftest subseq.empty-range
  (subseq "hello" 2 2)
  "")

;;; --- reverse ---

(deftest reverse.1
  (reverse '(1 2 3))
  (3 2 1))

(deftest reverse.2
  (reverse "hello")
  "olleh")

(deftest reverse.empty-list
  (reverse '())
  nil)

(deftest reverse.empty-string
  (reverse "")
  "")

(deftest reverse.single
  (reverse '(42))
  (42))

;;; --- sort ---

(deftest sort.1
  (sort '(3 1 4 1 5 9 2 6) #'<)
  (1 1 2 3 4 5 6 9))

(deftest sort.descending
  (sort '(3 1 4 1 5) #'>)
  (5 4 3 1 1))

(deftest sort.already-sorted
  (sort '(1 2 3) #'<)
  (1 2 3))

;;; --- map variants ---

(deftest mapc.1
  (let ((result (mapc #'print '(1 2 3))))
    (notnot (listp result)))
  t)

(deftest mapcan.1
  (mapcan (lambda (x) (if (evenp x) (list x) nil)) '(1 2 3 4 5 6))
  (2 4 6))

(deftest mapcan.empty
  (mapcan (lambda (x) (list x)) '())
  nil)

(deftest mapcan.all-nil
  (mapcan (lambda (x) nil) '(1 2 3))
  nil)

;;; --- complement / constantly ---

(deftest complement.1
  (notnot (funcall (complement #'evenp) 3))
  t)

(deftest complement.2
  (funcall (complement #'evenp) 4)
  nil)

(deftest constantly.1
  (funcall (constantly 42) 1 2 3)
  42)

(deftest constantly.nil
  (funcall (constantly nil))
  nil)

;;; --- rassoc ---

(deftest rassoc.1
  (rassoc 2 '((a . 1) (b . 2) (c . 3)))
  (b . 2))

(deftest rassoc.not-found
  (rassoc 9 '((a . 1) (b . 2)))
  nil)

(deftest rassoc.empty
  (rassoc 1 '())
  nil)

;;; --- acons ---

(deftest acons.1
  (acons 'a 1 '((b . 2)))
  ((a . 1) (b . 2)))

(deftest acons.empty
  (acons 'x 42 nil)
  ((x . 42)))

;;; --- pairlis ---

(deftest pairlis.1
  (let ((result (pairlis '(a b c) '(1 2 3))))
    (and (= (length result) 3)
         (eql (cdr (assoc 'a result)) 1)
         (eql (cdr (assoc 'b result)) 2)
         (eql (cdr (assoc 'c result)) 3)))
  t)

(deftest pairlis.empty
  (pairlis '() '())
  nil)

(deftest pairlis.with-alist
  (let ((result (pairlis '(a) '(1) '((b . 2)))))
    (and (= (length result) 2)
         (eql (cdr (assoc 'a result)) 1)
         (eql (cdr (assoc 'b result)) 2)))
  t)

(do-tests-summary)
