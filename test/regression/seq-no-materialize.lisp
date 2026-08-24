;;; REMOVE-family, MAPCAN and SORT walk a list directly instead of collecting it
;;; into a List (and, for REMOVE, the dropped indices into a HashSet) first. The
;;; general paths are still there for the shapes the fast ones exclude
;;; (:count, :from-end, :start/:end, DELETE, multi-list MAPCAN), so both have to
;;; keep agreeing.

(deftest seq-nomat.remove-if-basic
  (list (remove-if #'evenp '(1 2 3 4 5))
        (remove-if #'evenp '())
        (remove-if (constantly t) '(1 2))
        (remove-if (constantly nil) '(1 2))
        (remove-if-not #'evenp '(1 2 3 4)))
  ((1 3 5) nil nil (1 2) (2 4)))

(deftest seq-nomat.remove-nothing-returns-same-list
  ;; (setq x (delete .. x)) sharing structure when nothing matched is relied on
  ;; by real code; the fast path must keep it.
  (let ((l (list 1 3 5)))
    (list (eq l (remove-if #'evenp l))
          (eq l (remove 2 l))))
  (t t))

(deftest seq-nomat.remove-with-count-and-from-end
  (list (remove 2 '(1 2 3 2))
        (remove 2 '(1 2 3 2) :count 1)
        (remove 2 '(1 2 3 2) :from-end t :count 1)
        (remove 2 '(1 2 3 2 5) :start 2)
        (remove 2 '(1 2 3 2 5) :end 2))
  ((1 3) (1 3 2) (1 2 3) (1 2 3 5) (1 3 2 5)))

(deftest seq-nomat.remove-key-and-test
  (list (remove 1 '((1) (2)) :key #'car)
        (remove "a" '("a" "b") :test #'string=)
        (remove 3 '(1 2 3 4) :test #'<))
  (((2)) ("b") (1 2 3)))

(deftest seq-nomat.delete-still-destructive
  (let ((l (list 1 2 3 4)))
    (list (delete 2 l) (length l)))
  ((1 3 4) 3))

(deftest seq-nomat.remove-on-vector-and-string
  (list (coerce (remove 2 #(1 2 3)) 'list) (remove #\a "abca"))
  ((1 3) "bc"))

(deftest seq-nomat.mapcan-single-list
  (list (mapcan (lambda (x) (list x x)) '(1 2))
        (mapcan (lambda (x) (when (evenp x) (list x))) '(1 2 3 4))
        (mapcan (lambda (x) (declare (ignore x)) nil) '(1 2))
        (mapcan #'list '()))
  ((1 1 2 2) (2 4) nil nil))

(deftest seq-nomat.mapcan-calls-function-for-every-element
  (let ((seen nil))
    (mapcan (lambda (x) (push x seen) nil) '(1 2 3))
    (nreverse seen))
  (1 2 3))

(deftest seq-nomat.mapcan-multi-list
  (mapcan (lambda (a b) (list a b)) '(1 2) '(10 20))
  (1 10 2 20))

(deftest seq-nomat.mapcan-non-list-piece-last
  ;; NCONC allows a non-list as the last piece.
  (mapcan (lambda (x) (if (= x 2) 'tail (list x))) '(1 2))
  (1 . tail))

(deftest seq-nomat.mapcan-splices-not-copies
  ;; The pieces are spliced (NCONC), so the result shares their conses.
  (let* ((piece (list :a))
         (r (mapcan (lambda (x) (declare (ignore x)) piece) '(1))))
    (eq r piece))
  t)

(deftest seq-nomat.sort-lists
  (list (sort (list 3 1 2) #'<)
        (sort (list 3 1 2) #'>)
        (sort (list) #'<)
        (sort (list 1) #'<)
        (sort (list 2 2 1) #'<))
  ((1 2 3) (3 2 1) nil (1) (1 2 2)))

(deftest seq-nomat.sort-key-and-stability
  (list (sort (list (list 2 'b) (list 1 'a)) #'< :key #'car)
        (stable-sort (list (list 1 'a) (list 1 'b) (list 0 'c)) #'< :key #'car))
  (((1 a) (2 b)) ((0 c) (1 a) (1 b))))

(deftest seq-nomat.sort-vector-and-string
  (list (coerce (sort (vector 3 1 2) #'<) 'list) (sort (copy-seq "cba") #'char<))
  ((1 2 3) "abc"))

(deftest seq-nomat.sort-predicate-error-propagates
  (handler-case (sort (list 1 'a 2) #'<) (type-error () :type-error) (error () :other))
  :type-error)

;;; FIND / POSITION carry a 2-argument direct entry, so (find x list) does not
;;; build the args array the variadic path needs. Keyword shapes still take the
;;; general parser and both must agree.

(deftest seq-nomat.find-two-arg-and-keywords
  (list (find 7 '(1 2 3 4 5 6 7))
        (find 99 '(1 2 3))
        (find 7 #(1 7 3))
        (find #\b "abc")
        (find 1 '((1) (2)) :key #'car)
        (find "a" '("a" "b") :test #'string=)
        (find 2 '(1 2 3) :start 2)
        (find 2 '(2 1 2) :from-end t))
  (7 nil 7 #\b (1) "a" nil 2))

(deftest seq-nomat.position-two-arg-and-keywords
  (list (position 7 '(1 2 3 4 5 6 7))
        (position 99 '(1 2 3))
        (position 7 #(1 7 3))
        (position #\b "abc")
        (position 1 '((1) (2)) :key #'car)
        (position 2 '(2 1 2) :from-end t)
        (position 2 '(1 2 3) :start 2)
        (position 2 '(1 2 3) :end 1))
  (6 nil 1 1 0 2 nil nil))

(deftest seq-nomat.find-position-on-empty
  (list (find 1 '()) (position 1 '()) (find 1 #()) (position 1 ""))
  (nil nil nil nil))

(deftest seq-nomat.position-of-nil-element
  ;; FIND returns the element (NIL, indistinguishable); POSITION is the check.
  (list (find nil '(1 nil 2)) (position nil '(1 nil 2)))
  (nil 1))
