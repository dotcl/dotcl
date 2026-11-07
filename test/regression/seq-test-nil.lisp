;;; :TEST NIL and :TEST-NOT NIL mean "not given" (the default EQL), as in SBCL,
;;; so code that forwards an optional test keeps working (FIND, POSITION,
;;; TREE-EQUAL and friends; SBCL also accepts it there).

(deftest seq-test-nil.find-position
  (list (find 1 '(1 2) :test nil)
        (find 1 '(1 2) :test-not nil)
        (position 2 '(1 2) :test nil)
        (position 2 '(1 2) :test-not nil)
        (find 1 #(1 2) :test nil)
        (find #\b "abc" :test nil)
        (position #\b "abc" :test nil))
  (1 1 1 1 1 #\b 1))

(deftest seq-test-nil.list-operations
  (list (member 2 '(1 2 3) :test nil)
        (assoc 2 '((1 . a) (2 . b)) :test nil)
        (count 1 '(1 2 1) :test nil)
        (remove 1 '(1 2 1) :test nil)
        (tree-equal '(1 (2)) '(1 (2)) :test nil)
        (tree-equal '(1 (2)) '(1 (3)) :test nil))
  ((2 3) (2 . b) 2 (2) t nil))
