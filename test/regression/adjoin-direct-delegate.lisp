;;; ADJOIN moved from a &key Lisp defun to C# with 2/4/6-arg direct entries
;;; (PUSHNEW expands into the keyworded shapes). The entries decode their own
;;; keyword pairs, so :test / :test-not / :key must agree with the args-array
;;; path — and the key must be applied to the ITEM for the comparison while the
;;; element consed on stays the original item.

(deftest adjoin-direct.absent-eql
  (adjoin 3 (list 1 2))
  (3 1 2))

(deftest adjoin-direct.present-eql
  (adjoin 2 (list 1 2))
  (1 2))

(deftest adjoin-direct.empty-list
  (adjoin 1 nil)
  (1))

(deftest adjoin-direct.test-equal-present
  (adjoin "a" (list "a" "b") :test #'equal)
  ("a" "b"))

(deftest adjoin-direct.test-equal-absent
  (adjoin "c" (list "a" "b") :test #'equal)
  ("c" "a" "b"))

(deftest adjoin-direct.eql-does-not-match-strings
  (length (adjoin "a" (list "a")))
  2)

;;; :test-not matches when the test is false, so 2 counts as "already present"
;;; and nothing is consed on; with a list of only 1 nothing matches and it is.
(deftest adjoin-direct.test-not-present
  (adjoin 1 (list 1 2) :test-not #'eql)
  (1 2))

(deftest adjoin-direct.test-not-absent
  (adjoin 1 (list 1) :test-not #'eql)
  (1 1))

;;; :key applies to the item as well as to the elements, but the item is consed
;;; on unchanged.
(deftest adjoin-direct.key-present
  (adjoin (cons 1 'new) (list (cons 1 'old)) :key #'car)
  ((1 . old)))

(deftest adjoin-direct.key-absent-conses-original-item
  (adjoin (cons 2 'new) (list (cons 1 'old)) :key #'car)
  ((2 . new) (1 . old)))

(deftest adjoin-direct.key-and-test
  (adjoin (cons "A" 1) (list (cons "a" 0)) :key #'car :test #'string-equal)
  (("a" . 0)))

(deftest adjoin-direct.symbol-designator-test
  (adjoin "a" (list "a" "b") :test 'equal)
  ("a" "b"))

;;; PUSHNEW rides on the same entries.
(deftest adjoin-direct.pushnew-test
  (let ((l (list "a")))
    (pushnew "a" l :test #'equal)
    (pushnew "b" l :test #'equal)
    l)
  ("b" "a"))

(deftest adjoin-direct.pushnew-key
  (let ((l (list (cons 1 'old))))
    (pushnew (cons 1 'new) l :key #'car)
    (pushnew (cons 2 'new) l :key #'car)
    l)
  ((2 . new) (1 . old)))

;;; APPLY takes the args-array path — same answer.
(deftest adjoin-direct.apply-matches-direct
  (equal (adjoin "c" (list "a") :test #'equal)
         (apply #'adjoin "c" (list (list "a") :test #'equal)))
  t)

(deftest adjoin-direct.unknown-keyword-errors
  (handler-case (progn (adjoin 1 (list 2) :bogus t) :no-error)
    (error () :error))
  :error)

(deftest adjoin-direct.not-a-list-errors
  (handler-case (progn (adjoin 1 5) :no-error)
    (error () :error))
  :error)
