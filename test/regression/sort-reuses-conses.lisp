;;; SORT on a list reuses the list it was given.
;;;
;;; ANSI lets SORT destroy its argument, and the vector and string paths here
;;; already wrote their result back in place -- but the list path consed a whole
;;; new list from the sorted array and dropped the cells it had just walked.
;;; That is N conses per sort on top of the array the comparator needs.
;;;
;;; The comparator was a lambda as well, so every call built a display class for
;;; the captured predicate and key and a delegate over it before the first
;;; comparison ran.
;;;
;;; (sort (list 3 1 2) #'<) 376 -> 248 bytes.
;;;
;;; Every expected value here was taken from SBCL.

(deftest sort-rc.list
  (list (sort (list 3 1 2) #'<)
        (sort (list) #'<)
        (sort (list 1) #'<)
        (sort (list 5 4 3 2 1) #'<)
        (sort (list 1 2 3) #'>))
  ((1 2 3) nil (1) (1 2 3 4 5) (3 2 1)))

;;; The sorted list comes back through the cells that were passed in, so the
;;; caller's variable still names a sorted list. SBCL relinks instead, which
;;; leaves the variable pointing at a suffix -- code that relies on either is
;;; relying on undefined behaviour, but nothing here should crash or lose
;;; elements.
(deftest sort-rc.result-is-sorted-through-the-same-cells
  (let* ((l (list 3 1 2)) (r (sort l #'<)))
    (list r (and (member (car l) r) t)))
  ((1 2 3) t))

(deftest sort-rc.setq-idiom
  (let ((l (list 3 1 2))) (setq l (sort l #'<)) l)
  (1 2 3))

(deftest sort-rc.key
  (list (sort (list (cons 1 'a) (cons 3 'b) (cons 2 'c)) #'< :key #'car)
        (sort (list 2 1 3) #'< :key nil))
  (((1 . a) (2 . c) (3 . b)) (1 2 3)))

;;; STABLE-SORT keeps the order of elements the predicate calls equal.
(deftest sort-rc.stable
  (list (stable-sort (list (cons 1 'a) (cons 1 'b) (cons 0 'c)) #'< :key #'car)
        (stable-sort (list 3 1 2) #'<))
  (((0 . c) (1 . a) (1 . b)) (1 2 3)))

(deftest sort-rc.vector-and-string
  (list (coerce (sort (copy-seq #(3 1 2)) #'<) 'list)
        (sort (copy-seq "cba") #'char<))
  ((1 2 3) "abc"))

;;; The comparator can call SORT again: the state it needs is in the comparer
;;; object, not in anything shared.
(deftest sort-rc.reentrant
  (sort (list 3 1 2) (lambda (a b) (< (car (sort (list a) #'<)) b)))
  (1 2 3))

;;; An error out of the predicate still reaches the handler rather than
;;; surfacing as the .NET exception Array.Sort wraps it in.
(deftest sort-rc.predicate-error
  (handler-case (sort (list 3 1 2) (lambda (a b) (declare (ignore a b))
                                     (error "from the predicate")))
    (simple-error (e) (princ-to-string e)))
  "from the predicate")
