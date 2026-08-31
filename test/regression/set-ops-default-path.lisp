;;; The set functions and TREE-EQUAL stopped paying per element to pass their
;;; keyword arguments along.
;;;
;;; UNION, INTERSECTION, SET-DIFFERENCE, SET-EXCLUSIVE-OR and SUBSETP all asked
;;; "is this element in the other list" through %SET-MEMBER, which takes five
;;; arguments -- and a five-argument call to a named function allocates an array
;;; to record its call frame (the frame keeps its first four arguments in inline
;;; slots and has nowhere to put a fifth). (subsetp l5 l5), which builds nothing
;;; at all, cost 368 bytes.
;;;
;;; With no :TEST, :TEST-NOT or :KEY the question is exactly what MEMBER
;;; answers, and MEMBER is a C# builtin with a two-argument direct entry.
;;; Supplied-p flags decide, so an explicit :TEST #'EQL still takes the general
;;; path rather than being second-guessed.
;;;
;;; TREE-EQUAL passed :TEST and :TEST-NOT down at every node, so every cons in
;;; the tree cost a keyword argument list: 768 bytes to compare two 5-element
;;; lists. It now carries them positionally through a four-parameter helper.
;;;
;;; subsetp 368 -> 48, intersection 528 -> 208, tree-equal 768 -> 48.
;;;
;;; Every expected value here was taken from SBCL.

(defun sodp-sorted (l) (sort (copy-list l) #'string< :key #'princ-to-string))

(deftest sodp.default-path
  (list (sodp-sorted (union '(1 2 3 4) '(3 4 5 6)))
        (sodp-sorted (intersection '(1 2 3 4) '(3 4 5 6)))
        (sodp-sorted (set-difference '(1 2 3 4) '(3 4 5 6)))
        (sodp-sorted (set-exclusive-or '(1 2 3 4) '(3 4 5 6)))
        (subsetp '(1 2) '(1 2 3 4))
        (subsetp '(1 2 3 4) '(1 2)))
  ((1 2 3 4 5 6) (3 4) (1 2) (1 2 5 6) t nil))

(deftest sodp.empty-arguments
  (list (sodp-sorted (union '() '(3 4)))
        (sodp-sorted (union '(1 2) '()))
        (intersection '(1 2) '())
        (intersection '() '(1 2))
        (sodp-sorted (set-difference '(1 2) '()))
        (subsetp '() '(1 2)))
  ((3 4) (1 2) nil nil (1 2) t))

;;; The destructive names take the same path.
(deftest sodp.destructive-names
  (list (sodp-sorted (nunion (list 1 2 3 4) (list 3 4 5 6)))
        (sodp-sorted (nintersection (list 1 2 3 4) (list 3 4 5 6)))
        (sodp-sorted (nset-difference (list 1 2 3 4) (list 3 4 5 6)))
        (sodp-sorted (nset-exclusive-or (list 1 2 3 4) (list 3 4 5 6))))
  ((1 2 3 4 5 6) (3 4) (1 2) (1 2 5 6)))

(deftest sodp.test-keyword
  (list (sodp-sorted (union '("a" "b") '("b" "c") :test #'string=))
        (sodp-sorted (intersection '("a" "b") '("b" "c") :test #'string=))
        (sodp-sorted (set-difference '("a" "b") '("b" "c") :test #'string=))
        (subsetp '("a") '("a" "b") :test #'string=))
  (("a" "b" "c") ("b") ("a") t))

(deftest sodp.key-keyword
  (list (sodp-sorted (intersection '((1) (2)) '((2) (3)) :key #'car))
        (sodp-sorted (set-difference '((1) (2)) '((2) (3)) :key #'car))
        (sodp-sorted (union '((1)) '((2)) :key #'car))
        (subsetp '((1)) '((1) (2)) :key #'car))
  (((2)) ((1)) ((1) (2)) t))

(deftest sodp.test-not-keyword
  (list (sodp-sorted (intersection '(1 2 3 4) '(3 4 5 6) :test-not #'eql))
        (sodp-sorted (set-difference '(1 2 3 4) '(3 4 5 6) :test-not #'eql))
        (subsetp '(1) '(2) :test-not #'eql))
  ((1 2 3 4) nil t))

;;; An explicitly supplied :TEST #'EQL or :KEY NIL means the same thing as the
;;; default, and must still answer the same.
(deftest sodp.explicit-defaults
  (list (sodp-sorted (intersection '(1 2 3 4) '(3 4 5 6) :test #'eql))
        (sodp-sorted (intersection '(1 2 3 4) '(3 4 5 6) :key nil))
        (sodp-sorted (union '(1 2) '(2 3) :test #'= :key #'identity)))
  ((3 4) (3 4) (1 2 3)))

(deftest sodp.tree-equal
  (list (tree-equal '(1 (2 3)) '(1 (2 3)))
        (tree-equal '(1 (2 3)) '(1 (2 4)))
        (tree-equal 1 1)
        (tree-equal 1 2)
        (tree-equal '(1 . 2) '(1 . 2))
        (tree-equal '(1) 1)
        (tree-equal 1 '(1))
        (tree-equal nil nil))
  (t nil t nil t nil nil t))

;;; :TEST-NOT is satisfied when the function returns FALSE. TREE-EQUAL had a
;;; double negation there, so it answered T exactly when it should have answered
;;; NIL: (tree-equal '(1) '(1) :test-not #'eql) was T, and SBCL says NIL. The
;;; leaves of '(1) are 1 and the terminating NIL, and (eql nil nil) is true, so
;;; no list can be TREE-EQUAL to another under :test-not #'eql.
;;;
;;; :TEST #'= reaches that same terminating NIL and hands it to =, which is a
;;; type error -- in SBCL too.
(deftest sodp.tree-equal-keywords
  (list (tree-equal '("a") '("a") :test #'string=)
        (tree-equal '(1) '(1) :test-not #'eql)
        (tree-equal '(1) '(2) :test-not #'eql)
        (tree-equal 1 1 :test-not #'eql)
        (tree-equal 1 2 :test-not #'eql)
        (tree-equal '(1 . 2) '(3 . 4) :test-not #'eql)
        (tree-equal '(1 . 2) '(1 . 4) :test-not #'eql)
        (tree-equal nil nil :test-not #'eql)
        (tree-equal '(1 . 2) '(1 . 2) :test-not #'/=)
        (tree-equal "a" "a" :test-not #'string/=)
        (handler-case (progn (tree-equal '(1 2) '(1 2) :test #'=) :no-error)
          (type-error () :type-error)))
  (t nil nil nil t t nil nil t t :type-error))

;;; A dotted second argument is a type error, and the offending object is the
;;; atom that terminated the list -- not the list itself, which IS of type LIST
;;; and would make the condition describe something that is not wrong.
;;;
;;; MEMBER is where this is noticed. Its keyword-taking entries always checked;
;;; the two-argument entry the compiler emits for (member x l) did not, so it
;;; walked off a dotted list and answered NIL. Routing the set functions through
;;; MEMBER made that visible.
(deftest sodp.dotted-second-argument
  (flet ((kind (thunk)
           (handler-case (progn (funcall thunk) :no-error)
             (type-error (e) (list (type-error-datum e)
                                   (type-error-expected-type e)
                                   (typep (type-error-datum e)
                                          (type-error-expected-type e)))))))
    (list (kind (lambda () (member 'z '(d e f . g))))
          (kind (lambda () (subsetp (list 1 2 3) (list* 4 5 6))))
          (kind (lambda () (intersection (list 1 2 3) (list* 4 5 6))))
          (kind (lambda () (union (list 1 2 3) (list* 4 5 6))))
          (kind (lambda () (set-difference (list 1 2 3) (list* 4 5 6))))
          (kind (lambda () (set-exclusive-or (list 1 2 3) (list* 4 5 6))))))
  ((g list nil) (6 list nil) (6 list nil) (6 list nil) (6 list nil) (6 list nil)))

;;; Finding the item before the dotted tail is not an error, and an empty first
;;; argument never looks at the second at all.
(deftest sodp.dotted-second-argument-not-reached
  (list (member 'd '(d e f . g))
        (intersection '() (list* 4 5 6))
        (subsetp '() (list* 4 5 6))
        (set-difference '() (list* 4 5 6)))
  ((d e f . g) nil t nil))
