;;; The second argument of UNION, INTERSECTION, SET-DIFFERENCE, SET-EXCLUSIVE-OR and SUBSETP is checked
;;; to be a list even when the first is empty and the operation never looks at it.

(defun %sol-class (thunk) (handler-case (funcall thunk) (type-error () :type-error)))

(deftest set-operations-list-argument.rejected
  (list (%sol-class (lambda () (union nil 5)))
        (%sol-class (lambda () (intersection nil 5)))
        (%sol-class (lambda () (set-difference nil 5)))
        (%sol-class (lambda () (set-exclusive-or nil 5)))
        (%sol-class (lambda () (subsetp nil 5)))
        (%sol-class (lambda () (nintersection nil "abc")))
        (%sol-class (lambda () (intersection '(1) 5))))
  (:type-error :type-error :type-error :type-error :type-error :type-error :type-error))

(deftest set-operations-list-argument.accepted
  (list (union nil nil) (union nil '(1)) (union '(1) nil)
        (intersection nil nil) (intersection nil '(1)) (intersection '(1) nil)
        (set-difference nil nil) (set-difference nil '(1)) (set-difference '(1) nil)
        (set-exclusive-or nil nil) (subsetp nil nil) (subsetp nil '(1))
        (sort (union '(1 2) '(2 3)) #'<) (intersection '(1 2) '(2 3)) (set-difference '(1 2) '(2 3))
        (intersection '(1 2) '(2 3) :test #'eql :key #'identity))
  (nil (1) (1) nil nil nil nil nil (1) nil t t (1 2 3) (2) (1) (2)))
