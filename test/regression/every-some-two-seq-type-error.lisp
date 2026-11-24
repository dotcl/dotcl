;;; EVERY / SOME with two sequences signal a TYPE-ERROR for an argument that is not a
;;; sequence even when the other sequence is empty.

(defun %es2-type-error-p (thunk)
  (handler-case (progn (funcall thunk) nil) (type-error () t)))

(deftest every-some-two-seq-type-error.every
  (list (%es2-type-error-p (lambda () (every #'eq nil 0)))
        (%es2-type-error-p (lambda () (every #'eq 0 nil)))
        (%es2-type-error-p (lambda () (every #'eq '(1) #\a))))
  (t t t))

(deftest every-some-two-seq-type-error.some
  (list (%es2-type-error-p (lambda () (some #'eq nil 0)))
        (%es2-type-error-p (lambda () (some #'eq 0 nil)))
        (%es2-type-error-p (lambda () (some #'eq '(1) #\a))))
  (t t t))

(deftest every-some-two-seq-type-error.valid-unchanged
  (list (every #'< '(1 2) #(2 3)) (every #'< nil '(1)) (some #'eql "ab" '(#\b #\a)) (some #'eql nil nil))
  (t t nil nil))
