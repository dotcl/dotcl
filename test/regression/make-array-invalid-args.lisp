;;; MAKE-ARRAY: :DISPLACED-TO NIL is the same as leaving it out; a displacement offset without a
;;; target, a displaced array with initial contents, and a dotted dimension list are errors.

(defun %mai-class (thunk)
  (handler-case (funcall thunk) (type-error () :type-error) (error () :error)))

(deftest make-array-invalid-args.displaced-to-nil
  (list (length (make-array 3 :displaced-to nil))
        (length (make-array 3 :displaced-to nil :displaced-index-offset 0))
        (aref (make-array 3 :displaced-to nil :initial-element 7) 2))
  (3 3 7))

(deftest make-array-invalid-args.rejected
  (list (%mai-class (lambda () (make-array 3 :displaced-index-offset 1)))
        (%mai-class (lambda () (make-array 3 :displaced-to (make-array 5) :initial-element 0)))
        (%mai-class (lambda () (make-array 3 :displaced-to (make-array 5) :initial-contents '(1 2 3))))
        (%mai-class (lambda () (make-array (cons 2 3)))))
  (:error :error :error :type-error))

(deftest make-array-invalid-args.valid-displaced
  (let* ((base (make-array 5 :initial-contents '(0 1 2 3 4)))
         (d (make-array 3 :displaced-to base :displaced-index-offset 2)))
    (list (aref d 0) (aref d 2) (array-dimensions (make-array '(2 3) :initial-element 1))))
  (2 4 (2 3)))
