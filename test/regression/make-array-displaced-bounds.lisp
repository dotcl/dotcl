;;; A displaced array has to fit inside its target. MAKE-ARRAY and ADJUST-ARRAY
;;; signal an ERROR when the offset plus the new total size runs past the end of
;;; the target, instead of making an array whose elements cannot be read.

(defun %madb-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(deftest make-array-displaced-bounds.too-long
  (let ((b (make-array 6 :initial-contents '(0 1 2 3 4 5))))
    (%madb-signals-p (lambda () (make-array 9 :displaced-to b))))
  t)

(deftest make-array-displaced-bounds.offset-plus-size
  (let ((b (make-array 6 :initial-contents '(0 1 2 3 4 5))))
    (list (%madb-signals-p (lambda () (make-array 4 :displaced-to b :displaced-index-offset 3)))
          (%madb-signals-p (lambda () (make-array 1 :displaced-to b :displaced-index-offset 7)))))
  (t t))

(deftest make-array-displaced-bounds.exact-fit
  (let ((b (make-array 6 :initial-contents '(0 1 2 3 4 5))))
    (list (coerce (make-array 3 :displaced-to b :displaced-index-offset 3) 'list)
          (length (make-array 0 :displaced-to b :displaced-index-offset 6))))
  ((3 4 5) 0))

(deftest make-array-displaced-bounds.multi-dimensional-target
  (let ((a (make-array '(3 2) :initial-contents '((0 1) (2 3) (4 5)))))
    (list (coerce (make-array 2 :displaced-to a :displaced-index-offset 4) 'list)
          (%madb-signals-p (lambda () (make-array 3 :displaced-to a :displaced-index-offset 4)))))
  ((4 5) t))

(deftest make-array-displaced-bounds.adjust-array
  (let ((b (make-array 6 :initial-contents '(0 1 2 3 4 5)))
        (v (make-array 2 :adjustable t)))
    (list (%madb-signals-p (lambda () (adjust-array v 5 :displaced-to b :displaced-index-offset 2)))
          (coerce (adjust-array v 4 :displaced-to b :displaced-index-offset 2) 'list)))
  (t (2 3 4 5)))
