;;; DELETE on a vector with a fill pointer works in place, as on SBCL: the
;;; survivors move down, the fill pointer drops, and the result is the vector
;;; itself, still with its fill pointer. It returned a fresh simple vector.
;;; event-emitter stores (setf (gethash ...) (delete ...)) and then calls
;;; VECTOR-PUSH-EXTEND on it, which failed with "no fill pointer". A vector
;;; without a fill pointer still gets a fresh result. Expected values are
;;; SBCL 2.6.8's answers.

(deftest delete-fill-pointer-vector.adjustable
  (let* ((v (make-array 3 :fill-pointer 3 :adjustable t :initial-contents (list 1 2 3)))
         (r (delete 2 v)))
    (list (eq v r) (coerce r 'list) (fill-pointer v) (aref v 1)
          (progn (vector-push-extend 9 r) (coerce r 'list))))
  (t (1 3) 2 3 (1 3 9)))

(deftest delete-fill-pointer-vector.not-adjustable
  (let* ((v (make-array 4 :fill-pointer 4 :initial-contents (list 1 2 3 2)))
         (r (delete 2 v :from-end t :count 1)))
    (list (eq v r) (coerce r 'list) (fill-pointer v)))
  (t (1 2 3) 3))

(deftest delete-fill-pointer-vector.string
  (let* ((s (make-array 4 :element-type 'character :fill-pointer 4 :initial-contents "abcb"))
         (r (delete #\b s)))
    (list (eq s r) r (fill-pointer s)))
  (t "ac" 2))

(deftest delete-fill-pointer-vector.delete-if-start
  (let* ((v (make-array 5 :fill-pointer 5 :initial-contents (list 1 2 3 4 5)))
         (r (delete-if #'evenp v :start 2)))
    (list (eq v r) (coerce r 'list)))
  (t (1 2 3 5)))

(deftest delete-fill-pointer-vector.nothing-removed
  (let ((v (make-array 2 :fill-pointer 2 :initial-contents (list 1 2))))
    (list (eq v (delete 7 v)) (fill-pointer v)))
  (t 2))

;; No fill pointer: a new vector, the argument untouched in length.
(deftest delete-fill-pointer-vector.simple
  (let* ((v (vector 1 2 3)) (r (delete 2 v)))
    (list (eq v r) (coerce r 'list) (length v)))
  (nil (1 3) 3))
