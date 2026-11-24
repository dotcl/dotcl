;;; SUBSEQ of a vector copies the piece as a block when the elements are the vector's own array.
;;; Every other kind of vector, the bounds, the new vector's independence and its element type are
;;; as before.

(defun %svb (x) (coerce x 'list))

(deftest subseq-vector-block-copy.simple-vectors
  (let ((v (vector 0 1 2 3 4 5 6 7 8 9)))
    (list (%svb (subseq v 3 6)) (%svb (subseq v 0)) (%svb (subseq v 7)) (%svb (subseq v 4 4))
          (%svb (subseq v 0 10)) (%svb (subseq v 9))
          (let ((p (subseq v 2 5))) (setf (aref p 0) :x) (list (aref v 2) (aref p 0)))
          (eq (subseq v 0) v) (length (subseq (make-array 5000 :initial-element 1) 100 4900))))
  ((3 4 5) (0 1 2 3 4 5 6 7 8 9) (7 8 9) nil (0 1 2 3 4 5 6 7 8 9) (9) (2 :x) nil 4800))

(deftest subseq-vector-block-copy.other-vector-kinds
  (let* ((fp (make-array 10 :fill-pointer 6 :initial-contents '(0 1 2 3 4 5 6 7 8 9)))
         (adj (make-array 6 :adjustable t :initial-contents '(a b c d e f)))
         (base (vector 0 1 2 3 4 5 6 7 8 9))
         (disp (make-array 5 :displaced-to base :displaced-index-offset 3))
         (fix (make-array 6 :element-type 'fixnum :initial-contents '(1 2 3 4 5 6)))
         (dbl (make-array 4 :element-type 'double-float :initial-contents '(1d0 2d0 3d0 4d0)))
         (bits (make-array 8 :element-type 'bit :initial-contents '(1 0 1 1 0 0 1 0))))
    (list (%svb (subseq fp 2 5)) (%svb (subseq fp 4)) (handler-case (subseq fp 2 8) (error () :error))
          (%svb (subseq adj 1 4)) (%svb (subseq disp 1 4)) (%svb (subseq fix 2 5))
          (array-element-type (subseq fix 2 5)) (%svb (subseq dbl 1 3)) (%svb (subseq bits 2 6))))
  ((2 3 4) (4 5) :error (b c d) (4 5 6) (3 4 5) fixnum (2d0 3d0) (1 1 0 0)))

(deftest subseq-vector-block-copy.errors
  (let ((v (vector 1 2 3)))
    (list (handler-case (subseq v 2 1) (error () :error)) (handler-case (subseq v 0 4) (error () :error))
          (handler-case (subseq v 4) (error () :error)) (handler-case (subseq v -1) (error () :error))
          (handler-case (subseq v 1.5) (error () :error)) (%svb (subseq v 3))))
  (:error :error :error :error :error nil))
