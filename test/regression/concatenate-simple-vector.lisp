;;; (CONCATENATE 'SIMPLE-VECTOR / 'VECTOR ...) of simple vectors and NILs, and every other shape of
;;; argument (strings, lists, fill-pointer / adjustable / displaced / specialized vectors, a result type
;;; with an element type or a length), still answer as before.

(defun %csv-cat3 (a b c) (concatenate 'simple-vector a b c))
(defun %csv-cat2 (a b) (concatenate 'vector a b))

(defun %csv-lists (xs) (mapcar (lambda (x) (if (vectorp x) (coerce x 'list) x)) xs))

(deftest concatenate-simple-vector.answers
  (%csv-lists (list (%csv-cat3 (vector 1 2) (vector 3) (vector 4 5))
        (%csv-cat3 nil (vector 1) nil)
        (%csv-cat2 (vector) (vector))
        (%csv-cat2 (vector 1) '(2 3))
        (%csv-cat2 (vector #\a) "bc")
        (%csv-cat2 (make-array 2 :initial-contents '(1 2) :fill-pointer 1) (vector 9))
        (%csv-cat2 (make-array 3 :initial-contents '(1 2 3) :adjustable t) (vector 9))
        (%csv-cat2 (make-array 2 :displaced-to (vector 7 8 9) :displaced-index-offset 1) (vector 0))
        (%csv-cat2 (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(5 6)) (vector 1))
        (type-of (%csv-cat3 (vector 1) (vector 2) nil))
        (simple-vector-p (%csv-cat2 (vector 1) (vector 2)))
        (concatenate '(vector (unsigned-byte 8)) (vector 1 2) (vector 3))
        (handler-case (concatenate '(vector * 3) (vector 1) (vector 2)) (error () :error))
        (let ((a (vector 1 2)) (r (%csv-cat2 (vector 1 2) (vector 3)))) (setf (svref r 0) :x) a)))
  ((1 2 3 4 5) (1) () (1 2 3) (#\a #\b #\c) (1 9) (1 2 3 9) (8 9 0) (5 6 1)
   (simple-vector 2) t (1 2 3) :error (1 2)))

(defun %csv-loop (l r n) (declare (fixnum n)) (let ((x nil)) (dotimes (i n) (setq x (concatenate 'simple-vector l (vector i) r))) x))

(deftest-compiled-only concatenate-simple-vector.fast
  (let ((l (vector 1 2 3 4 5 6 7 8)) (r (vector 10 11 12)))
    (%csv-loop l r 10)
    ;; the result is 24 + 8 * 12 bytes of array + the LispVector (about 100 bytes) + (VECTOR I): the
    ;; general path made a second array and a list of the pieces
    (< (/ (min-allocated-bytes (lambda () (%csv-loop l r 100000))) 100000.0) 400))
  t)
