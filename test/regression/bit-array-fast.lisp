;;; SBIT / BIT / AREF on a simple bit vector read and store the packed word in place, and a bit
;;; read with a fixnum subscript is a raw 0 or 1. Everything else (displaced, fill-pointer,
;;; adjustable, two-dimensional, a value that is not 0 or 1, an index out of range) is what it was.

(defun %baf-sbit (v i) (declare (fixnum i)) (sbit v i))
(defun %baf-bit (v i) (declare (fixnum i)) (bit v i))
(defun %baf-aref (v i) (declare (fixnum i)) (aref v i))
(defun %baf-set-sbit (v i x) (declare (fixnum i)) (setf (sbit v i) x))
(defun %baf-set-bit (v i x) (declare (fixnum i)) (setf (bit v i) x))
(defun %baf-set-aref (v i x) (declare (fixnum i)) (setf (aref v i) x))
(defun %baf-err (thunk) (handler-case (funcall thunk) (error (e) (type-of e))))

(defun %baf-sieve (n)
  (declare (type (integer 0 100000) n))
  (let ((v (make-array (1+ n) :element-type 'bit :initial-element 0)) (count 0))
    (declare (fixnum count))
    (setf (sbit v 0) 1 (sbit v 1) 1)
    (loop for i from 2 to n do
      (when (zerop (sbit v i))
        (incf count)
        (loop for j from (* i i) to n by i do (setf (sbit v j) 1))))
    count))

(defun %baf-sieve-into (v n)
  (declare (type (simple-bit-vector) v) (type (integer 0 100000000) n))
  (loop for i from 2 to n do
    (when (zerop (sbit v i))
      (loop for j from (* i i) to n by i do (setf (sbit v j) 1))))
  v)

(deftest bit-array-fast.simple-vector
  (let ((v (make-array 130 :element-type 'bit :initial-element 0)))
    (list (%baf-set-sbit v 0 1) (%baf-set-sbit v 63 1) (%baf-set-sbit v 64 1) (%baf-set-sbit v 129 1)
          (%baf-sbit v 0) (%baf-sbit v 1) (%baf-sbit v 63) (%baf-sbit v 64) (%baf-sbit v 129)
          (%baf-bit v 129) (%baf-aref v 63) (%baf-set-bit v 63 0) (%baf-sbit v 63) (%baf-set-aref v 5 1)
          (%baf-aref v 5) (count 1 v) (%baf-sieve 100) (%baf-sieve 1) (%baf-sieve 2)
          (%baf-sieve 100000)))
  (1 1 1 1 1 0 1 1 1 1 1 0 0 1 1 4 25 0 1 9592))

(deftest bit-array-fast.not-zero-or-one
  (let ((v (make-array 8 :element-type 'bit :initial-element 0)))
    (list (%baf-err (lambda () (%baf-set-sbit v 0 2)))
          (%baf-err (lambda () (%baf-set-sbit v 0 -1)))
          (%baf-err (lambda () (%baf-set-sbit v 0 nil)))
          (%baf-err (lambda () (%baf-set-sbit v 0 1.0)))
          (%baf-err (lambda () (%baf-set-aref v 0 2)))
          (%baf-err (lambda () (%baf-set-sbit v 8 1)))
          (%baf-err (lambda () (%baf-set-sbit v -1 1)))
          (%baf-err (lambda () (%baf-sbit v 8)))
          (%baf-err (lambda () (%baf-sbit v -1)))
          (%baf-err (lambda () (%baf-bit v most-positive-fixnum)))
          (%baf-sbit v 0)))
  (type-error type-error type-error type-error type-error type-error type-error type-error
   type-error type-error 0))

(deftest bit-array-fast.other-kinds-of-bit-array
  (let ((fp (make-array 8 :element-type 'bit :fill-pointer 4 :initial-element 0))
        (adj (make-array 8 :element-type 'bit :adjustable t :initial-element 0))
        (base (make-array 16 :element-type 'bit :initial-element 0))
        (m (make-array '(3 4) :element-type 'bit :initial-element 0)))
    (let ((disp (make-array 8 :element-type 'bit :displaced-to base :displaced-index-offset 4)))
      (%baf-set-bit fp 2 1) (%baf-set-bit adj 3 1) (%baf-set-bit disp 1 1)
      (setf (bit m 1 2) 1 (aref m 2 3) 1)
      (list (%baf-bit fp 2) (%baf-aref fp 6) (%baf-bit adj 3) (%baf-bit disp 1) (%baf-bit base 5)
            (%baf-err (lambda () (%baf-sbit fp 2)))
            (%baf-err (lambda () (%baf-sbit adj 3)))
            (%baf-err (lambda () (%baf-sbit disp 1)))
            (%baf-err (lambda () (%baf-set-sbit adj 3 1)))
            (bit m 1 2) (aref m 2 3) (aref m 0 0) (%baf-err (lambda () (%baf-bit m 1)))
            (%baf-err (lambda () (%baf-sbit "abc" 1))) (%baf-err (lambda () (%baf-bit (vector 1 0) 1)))
            (%baf-err (lambda () (%baf-bit nil 0))))))
  (1 0 1 1 1 type-error type-error type-error type-error 1 1 0 program-error type-error type-error type-error))

(deftest bit-array-fast.setf-value-and-bit-operations
  (let ((a (make-array 5 :element-type 'bit :initial-contents '(1 0 1 0 1)))
        (b (make-array 5 :element-type 'bit :initial-contents '(1 1 0 0 1))))
    (list (setf (sbit a 1) 1) (setf (bit b 2) 1) (coerce a 'list) (coerce b 'list)
          (coerce (bit-and a b) 'list) (coerce (bit-xor a b) 'list) (coerce (bit-not a) 'list)
          (coerce (bit-ior a b a) 'list)
          (let ((c (make-array 5 :element-type 'bit :initial-element 0)))
            (bit-and a b c) (coerce c 'list))
          (incf (sbit a 0) 0) (let ((x 0)) (list (incf x) (setf (sbit a 4) (- 1 (sbit a 4))) (sbit a 4)))))
  (1 1 (1 1 1 0 1) (1 1 1 0 1) (1 1 1 0 1) (0 0 0 0 0) (0 0 0 1 0) (1 1 1 0 1) (1 1 1 0 1)
   1 (1 0 0)))

(deftest bit-array-fast.index-expressions-are-evaluated-once-in-order
  (let ((v (make-array 4 :element-type 'bit :initial-element 0)) (log '()) (i 0))
    (declare (fixnum i))
    (setf (sbit (progn (push :a log) v) (progn (push :i log) (incf i) i)) (progn (push :x log) 1))
    (list (reverse log) (sbit (progn (push :a2 log) v) (progn (push :i2 log) 1)) (reverse log) i (coerce v 'list)))
  ((:a :i :x) 1 (:a :i :x :a2 :i2) 1 (0 1 0 0)))

(deftest-compiled-only bit-array-fast.no-allocation
  (let ((v (make-array 1000001 :element-type 'bit :initial-element 0)))
    (list (< (min-allocated-bytes (lambda () (fill v 0) (%baf-sieve-into v 1000000))) 20000)))
  (t))
