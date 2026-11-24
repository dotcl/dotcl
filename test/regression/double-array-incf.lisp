;;; A double-float accumulator INCF'd by a product of two AREFs of
;;; double-float arrays stays in a raw double: nothing is boxed per element.

(defun %dai-matmul (n)
  (let* ((a (make-array (list n n) :element-type 'double-float :initial-element 1.5d0))
         (b (make-array (list n n) :element-type 'double-float :initial-element 2.5d0))
         (c (make-array (list n n) :element-type 'double-float :initial-element 0d0)))
    (dotimes (i n)
      (dotimes (j n)
        (setf (aref a i j) (+ 1d0 (* 0.5d0 i) j))
        (let ((s 0d0))
          (dotimes (k n) (incf s (* (aref a i k) (aref b k j))))
          (setf (aref c i j) s))))
    (list (aref c 0 0) (aref c (min 3 (1- n)) (min 4 (1- n))) (aref c (1- n) (1- n)))))

(defun %dai-decf (n)
  (let ((a (make-array n :element-type 'double-float :initial-element 0.25d0))
        (s 10d0))
    (dotimes (i n) (decf s (aref a i)))
    (* s 1d0)))

(deftest-emitting-only double-array-incf.no-allocation
  (progn
    (%dai-matmul 10)
    (< (min-allocated-bytes (lambda () (%dai-matmul 100))) 400000))
  t)

(deftest double-array-incf.values
  (list (%dai-matmul 1) (%dai-matmul 7) (%dai-decf 8) (%dai-decf 0))
  ((2.5d0 2.5d0 2.5d0) (25.0d0 63.75d0 122.5d0) 8.0d0 10.0d0))
