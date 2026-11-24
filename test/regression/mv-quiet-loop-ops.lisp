;;; A loop of structure-slot, array and raw-bits operations leaves the multiple-values channel as
;;; the one reset in front of the loop made it: the resets inside the loop are not repeated, and
;;; a values call made before the loop does not show through after it.

(defstruct (mvq (:constructor make-mvq (x y z m)))
  (x 0d0 :type double-float) (y 0d0 :type double-float) (z 0d0 :type double-float) (m 0 :type fixnum))

(defun %mvq-struct (p n)
  (declare (fixnum n))
  (values 1 2 3)
  (let ((last nil))
    (dotimes (i n)
      (setq last (setf (mvq-x p) (+ (mvq-x p) 1d0)))
      (setf (mvq-y p) (+ (mvq-y p) (mvq-x p)))
      (setf (mvq-z p) (+ (mvq-z p) (mvq-y p))))
    last))

(defun %mvq-double-array (a n)
  (declare (type (simple-array double-float (*)) a) (fixnum n))
  (floor 7 2)
  (let ((last 0d0))
    (declare (double-float last))
    (dotimes (i n)
      (setf (aref a (mod i 4)) (+ (aref a (mod i 4)) 1d0))
      (setq last (aref a (mod i 4))))
    last))

(defun %mvq-vector (v n)
  (declare (type simple-vector v) (fixnum n))
  (values :a :b)
  (let ((last nil))
    (dotimes (i n)
      (setq last (aref v (mod i 3))))
    last))

(defun %mvq-struct-in-vector (v n)
  (declare (type simple-vector v) (fixnum n))
  (values 10 20)
  (let ((acc 0d0))
    (declare (double-float acc))
    (dotimes (i n)
      (let ((p (aref v (mod i 2))))
        (setf (mvq-x p) (+ (mvq-x p) 1d0))
        (setq acc (+ acc (mvq-x p) (mvq-y p) (mvq-z p)))))
    acc))

(deftest mv-quiet-loop-ops.struct-slots
  (let ((p (make-mvq 0d0 0d0 0d0 0)))
    (list (multiple-value-list (%mvq-struct p 3))
          (multiple-value-list (progn (values 7 8 9) (%mvq-struct p 0)))
          (multiple-value-list (progn (values 7 8 9) (%mvq-struct p 1)))
          (nth-value 1 (%mvq-struct p 2))
          (multiple-value-bind (a b) (%mvq-struct p 1) (list a b))))
  ((3d0) (nil) (4d0) nil (7d0 nil)))

(deftest mv-quiet-loop-ops.double-array
  (let ((a (make-array 4 :element-type 'double-float :initial-element 0d0)))
    (list (multiple-value-list (%mvq-double-array a 4))
          (multiple-value-list (%mvq-double-array a 0))
          (nth-value 1 (%mvq-double-array a 5))))
  ((1d0) (0d0) nil))

(deftest mv-quiet-loop-ops.simple-vector
  (let ((v (vector 1 2 3)))
    (list (multiple-value-list (%mvq-vector v 4))
          (multiple-value-list (%mvq-vector v 0))
          (nth-value 1 (%mvq-vector v 3))))
  ((1) (nil) nil))

(deftest mv-quiet-loop-ops.struct-in-vector
  (let ((v (vector (make-mvq 0d0 0d0 0d0 0) (make-mvq 0d0 0d0 0d0 0))))
    (list (multiple-value-list (%mvq-struct-in-vector v 2))
          (multiple-value-list (%mvq-struct-in-vector v 0))
          (nth-value 1 (%mvq-struct-in-vector v 4))))
  ((2d0) (0d0) nil))

;; The boundaries: an empty vector index error and a non-structure still signal what they did.
(deftest mv-quiet-loop-ops.errors-unchanged
  (list (handler-case (%mvq-struct 5 1) (error (e) (type-of e)))
        (handler-case (%mvq-vector (vector) 1) (error (e) (type-of e)))
        (handler-case (%mvq-double-array (make-array 0 :element-type 'double-float) 1) (error (e) (type-of e))))
  (type-error type-error type-error))
