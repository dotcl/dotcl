;;;; A fixnum stack over a simple-array, against IlParity.Stack in Ref.cs.
;;;;
;;;; Every declaration a writer could reasonably give is given: the slot types,
;;;; the argument types, and (speed 3) (safety 0) (debug 0). What is being
;;;; compared is what the compiler does with a fully declared program, not what
;;;; it does with hints withheld.

(in-package :cl-user)

(defstruct (ilp-stack (:constructor %make-ilp-stack (items count)))
  (items (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (count 0 :type fixnum))

(defun ilp-stack-new (capacity)
  (declare (fixnum capacity) (optimize (speed 3) (safety 0) (debug 0)))
  (%make-ilp-stack (make-array capacity :element-type 'fixnum :initial-element 0) 0))

(defun ilp-stack-push (s v)
  (declare (type ilp-stack s) (fixnum v)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((items (ilp-stack-items s))
        (n (ilp-stack-count s)))
    (declare (type (simple-array fixnum (*)) items) (fixnum n))
    (setf (aref items n) v)
    (setf (ilp-stack-count s) (the fixnum (1+ n)))
    v))

(defun ilp-stack-pop (s)
  (declare (type ilp-stack s) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((items (ilp-stack-items s))
        (n (the fixnum (1- (ilp-stack-count s)))))
    (declare (type (simple-array fixnum (*)) items) (fixnum n))
    (setf (ilp-stack-count s) n)
    (the fixnum (aref items n))))

(defun ilp-stack-peek (s)
  (declare (type ilp-stack s) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((items (ilp-stack-items s)))
    (declare (type (simple-array fixnum (*)) items))
    (the fixnum (aref items (the fixnum (1- (ilp-stack-count s)))))))

(defun ilp-stack-size (s)
  (declare (type ilp-stack s) (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (ilp-stack-count s)))

(defun ilp-stack-selfcheck (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((s (ilp-stack-new n))
        (acc 0))
    (declare (fixnum acc))
    (dotimes (i n)
      (declare (fixnum i))
      (ilp-stack-push s (the fixnum (* i 3))))
    (dotimes (i (the fixnum (floor n 2)))
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (ilp-stack-pop s)))))
    (setq acc (the fixnum (+ acc (ilp-stack-peek s))))
    (setq acc (the fixnum (+ acc (ilp-stack-size s))))
    acc))
