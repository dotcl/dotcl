;;;; A fixed-capacity ring buffer of fixnums, against IlParity.Ring.
;;;;
;;;; Head, tail and a count, with the wrap done by LOGAND against a power-of-two
;;;; mask. The densest field traffic of the five cases, which is what makes it
;;;; the one where a difference in how a struct is represented shows up first.

(in-package :cl-user)

(defstruct (ilp-ring (:constructor %make-ilp-ring (items mask head tail count)))
  (items (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (mask 0 :type fixnum)
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (count 0 :type fixnum))

(defun ilp-ring-new (capacity)
  "CAPACITY must be a power of two."
  (declare (fixnum capacity) (optimize (speed 3) (safety 0) (debug 0)))
  (%make-ilp-ring (make-array capacity :element-type 'fixnum :initial-element 0)
                  (the fixnum (1- capacity)) 0 0 0))

(defun ilp-ring-enqueue (r v)
  (declare (type ilp-ring r) (fixnum v)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((items (ilp-ring-items r))
        (tail (ilp-ring-tail r)))
    (declare (type (simple-array fixnum (*)) items) (fixnum tail))
    (setf (aref items tail) v)
    (setf (ilp-ring-tail r) (the fixnum (logand (the fixnum (1+ tail))
                                                (ilp-ring-mask r))))
    (setf (ilp-ring-count r) (the fixnum (1+ (ilp-ring-count r))))
    v))

(defun ilp-ring-dequeue (r)
  (declare (type ilp-ring r) (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((items (ilp-ring-items r))
         (head (ilp-ring-head r))
         (v (the fixnum (aref items head))))
    (declare (type (simple-array fixnum (*)) items) (fixnum head v))
    (setf (ilp-ring-head r) (the fixnum (logand (the fixnum (1+ head))
                                                (ilp-ring-mask r))))
    (setf (ilp-ring-count r) (the fixnum (1- (ilp-ring-count r))))
    v))

(defun ilp-ring-peek (r)
  (declare (type ilp-ring r) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((items (ilp-ring-items r)))
    (declare (type (simple-array fixnum (*)) items))
    (the fixnum (aref items (ilp-ring-head r)))))

(defun ilp-ring-size (r)
  (declare (type ilp-ring r) (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (ilp-ring-count r)))

(defun ilp-ring-selfcheck (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((r (ilp-ring-new 64))
        (acc 0))
    (declare (fixnum acc))
    (dotimes (i n)
      (declare (fixnum i))
      (ilp-ring-enqueue r (the fixnum (* i 5)))
      ;; Keep it under capacity: enqueue two, drain one, so the indices wrap
      ;; many times over the run.
      (when (= (the fixnum (logand i 1)) 1)
        (setq acc (the fixnum (+ acc (ilp-ring-dequeue r))))))
    (loop while (> (ilp-ring-size r) 0)
          do (setq acc (the fixnum (+ acc (ilp-ring-dequeue r)))))
    acc))
