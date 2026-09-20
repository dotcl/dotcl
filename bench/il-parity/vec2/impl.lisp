;;;; A 2D vector with DOUBLE-FLOAT slots, against IlParity.Vec2.
;;;;
;;;; The integer cases ask what a declared FIXNUM slot costs; this asks the same
;;;; of a DOUBLE-FLOAT one, where the boxed representation costs an object per
;;;; value rather than per value outside a small cache. Normalising is the shape
;;;; that reads both slots, does real arithmetic and writes both back.

(in-package :cl-user)

(defstruct (ilp-vec2 (:constructor %make-ilp-vec2 (x y)))
  (x 0.0d0 :type double-float)
  (y 0.0d0 :type double-float))

(defun ilp-vec2-new (x y)
  (declare (double-float x y) (optimize (speed 3) (safety 0) (debug 0)))
  (%make-ilp-vec2 x y))

(defun ilp-vec2-length (v)
  (declare (type ilp-vec2 v) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((x (ilp-vec2-x v))
        (y (ilp-vec2-y v)))
    (declare (double-float x y))
    (the double-float (sqrt (the double-float
                                 (+ (the double-float (* x x))
                                    (the double-float (* y y))))))))

(defun ilp-vec2-normalize (v)
  "Scale to unit length in place, returning the old length."
  (declare (type ilp-vec2 v) (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((x (ilp-vec2-x v))
         (y (ilp-vec2-y v))
         (len (the double-float (sqrt (the double-float
                                           (+ (the double-float (* x x))
                                              (the double-float (* y y))))))))
    (declare (double-float x y len))
    (if (= len 0.0d0)
        0.0d0
        (progn
          (setf (ilp-vec2-x v) (the double-float (/ x len)))
          (setf (ilp-vec2-y v) (the double-float (/ y len)))
          len))))

(defun ilp-vec2-dot (v other)
  (declare (type ilp-vec2 v other) (optimize (speed 3) (safety 0) (debug 0)))
  (the double-float (+ (the double-float (* (ilp-vec2-x v) (ilp-vec2-x other)))
                       (the double-float (* (ilp-vec2-y v) (ilp-vec2-y other))))))

(defun ilp-vec2-selfcheck (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((acc 0.0d0))
    (declare (double-float acc))
    (do ((i 1 (the fixnum (1+ i))))
        ((> i n))
      (declare (fixnum i))
      (let ((v (ilp-vec2-new (coerce i 'double-float)
                             (the double-float (* (coerce i 'double-float) 2.0d0)))))
        (setq acc (the double-float (+ acc (ilp-vec2-normalize v))))
        (setq acc (the double-float
                       (+ acc (the double-float (* (ilp-vec2-dot v v) 1000.0d0)))))
        (setq acc (the double-float (+ acc (ilp-vec2-length v))))))
    ;; Folded to an integer so the two halves compare exactly rather than within
    ;; a tolerance: the same operations in the same order give the same doubles.
    (the fixnum (floor (the double-float (* acc 1000.0d0))))))
