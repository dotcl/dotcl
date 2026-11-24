;;;; Bit tests and sets, an integer division with both results, and a closure over
;;;; one value, against IlParity.Bits.
;;;;
;;;; The shapes the compiler has special paths for: a bit vector element write
;;;; and read, LOGBITP on fixnums, TRUNCATE whose two values are both read, and a
;;;; closure made at run time. The C# side is the plain code for each.

(in-package :cl-user)

(defun ilp-bits-set (v i)
  (declare (type simple-bit-vector v) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (setf (sbit v i) 1)
  nil)

(defun ilp-bits-test (v i)
  (declare (type simple-bit-vector v) (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (sbit v i)))

(defun ilp-bits-logbit (x k)
  (declare (fixnum x k) (optimize (speed 3) (safety 0) (debug 0)))
  (logbitp k x))

(defun ilp-bits-divmod (a b)
  (declare (fixnum a b) (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (q r) (truncate a b)
    (declare (fixnum q r))
    (the fixnum (+ (the fixnum (* q 31)) r))))

(defun ilp-bits-adder (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (lambda (x) (declare (fixnum x)) (the fixnum (+ x n))))

(defun ilp-bits-selfcheck (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((v (make-array n :element-type 'bit :initial-element 0))
        (acc 0))
    (declare (type simple-bit-vector v) (fixnum acc))
    (do ((i 0 (the fixnum (1+ i))))
        ((>= i n))
      (declare (fixnum i))
      (when (zerop (the fixnum (mod i 3))) (ilp-bits-set v i)))
    (do ((i 0 (the fixnum (1+ i))))
        ((>= i n))
      (declare (fixnum i))
      (setq acc (the fixnum (+ acc (the fixnum (ilp-bits-test v i)))))
      (when (ilp-bits-logbit (the fixnum (* i 7919)) (the fixnum (mod i 40)))
        (setq acc (the fixnum (+ acc 3))))
      (setq acc (the fixnum (+ acc (the fixnum (ilp-bits-divmod (the fixnum (+ (the fixnum (* i 13)) 5))
                                                                  (the fixnum (1+ (the fixnum (mod i 17)))))))))
      (setq acc (the fixnum (+ acc (the fixnum (funcall (the function (ilp-bits-adder i)) (the fixnum (1+ i))))))))
    acc))
