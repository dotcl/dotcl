;;; A LOOP counter whose limit is an expression with a provable range keeps that range: the
;;; (* I I) start of the inner loop of a sieve is then known to fit, and the inner counter is a raw
;;; int64 instead of a boxed number per step.

(defun %llr-sieve (n)
  (declare (fixnum n))
  (let ((v (make-array (1+ n) :element-type '(unsigned-byte 8) :initial-element 0))
        (count 0))
    (declare (type (simple-array (unsigned-byte 8) (*)) v) (fixnum count))
    (loop for i from 2 below (1+ (isqrt n)) do
      (when (zerop (aref v i))
        (loop for j from (* i i) to n by i do (setf (aref v j) 1))))
    (loop for i from 2 to n do (when (zerop (aref v i)) (incf count)))
    count))

(defun %llr-mark (v n)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (type (integer 0 100000000) n))
  (loop for i from 2 below (1+ (isqrt n)) do
    (when (zerop (aref v i))
      (loop for j from (* i i) to n by i do (setf (aref v j) 1))))
  v)

(defun %llr-sieve-fixed ()
  (let ((n 50000)
        (v (make-array 50001 :element-type '(unsigned-byte 8) :initial-element 0))
        (count 0))
    (declare (type fixnum n count) (type (simple-array (unsigned-byte 8) (*)) v))
    (loop for i from 2 below (isqrt n) do
      (when (zerop (aref v i))
        (loop for j from (* i i) below n by i do (setf (aref v j) 1))))
    (dotimes (i n) (declare (fixnum i)) (when (and (> i 1) (zerop (aref v i))) (incf count)))
    count))

(defun %llr-limit-real (x)
  (let ((r '()))
    (loop for i from 0 below x do (push i r))
    (nreverse r)))

(defun %llr-limit-big ()
  (let ((r '()))
    (loop for i from 0 below (expt 2 70) by (expt 2 68) do (push i r))
    (length r)))

(deftest loop-limit-range.values
  (list (%llr-sieve 1) (%llr-sieve 2) (%llr-sieve 30) (%llr-sieve 1000) (%llr-sieve-fixed)
        (%llr-limit-real 3) (%llr-limit-real 2.5) (%llr-limit-real 7/2) (%llr-limit-real 0)
        (%llr-limit-real -3) (%llr-limit-big))
  (0 1 10 168 5134 (0 1 2) (0 1 2) (0 1 2 3) nil nil 4))

(deftest-compiled-only loop-limit-range.no-allocation
  (let ((v (make-array 1000001 :element-type '(unsigned-byte 8) :initial-element 0)))
    (%llr-mark v 1000)
    (list (< (min-allocated-bytes (lambda () (fill v 0) (%llr-mark v 1000000))) 20000)))
  (t))
