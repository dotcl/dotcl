;;; (SQRT (FLOAT I 1D0)) and (SQRT (COERCE I 'DOUBLE-FLOAT)) of an integer the compiler can see is
;;; not negative is a double: no box per step. A value that may be negative keeps the generic SQRT,
;;; whose answer is a complex number.

(defun %sci-sum-float (n)
  (declare (fixnum n))
  (let ((s 0d0))
    (declare (double-float s))
    (dotimes (i n) (setf s (+ s (sqrt (float i 1d0)))))
    s))

(defun %sci-sum-coerce (n)
  (declare (fixnum n))
  (let ((s 0d0))
    (declare (double-float s))
    (dotimes (i n) (setf s (+ s (sqrt (coerce i 'double-float)))))
    s))

(defun %sci-sum-loop (lo n)
  (declare (type (integer 0 1000) lo) (fixnum n))
  (let ((s 0d0))
    (declare (double-float s))
    (loop for i of-type fixnum from lo below n do (setf s (+ s (sqrt (float i 1d0)))))
    s))

(defun %sci-negative (x)
  (declare (fixnum x))
  (sqrt (float x 1d0)))

(defun %sci-points (n)
  (declare (fixnum n))
  (let ((r '()))
    (dotimes (i n) (push (sqrt (float i 1d0)) r))
    (nreverse r)))

(deftest sqrt-of-converted-integer.values
  (list (%sci-points 6)
        (= (%sci-sum-float 1000) (%sci-sum-coerce 1000))
        (< (abs (- (%sci-sum-float 1000) (loop for i below 1000 sum (sqrt (float i 1d0))))) 1d-9)
        (%sci-sum-float 0)
        (%sci-sum-loop 3 3)
        (< (abs (- (%sci-sum-loop 2 10) (loop for i from 2 below 10 sum (sqrt (float i 1d0))))) 1d-12))
  ((0d0 1d0 1.4142135623730951d0 1.7320508075688772d0 2d0 2.23606797749979d0) t t 0d0 0d0 t))

(deftest sqrt-of-converted-integer.negative-keeps-the-complex
  (list (%sci-negative 4) (%sci-negative -4) (%sci-negative 0) (%sci-negative most-negative-fixnum)
        (typep (%sci-negative -1) 'complex))
  (2d0 #C(0d0 2d0) 0d0 #C(0d0 3.03700049997605d9) t))

(deftest-compiled-only sqrt-of-converted-integer.no-allocation
  (progn (%sci-sum-float 1000) (%sci-sum-coerce 1000)
         (list (< (min-allocated-bytes (lambda () (%sci-sum-float 200000))) 20000)
               (< (min-allocated-bytes (lambda () (%sci-sum-coerce 200000))) 20000)))
  (t t))

;; The conversion itself, at the boundaries, against the generic function: a declared fixnum
;; goes through conv.r8 / conv.r4 and must round as FLOAT does.
(defun %sci-conv-d (x) (declare (fixnum x)) (list (float x 1d0) (coerce x 'double-float)))
(defun %sci-conv-s (x) (declare (fixnum x)) (list (float x 1f0) (coerce x 'single-float)))

(deftest sqrt-of-converted-integer.conversion-boundaries
  (let ((xs (list 0 -1 1 most-positive-fixnum most-negative-fixnum
                  (1- (expt 2 53)) (expt 2 53) (1+ (expt 2 53)) (+ (expt 2 53) 3)
                  (- (expt 2 53)) (1- (expt 2 62)) (expt 2 61) (1+ (expt 2 24)) (+ (expt 2 24) 3))))
    (list (every (lambda (x) (let ((r (%sci-conv-d x)))
                               (and (= (first r) (funcall #'float x 1d0))
                                    (= (second r) (funcall #'coerce x 'double-float)))))
                 xs)
          (every (lambda (x) (let ((r (%sci-conv-s x)))
                               (and (= (first r) (funcall #'float x 1f0))
                                    (= (second r) (funcall #'coerce x 'single-float)))))
                 xs)
          (funcall #'float (1+ (expt 2 62)) 1d0)
          (float (1+ (expt 2 62)) 1d0)))
  (t t 4.611686018427388d18 4.611686018427388d18))
