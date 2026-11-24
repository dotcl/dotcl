;;; An undeclared double accumulator updated from a DOTIMES counter -- (setq x (+ x (float i 1d0)))
;;; -- is a raw double: the counter is known not to be negative, and a value that names a variable
;;; bound inside the loop is read with that variable's declared range. Anything the compiler cannot
;;; show to be a double keeps the generic path and its answers.

(defun %fli-sum (n) (let ((x 0d0)) (dotimes (i n) (setq x (+ x (float i 1d0)))) x))
(defun %fli-sqrt (n) (let ((x 0d0)) (dotimes (i n) (setq x (+ (* x 0.999d0) (sqrt (float i 1d0))))) x))
(defun %fli-sqrt-ref (n)
  (let ((x 0d0)) (dotimes (i n) (setq x (+ (* (funcall #'identity x) 0.999d0) (funcall #'sqrt (funcall #'float i 1d0))))) x))
(defun %fli-assigned-counter (n)
  ;; The body assigns the counter: it is not known to be non-negative, so the SQRT stays generic.
  (let ((x 0d0)) (dotimes (i n) (setq i i) (setq x (+ x (realpart (sqrt (float i 1d0)))))) x))
(defun %fli-negative-sqrt (n)
  (let ((x 0d0)) (dotimes (i n) (setq x (+ x (realpart (sqrt (- (float i 1d0) 2d0)))))) x))
(defun %fli-captured (n)
  (let ((x 0d0) (fs '()))
    (dotimes (i n) (setq x (+ x (float i 1d0))) (push (lambda () x) fs))
    (mapcar #'funcall fs)))
(defun %fli-later-integer (n)
  (let ((x 0d0)) (dotimes (i n) (setq x (+ x 1d0))) (setq x 7) x))
(defun %fli-shadow (n)
  (let ((x 0d0) (i 2.5d0))
    (declare (double-float i))
    (dotimes (j n) (let ((i j)) (setq x (+ x (float i 1d0)))))
    (+ x i)))
(defun %fli-two (n)
  (let ((x 0d0) (y 1d0))
    (dotimes (i n) (setq x (+ x (float i 1d0)) y (* y 1.0001d0)))
    (list x y)))

(deftest float-loop-inference.values
  (list (%fli-sum 0) (%fli-sum 1) (%fli-sum 5) (%fli-sum 1000)
        (= (%fli-sqrt 1000) (%fli-sqrt-ref 1000)) (%fli-sqrt 0)
        (%fli-two 4) (%fli-shadow 3) (%fli-later-integer 3))
  (0d0 0d0 10d0 499500d0 t 0d0 (6d0 1.000400060004d0) 5.5d0 7))

(deftest float-loop-inference.counter-assigned-or-negative-argument
  (list (%fli-assigned-counter 5) (%fli-negative-sqrt 5)
        (%fli-sum -3) (%fli-sum 2.5) (%fli-sum (expt 10 0)))
  (6.146264369941973d0 2.414213562373095d0 0d0 3d0 0d0))

(deftest float-loop-inference.captured
  (let ((r (%fli-captured 3))) (list (length r) (every (lambda (v) (= v 3d0)) r)))
  (3 t))

(deftest-compiled-only float-loop-inference.no-allocation
  (progn (%fli-sum 1000) (%fli-sqrt 1000)
         (list (< (min-allocated-bytes (lambda () (%fli-sum 1000000))) 20000)
               (< (min-allocated-bytes (lambda () (%fli-sqrt 1000000))) 20000)))
  (t t))

;;; Variables of one LET that read each other (the Mandelbrot iteration), PSETQ included.
(defun %fli-mandel (size)
  (declare (fixnum size))
  (let ((count 0))
    (dotimes (py size)
      (dotimes (px size)
        (let ((cr (- (/ (* 3.0d0 px) size) 2.0d0))
              (ci (- (/ (* 2.0d0 py) size) 1.0d0))
              (zr 0d0) (zi 0d0) (i 0))
          (loop while (and (< i 50) (< (+ (* zr zr) (* zi zi)) 4d0))
                do (psetq zr (+ (- (* zr zr) (* zi zi)) cr)
                          zi (+ (* 2d0 zr zi) ci))
                   (incf i))
          (when (= i 50) (incf count)))))
    count))
(defun %fli-mandel-ref (size)
  (let ((count 0))
    (dotimes (py size)
      (dotimes (px size)
        (let ((cr (funcall #'- (funcall #'/ (funcall #'* 3.0d0 px) size) 2.0d0))
              (ci (funcall #'- (funcall #'/ (funcall #'* 2.0d0 py) size) 1.0d0))
              (zr 0d0) (zi 0d0) (i 0))
          (loop while (and (< i 50) (< (funcall #'+ (funcall #'* zr zr) (funcall #'* zi zi)) 4d0))
                do (psetq zr (funcall #'+ (funcall #'- (funcall #'* zr zr) (funcall #'* zi zi)) cr)
                          zi (funcall #'+ (funcall #'* 2d0 zr zi) ci))
                   (incf i))
          (when (= i 50) (incf count)))))
    count))
(defun %fli-psetq-swap (n)
  (let ((a 1d0) (b 2d0))
    (dotimes (i n) (psetq a b b (+ a b)))
    (list a b)))
(defun %fli-sibling-integer (n)
  ;; B is read by A's update but is assigned an integer later: neither is a raw double.
  (let ((a 0d0) (b 1d0))
    (dotimes (i n) (setq a (+ a b)))
    (setq b 5)
    (list a b)))
(defun %fli-sibling-reads-a-let (n)
  (let ((a 0d0) (b 1d0))
    (dotimes (i n) (setq a (+ a (let ((b 2d0)) b) b)))
    a))

(deftest float-loop-inference.siblings-and-psetq
  (list (= (%fli-mandel 40) (%fli-mandel-ref 40)) (%fli-mandel 1) (%fli-mandel 0)
        (%fli-psetq-swap 5) (%fli-sibling-integer 3) (%fli-sibling-reads-a-let 3))
  (t 0 0 (13d0 21d0) (3d0 5) 9d0))

(deftest-compiled-only float-loop-inference.mandel-no-allocation
  (progn (%fli-mandel 20)
         (list (< (min-allocated-bytes (lambda () (%fli-mandel 100))) 400000)))
  (t))

;;; A DOTIMES count that is no declared fixnum: inside the body it is known to be a real, so
;;; (/ (* 3.0d0 px) size) is a raw double. The count may be any real number.
(defun %fli-size-mandel (size)
  (let ((count 0))
    (dotimes (py size)
      (dotimes (px size)
        (let ((cr (- (/ (* 3.0d0 px) size) 2.0d0))
              (ci (- (/ (* 2.0d0 py) size) 1.0d0))
              (zr 0d0) (zi 0d0) (i 0))
          (loop while (and (< i 50) (< (+ (* zr zr) (* zi zi)) 4d0))
                do (psetq zr (+ (- (* zr zr) (* zi zi)) cr)
                          zi (+ (* 2d0 zr zi) ci))
                   (incf i))
          (when (= i 50) (incf count)))))
    count))
(defun %fli-size-sum (size) (let ((x 0d0)) (dotimes (i size) (setq x (+ x (/ 1d0 size)))) x))
(defun %fli-size-ref (size)
  (let ((x 0d0)) (dotimes (i size) (setq x (funcall #'+ x (funcall #'/ 1d0 size)))) x))
(defun %fli-size-assigned (size)
  ;; The body assigns the count variable: not rebound, the loop still ends by the first value.
  (let ((x 0d0)) (dotimes (i size) (setq x (+ x (/ 1d0 size)))) (setq size 100) (list x size)))
(defun %fli-size-closure (size)
  (let ((x 0d0) (f (lambda () (setq size 2))))
    (dotimes (i size) (setq x (+ x (/ 1d0 size))) (when (= i 1) (funcall f)))
    x))
(defvar *fli-special-size* 4)
(defun %fli-special-size ()
  (let ((x 0d0)) (dotimes (i *fli-special-size*) (setq x (+ x (/ 1d0 *fli-special-size*)))) x))

(deftest float-loop-inference.undeclared-real-count
  (list (= (%fli-size-mandel 30) (%fli-mandel-ref 30))
        (%fli-size-sum 4) (%fli-size-sum 5/2) (%fli-size-sum 2.5d0) (%fli-size-sum 3.5f0)
        (%fli-size-sum #m4) (%fli-size-sum 0) (%fli-size-sum -2)
        (= (%fli-size-sum 4) (%fli-size-ref 4)) (%fli-size-assigned 4)
        (%fli-size-closure 5) (%fli-special-size))
  (t 1d0 1.2000000000000002d0 1.2000000000000002d0 1.1428571428571428d0 1d0 0d0 0d0 t (1d0 100) 1.9d0 1d0))

(deftest float-loop-inference.undeclared-count-that-is-not-real
  (list (handler-case (%fli-size-sum nil) (error (e) (type-of e)))
        (handler-case (%fli-size-sum "3") (error (e) (type-of e)))
        (handler-case (%fli-size-sum #c(1 2)) (error (e) (type-of e)))
        (handler-case (%fli-size-sum 'a) (error (e) (type-of e))))
  (type-error type-error type-error type-error))

(deftest-compiled-only float-loop-inference.undeclared-count-no-allocation
  (progn (%fli-size-sum 1000)
         (list (< (min-allocated-bytes (lambda () (%fli-size-sum 1000000))) 20000)
               (< (min-allocated-bytes (lambda () (%fli-size-mandel 60))) 400000)))
  (t t))

;;; INCF / DECF of an undeclared double accumulator (a dot product).
(defun %fli-dot (n)
  (let* ((a (make-array (list n n) :element-type 'double-float :initial-element 1.5d0))
         (b (make-array (list n n) :element-type 'double-float :initial-element 2.5d0))
         (c (make-array (list n n) :element-type 'double-float :initial-element 0d0)))
    (dotimes (i n)
      (dotimes (j n)
        (let ((s 0d0))
          (dotimes (k n) (incf s (* (aref a i k) (aref b k j))))
          (setf (aref c i j) s))))
    (list (aref c 0 0) (aref c (1- n) (1- n)))))
(defun %fli-incf-decf (n)
  (let ((s 0d0) (d 100d0))
    (dotimes (i n) (incf s 0.5d0) (decf d (float i 1d0)) (incf s i))
    (list s d)))
(defun %fli-incf-mixed (n)
  ;; Not a double after all: a ratio and then an integer are added.
  (let ((s 0d0))
    (dotimes (i n) (incf s 1/2))
    (incf s 1)
    s))
(defun %fli-incf-integer-assigned (n)
  (let ((s 0d0))
    (dotimes (i n) (incf s 1d0))
    (setq s 7)
    (incf s 1)
    s))

(deftest float-loop-inference.incf-decf
  (list (%fli-dot 3) (%fli-dot 1)
        (%fli-incf-decf 4) (%fli-incf-mixed 3) (%fli-incf-integer-assigned 3))
  ((11.25d0 11.25d0) (3.75d0 3.75d0)
   (8d0 94d0) 2.5d0 8))

(deftest-compiled-only float-loop-inference.dot-no-allocation
  (progn (%fli-dot 4)
         (list (< (min-allocated-bytes (lambda () (%fli-dot 100))) 400000)))
  (t))

;;; A value that reads a variable bound inside the loop to a double (an inlined function's argument,
;;; a LET temporary): that variable is read as the double it is.
(declaim (inline %fli-clamp))
(defun %fli-clamp (n lo hi) (if (< n lo) lo (if (> n hi) hi n)))
(defun %fli-clamp-sum (m)
  (let ((s 0d0)) (dotimes (i m) (incf s (%fli-clamp (* i 0.5d0) 10d0 100d0))) s))
(defun %fli-let-temp (m)
  (let ((s 0d0))
    (dotimes (i m) (let ((n (* i 0.25d0)) (k (* i 0.5d0))) (setq s (+ s (if (< n 3d0) n (- n 1d0)) (* 0.5d0 k)))))
    s))
(defun %fli-let-temp-assigned (m)
  ;; N is assigned later in its LET: not known to be a double when it is read.
  (let ((s 0d0))
    (dotimes (i m) (let ((n (* i 0.25d0))) (setq s (+ s n)) (setq n 7)))
    s))
(defun %fli-let-temp-integer (m)
  (let ((s 0d0))
    (dotimes (i m) (let ((n (+ i 1))) (setq s (+ s n))))
    s))
(defun %fli-shadowed-temp (m)
  (let ((s 0d0) (n 100))
    (dotimes (i m) (let ((n (* i 2d0))) (setq s (+ s n))))
    (list s n)))

(deftest float-loop-inference.inner-let-doubles
  (list (%fli-clamp-sum 0) (%fli-clamp-sum 1) (%fli-clamp-sum 30) (%fli-clamp-sum 300)
        (%fli-let-temp 0) (%fli-let-temp 3) (%fli-let-temp 20)
        (%fli-let-temp-assigned 4) (%fli-let-temp-integer 4) (%fli-shadowed-temp 4))
  (0d0 10d0 322.5d0 20055d0 0d0 1.5d0 87d0 1.5d0 10d0 (12d0 100)))

(deftest-compiled-only float-loop-inference.inner-let-no-allocation
  (progn (%fli-clamp-sum 1000) (%fli-let-temp 1000)
         (list (< (min-allocated-bytes (lambda () (%fli-clamp-sum 1000000))) 20000)
               (< (min-allocated-bytes (lambda () (%fli-let-temp 1000000))) 20000)))
  (t t))
