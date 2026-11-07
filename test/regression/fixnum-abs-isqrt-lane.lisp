;;; ABS and ISQRT of fixnum-typed operands stay in the native fixnum lane (no boxed result per
;;; iteration) and give the same answers as the generic functions.

(defun %fail-abs-loop (n)
  (declare (fixnum n) (optimize speed))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i n) (setq s (logand (+ s (abs (- i 500))) #xffffff)))
    s))

(defun %fail-isqrt-loop (n)
  (declare (fixnum n) (optimize speed))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i n) (setq s (logand (+ s (isqrt i)) #xffffff)))
    s))

(defun %fail-isqrt-of (x)
  (declare (type (integer 0 4611686018427387903) x))
  (isqrt x))

(defun %fail-abs-of (x)
  (declare (type (integer -4611686018427387904 4611686018427387903) x))
  (abs x))

(deftest-compiled-only fixnum-abs-isqrt-lane.no-allocation
  (progn (%fail-abs-loop 100) (%fail-isqrt-loop 100)
         (list (< (min-allocated-bytes (lambda () (%fail-abs-loop 200000))) 4096)
               (< (min-allocated-bytes (lambda () (%fail-isqrt-loop 200000))) 4096)))
  (t t))

(deftest fixnum-abs-isqrt-lane.values
  (list (%fail-abs-loop 1000)
        (loop with s = 0 for i below 1000 do (setq s (logand (+ s (abs (- i 500))) #xffffff)) finally (return s))
        (%fail-isqrt-loop 100000)
        (loop with s = 0 for i below 100000 do (setq s (logand (+ s (isqrt i)) #xffffff)) finally (return s)))
  (#.(loop with s = 0 for i below 1000 do (setq s (logand (+ s (abs (- i 500))) #xffffff)) finally (return s))
   #.(loop with s = 0 for i below 1000 do (setq s (logand (+ s (abs (- i 500))) #xffffff)) finally (return s))
   #.(loop with s = 0 for i below 100000 do (setq s (logand (+ s (isqrt i)) #xffffff)) finally (return s))
   #.(loop with s = 0 for i below 100000 do (setq s (logand (+ s (isqrt i)) #xffffff)) finally (return s))))

(deftest fixnum-abs-isqrt-lane.edges
  (list (mapcar #'%fail-isqrt-of '(0 1 2 3 4 8 9 15 16 99 100 4611686018427387903 4611686014132420609
                                   4611686014132420608 4503599627370495 4503599627370496))
        (mapcar #'%fail-abs-of '(0 1 -1 5 -5 4611686018427387903 -4611686018427387904))
        (%fail-abs-of -4611686018427387904))
  ((0 1 1 1 2 2 3 3 4 9 10 2147483647 2147483647 2147483646 67108863 67108864)
   (0 1 1 5 5 4611686018427387903 4611686018427387904)
   4611686018427387904))

(deftest fixnum-abs-isqrt-lane.not-fixnum-still-generic
  (list (abs -5) (abs (- (expt 2 70))) (isqrt (expt 10 30)) (isqrt 99)
        (handler-case (isqrt -4) (error () :error))
        (flet ((abs (x) (* x 10))) (abs 3)))
  (5 #.(expt 2 70) #.(expt 10 15) 9 :error 30))
