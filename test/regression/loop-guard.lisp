;;; A LOOP whose first clause counts from a start that is not a constant, or by a step that is
;;; not a constant, is expanded twice behind one test (start, limit and step are integers well inside
;;; int64): the counter is a raw integer in the long lane when the test holds, and the ordinary loop
;;; otherwise. Values at and beyond the edges of the guard, bignums, floats and ratios must be what
;;; they always were (the expected values are SBCL's).

(defun %lgd-up (s l b)
  (let ((r '()))
    (loop for j from s to l by b do (push j r) finally (return (list (nreverse r) j)))))
(defun %lgd-below (s l b)
  (let ((r '()))
    (loop for j from s below l by b do (push j r) finally (return (list (nreverse r) j)))))
(defun %lgd-noby (s l)
  (let ((r '()))
    (loop for j from s to l do (push j r) finally (return (list (nreverse r) j)))))
(defun %lgd-ret (s l b)
  (loop for j from s to l by b when (> j 20) do (return (list :found j))))
(defun %lgd-sum (s l b)
  (let ((sum 0)) (loop for j from s to l by b do (incf sum j)) sum))

(defparameter *lgd-cases*
  '((%lgd-up 3 20 4) (%lgd-up 20 3 1) (%lgd-up 3 3 1) (%lgd-up -5 5 3) (%lgd-up 1.5 6 2) (%lgd-up 1 6.5 2) (%lgd-up 1/2 5 1)
    (%lgd-up 1 9 1/2) (%lgd-up 1 4 1.5)
    (%lgd-up 1152921504606846976 1152921504606846980 2) (%lgd-up 2305843009213693950 2305843009213693955 2)
    (%lgd-up 2305843009213693952 2305843009213693953 1) (%lgd-up 4611686018427387900 4611686018427387905 2)
    (%lgd-up 9223372036854775805 9223372036854775810 1) (%lgd-up 9223372036854775800 9223372036854775810 4)
    (%lgd-up 1 5 2305843009213693952) (%lgd-up 1 5 2305843009213693953) (%lgd-up 1 5 (expt 2 70))
    (%lgd-up (- (expt 2 70)) (- 10 (expt 2 70)) 3) (%lgd-up 1 (expt 2 70) (expt 2 69))
    (%lgd-up -2305843009213693955 -2305843009213693950 2) (%lgd-up -9223372036854775810 -9223372036854775805 2)
    (%lgd-below 3 20 4) (%lgd-below 3 3 1) (%lgd-below 0 2305843009213693953 1152921504606846977) (%lgd-below 1 10 3)
    (%lgd-noby 2305843009213693950 2305843009213693954) (%lgd-noby 5 8) (%lgd-noby 8 5) (%lgd-noby 1.0 3)
    (%lgd-ret 3 100 7) (%lgd-ret 3 10 7) (%lgd-ret 30 100 7)
    (%lgd-sum 7 1000 7) (%lgd-sum 0 100000 13)))

(defun %lgd-count-primes (n)
  (let ((sieve (make-array (1+ n) :initial-element t)) (count 0))
    (loop for i from 2 to n
          when (aref sieve i)
            do (incf count)
               (loop for j from (* i i) to n by i do (setf (aref sieve j) nil)))
    count))

(deftest loop-guard.values
  (mapcar (lambda (c) (handler-case (apply (car c) (mapcar #'eval (cdr c))) (error () :error)))
          *lgd-cases*)
  (((3 7 11 15 19) 23) (NIL 20) ((3) 4) ((-5 -2 1 4) 7) ((1.5 3.5 5.5) 7.5) ((1 3 5) 7) ((1/2 3/2 5/2 7/2 9/2) 11/2) ((1 3/2 2 5/2 3 7/2 4 9/2 5 11/2 6 13/2 7 15/2 8 17/2 9) 19/2) ((1 2.5 4.0) 5.5) ((1152921504606846976 1152921504606846978 1152921504606846980) 1152921504606846982) ((2305843009213693950 2305843009213693952 2305843009213693954) 2305843009213693956) ((2305843009213693952 2305843009213693953) 2305843009213693954) ((4611686018427387900 4611686018427387902 4611686018427387904) 4611686018427387906) ((9223372036854775805 9223372036854775806 9223372036854775807 9223372036854775808 9223372036854775809 9223372036854775810) 9223372036854775811) ((9223372036854775800 9223372036854775804 9223372036854775808) 9223372036854775812) ((1) 2305843009213693953) ((1) 2305843009213693954) ((1) 1180591620717411303425) ((-1180591620717411303424 -1180591620717411303421 -1180591620717411303418 -1180591620717411303415) -1180591620717411303412) ((1 590295810358705651713) 1180591620717411303425) ((-2305843009213693955 -2305843009213693953 -2305843009213693951) -2305843009213693949) ((-9223372036854775810 -9223372036854775808 -9223372036854775806) -9223372036854775804) ((3 7 11 15 19) 23) (NIL 3) ((0 1152921504606846977) 2305843009213693954) ((1 4 7) 10) ((2305843009213693950 2305843009213693951 2305843009213693952 2305843009213693953 2305843009213693954) 2305843009213693955) ((5 6 7 8) 9) (NIL 8) ((1.0 2.0 3.0) 4.0) (:FOUND 24) NIL (:FOUND 30) 71071 384634614))

(deftest loop-guard.sieve
  (list (%lgd-count-primes 1) (%lgd-count-primes 2) (%lgd-count-primes 100) (%lgd-count-primes 10000))
  (0 1 25 1229))

(deftest-compiled-only loop-guard.no-boxing
  (list (< (min-allocated-bytes (lambda () (%lgd-count-primes 200000))) 3000000))
  (t))
