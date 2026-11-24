;;; MAX, MIN, LOGAND, LOGIOR, LOGXOR, GCD, LIST and APPEND have direct entries for
;;; the small argument counts that FUNCALL, APPLY and REDUCE use, so a call through
;;; the function object builds no argument array. The answers are the ones the
;;; n-ary definitions give, at the edges of the number tower too.

(defun %fbd-call (f &rest args) (apply f args))

(deftest funcall-builtin-direct.max-min
  (list (funcall #'max 5 7) (funcall #'max 7 5) (funcall #'min 5 7) (funcall #'min 7 5)
        (funcall #'max -3) (funcall #'min 4)
        (apply #'max '(1 9 3)) (apply #'min '(8 2 5)) (apply #'max '(1 2 3 4 5 6))
        (funcall #'max 1/2 1/3) (funcall #'min 1/2 1/3)
        (funcall #'max most-positive-fixnum (1+ most-positive-fixnum))
        (funcall #'min most-negative-fixnum (1- most-negative-fixnum))
        (funcall #'max (expt 10 30) (- (expt 10 30)))
        (funcall #'max 2.5d0 3) (funcall #'min 2.5d0 3)
        (reduce #'max '(3 9 2 7)) (reduce #'min '(3 9 2 7) :key #'-)
        (reduce #'max (vector 4 8 1) :key #'identity))
  (7 7 5 5 -3 4 9 2 6 1/2 1/3
   #.(1+ most-positive-fixnum) #.(1- most-negative-fixnum) #.(expt 10 30) 3 2.5d0
   9 -9 8))

(deftest funcall-builtin-direct.max-min-errors
  (list (handler-case (funcall #'max 'a 1) (type-error () :type-error))
        (handler-case (funcall #'min 1 "x") (type-error () :type-error))
        (handler-case (funcall #'max :k) (type-error () :type-error))
        (handler-case (apply #'max nil) (error () :error)))
  (:type-error :type-error :type-error :error))

(deftest funcall-builtin-direct.logical
  (list (funcall #'logand 12 10) (funcall #'logior 12 10) (funcall #'logxor 12 10)
        (funcall #'logand -1 255) (funcall #'logior 0 -1)
        (funcall #'logand (expt 2 70) (1- (expt 2 71)))
        (funcall #'logxor (- (expt 2 70)) (expt 2 70))
        (apply #'logand '(255 15 7)) (apply #'logior '(1 2 4)) (reduce #'logxor '(1 3 7))
        (handler-case (funcall #'logand 1.5 1) (type-error () :type-error))
        (handler-case (funcall #'logior 'a 1) (type-error () :type-error)))
  (8 14 6 255 -1 #.(expt 2 70) #.(- (* 2 (expt 2 70))) 7 7 5 :type-error :type-error))

(deftest funcall-builtin-direct.gcd
  (list (funcall #'gcd 12 18) (funcall #'gcd -12 18) (funcall #'gcd 0 5) (funcall #'gcd 0 0)
        (funcall #'gcd (expt 2 70) (expt 2 65)) (apply #'gcd '(12 18 27)) (funcall #'gcd 7)
        (handler-case (funcall #'gcd 1.5 2) (type-error () :type-error))
        (handler-case (funcall #'gcd 2 'a) (type-error () :type-error)))
  (6 6 5 0 #.(expt 2 65) 3 7 :type-error :type-error))

(deftest funcall-builtin-direct.list-append
  (list (funcall #'list 1) (funcall #'list 1 2) (funcall #'list 1 2 3) (funcall #'list)
        (apply #'list '(1 2 3 4)) (mapcar #'list '(1 2) '(a b))
        (funcall #'append '(1) '(2)) (funcall #'append nil '(2)) (funcall #'append '(1) 2)
        (funcall #'append '(1 2) nil) (apply #'append '((1) (2) (3))) (funcall #'append)
        (funcall #'append '(1))
        (handler-case (funcall #'append 1 '(2)) (type-error () :type-error))
        (let ((tail (list 9))) (eq tail (cdr (funcall #'append '(1) tail)))))
  ((1) (1 2) (1 2 3) nil (1 2 3 4) ((1 a) (2 b))
   (1 2) (2) (1 . 2) (1 2) (1 2 3) nil (1) :type-error t))

(deftest funcall-builtin-direct.same-as-direct-calls
  (loop for (a b) in '((0 0) (1 -1) (255 256) (-7 3) (123456789012 98765))
        always (and (= (funcall #'max a b) (max a b)) (= (funcall #'min a b) (min a b))
                    (= (funcall #'logand a b) (logand a b)) (= (funcall #'logior a b) (logior a b))
                    (= (funcall #'logxor a b) (logxor a b)) (= (funcall #'gcd a b) (gcd a b))
                    (equal (funcall #'list a b) (list a b)) (equal (funcall #'append (list a) (list b)) (list a b))))
  t)

(defun %fbd-fc2 (f a b) (funcall f a b))
(defun %fbd-loop (f a b) (dotimes (i 1000) (%fbd-fc2 f a b)))

(defun %fbd-bytes (f a b) (min-allocated-bytes (lambda () (%fbd-loop f a b)) 5))

;; #'+ already has direct entries, so it is the measure of a call that builds no array.
(deftest-compiled-only funcall-builtin-direct.allocation
  (let ((base (%fbd-bytes #'+ 5 7)))
    (list (<= (%fbd-bytes #'max 5 7) base)
          (<= (%fbd-bytes #'min 5 7) base)
          (<= (%fbd-bytes #'logand 5 7) base)
          (<= (%fbd-bytes #'gcd 12 18) base)
          (< (- (%fbd-bytes #'append '(1) '(2)) base) (* 1000 40))))
  (t t t t t))
