;;; Nested +, -, * on integers compute their intermediates without boxing them
;;; when the operands are fixnums, and a LET whose variables are each read once
;;; is compiled as its body with the inits in place. The values, overflow into
;;; bignums, non-fixnum operands, evaluation order and the number of values must
;;; be what the generic calls give.

(setf dotcl:*save-sil* t)

(defun %nia-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %nia-mul-add (x y z) (+ (* x y) z))
(defun %nia-sub-mul (x y z) (- (* x y) z))
(defun %nia-int (x r) (declare (integer x r)) (+ (* x 7) r))
(defun %nia-fix (i j) (declare (fixnum i j)) (+ (* i i) j))
(defun %nia-sum3 (a b c) (+ a b c))
(defun %nia-diff2 (a b c d) (- (* a b) (* c d)))
(defun %nia-incr (x y) (1+ (* x y)))
(defun %nia-left (x y z) (* z (+ x y)))

(deftest nested-integer-arith.values
  (list (%nia-mul-add 3 4 5)
        (%nia-mul-add -3 4 5)
        (%nia-mul-add 100000 100000 1)
        (%nia-int 100000 50)
        (%nia-fix 3037000499 1)
        (%nia-sum3 1 2 3)
        (%nia-diff2 7 8 3 4)
        (%nia-incr 6 7)
        (%nia-left 2 3 4))
  (17 -7 10000000001 700050 9223372030926249002 6 44 43 20))

(deftest nested-integer-arith.overflow
  (list (%nia-mul-add 4611686018427387904 2 0)
        (%nia-mul-add 4611686018427387904 4 -1)
        (%nia-mul-add 9223372036854775807 9223372036854775807 1)
        (%nia-sum3 9223372036854775807 1 1)
        (%nia-sum3 9223372036854775807 1 -2)
        (%nia-incr 3037000500 3037000500)
        (%nia-left 9223372036854775807 1 2))
  (9223372036854775808 18446744073709551615
   85070591730234615847396907784232501250 9223372036854775809
   9223372036854775806
   9223372037000250001 18446744073709551616))

;;; An intermediate of exactly -2^63 fits, but is the value the lane uses to
;;; leave for the generic path.
(deftest nested-integer-arith.int64-min-intermediate
  (list (%nia-sub-mul -4611686018427387904 2 1)
        (%nia-mul-add -4611686018427387904 2 0)
        (%nia-sum3 -9223372036854775807 -1 1))
  (-9223372036854775809 -9223372036854775808 -9223372036854775807))

(deftest nested-integer-arith.other-numbers
  (list (%nia-mul-add 1.5d0 2 1)
        (%nia-mul-add 1/2 4 1)
        (%nia-mul-add (expt 2 70) 2 (- (expt 2 71)))
        (%nia-sum3 1 2.0 3)
        (%nia-diff2 #c(0 1) #c(0 1) 1 1))
  (4.0d0 3 0 6.0 -2))

(deftest nested-integer-arith.type-error
  (list (handler-case (%nia-mul-add 'a 2 3) (type-error () :type-error))
        (handler-case (%nia-mul-add 2 3 'b) (type-error () :type-error))
        (handler-case (%nia-sum3 1 nil 3) (type-error () :type-error)))
  (:type-error :type-error :type-error))

;;; The multiply signals before the second operand of the add is evaluated.
(defun %nia-order (x cell) (+ (* x 2) (progn (incf (car cell)) 1)))

(deftest nested-integer-arith.order
  (let ((c (list 0)))
    (list (handler-case (%nia-order 'a c) (type-error () :type-error))
          (car c)
          (%nia-order 5 c)
          (car c)))
  (:type-error 0 11 1))

(deftest nested-integer-arith.one-value
  (list (multiple-value-list (%nia-mul-add 2 3 4))
        (multiple-value-list (%nia-sum3 1 2 3)))
  ((10) (6)))

(deftest-emitting-only nested-integer-arith.sil
  (list (%nia-count "Runtime.MultiplyOrMin" #'%nia-mul-add)
        (%nia-count "Runtime.MultiplyOrMin" #'%nia-int)
        (%nia-count "Runtime.MultiplyOrMin" #'%nia-fix)
        (%nia-count "Runtime.AddOrMin" #'%nia-sum3)
        (%nia-count "Runtime.MultiplyOrMin" #'%nia-diff2))
  (1 1 1 1 2))

;;; ---- LET whose variables are read once ----

;;; Coalton's arithmetic: each operation binds its operands, and a LET binds
;;; the inner result.
(defun %nia-coalton (x r)
  (declare (integer x r))
  (the integer
       (let ((a1 (let ((b0 7))
                   (declare (ignorable b0))
                   (locally (declare (optimize (safety 0)))
                     (the integer (let ((a x) (b b0)) (values (identity (* a b)))))))))
         (declare (ignorable a1))
         (locally (declare (optimize (safety 0)))
           (the integer (let ((a a1) (b r)) (values (identity (+ a b)))))))))

;;; A constant operand is bound by its own LET around the operation.
(defun %nia-coalton-lcg (x)
  (the integer
       (let ((a2 (let ((a1 (let ((b1 1103515245))
                             (declare (ignorable b1))
                             (locally (declare (optimize (safety 0)))
                               (the integer (let ((a x) (b b1)) (values (identity (* a b)))))))))
                   (declare (ignorable a1))
                   (let ((b2 12345))
                     (declare (ignorable b2))
                     (locally (declare (optimize (safety 0)))
                       (the integer (let ((a a1) (b b2)) (values (identity (+ a b))))))))))
         (declare (ignorable a2))
         (let ((n 2147483648))
           (declare (ignorable n))
           (the integer (let ((a a2) (n n)) (values (mod a n))))))))

(defun %nia-floor-tail (x) (let ((q (floor x 2))) q))
(defun %nia-floor-arg (x) (let ((q (floor x 2)) (k 1)) (+ q k)))
(defun %nia-floor-the (x) (let ((q (floor x 2))) (the integer q)))

(deftest nested-integer-arith.let-values
  (list (%nia-coalton 100000 50)
        (%nia-coalton (expt 2 62) 1)
        (multiple-value-list (%nia-floor-tail 7))
        (multiple-value-list (%nia-floor-the 7))
        (%nia-floor-arg 7)
        (%nia-coalton-lcg 42)
        (%nia-coalton-lcg -42)
        (%nia-coalton-lcg (expt 2 70))
        (multiple-value-list (%nia-coalton-lcg 1)))
  (700050 32281802128991715329 (3) (3) 4 1250496027 897012311 12345 (1103527590)))

(deftest-emitting-only nested-integer-arith.let-sil
  (list (%nia-count "Runtime.MultiplyOrMin" #'%nia-coalton)
        (%nia-count "Runtime.BoxU64" #'%nia-coalton-lcg))
  (1 1))

(defvar *nia-special* 1)

(defun %nia-let-order ()
  (let ((log '()))
    (list (let ((a (progn (push :a log) 1))
                (b (progn (push :b log) 2)))
            (+ b a))
          (reverse log))))

(defun %nia-let-capture ()
  (let ((b 10))
    (let ((a (+ b 1)) (b 2))
      (+ a b))))

(defun %nia-let-setq-before ()
  (let ((x 1))
    (let ((a (progn (setq x 5) 1)) (b x))
      (+ a b))))

(defun %nia-let-setq-after ()
  (let ((x 1))
    (let ((a x) (b (progn (setq x 5) 2)))
      (+ a b))))

(defun %nia-let-swap (a b)
  (let ((a b) (b a))
    (- a b)))

(defun %nia-let-special ()
  (let ((*nia-special* 2))
    (+ *nia-special* 0)))

(defun %nia-let-star (x)
  (let* ((a (* x 3)))
    (+ a 1)))

(deftest nested-integer-arith.let-semantics
  (list (%nia-let-order)
        (%nia-let-capture)
        (%nia-let-setq-before)
        (%nia-let-setq-after)
        (%nia-let-swap 10 3)
        (%nia-let-special)
        (%nia-let-star 5))
  ((3 (:a :b)) 13 6 3 -7 2 16))
