;;; Integer arithmetic must not go through the rational path. ADD, SUBTRACT and
;;; MULTIPLY fell through to (Ratio.Make (* an bn) (* ad bd)) for anything that
;;; was not fixnum-by-fixnum, and Ratio.Make reduced by GCD even when the
;;; denominator was 1 -- BigInteger division was 43% of a benchmark whose only
;;; operation is multiplication.
;;;
;;; The results were always right; what these fix is that they stay right now
;;; that integers take a separate route from ratios.

(deftest integer-arith.bignum-multiply
  (let ((f (let ((acc 1)) (dotimes (i 30 acc) (setf acc (* acc (1+ i)))))))
    (list f (integerp f) (typep f 'bignum)))
  (265252859812191058636308480000000 t t))

(deftest integer-arith.mixed-fixnum-bignum
  (let ((big (expt 2 100)))
    (list (* big 3) (+ big 1) (- big 1) (* 3 big) (+ 1 big)))
  (3802951800684688204490109616128
   1267650600228229401496703205377
   1267650600228229401496703205375
   3802951800684688204490109616128
   1267650600228229401496703205377))

(deftest integer-arith.result-narrows-to-fixnum
  (let* ((big (expt 2 70))
         (back (- big (1- (expt 2 70)))))
    (list back (typep back 'fixnum)))
  (1 t))

(deftest integer-arith.signs
  (let ((big (expt 2 80)))
    (list (* -1 big) (* big -1) (- 0 big) (+ (- big) big) (* (- big) (- big))))
  (-1208925819614629174706176
   -1208925819614629174706176
   -1208925819614629174706176
   0
   1461501637330902918203684832716283019655932542976))

;;; Ratios must still reduce, and integer/ratio mixtures must still work.
(deftest integer-arith.ratios-still-reduce
  (list (+ 1/3 1/6) (* 2/3 3/4) (- 1/2 1/2) (/ 4 8) (* 1/2 (expt 2 70)))
  (1/2 1/2 0 1/2 590295810358705651712))

(deftest integer-arith.ratio-with-bignum
  (let ((big (expt 2 70)))
    (list (+ big 1/2) (* big 1/2) (- big 1/2)))
  (2361183241434822606849/2 590295810358705651712 2361183241434822606847/2))

(deftest integer-arith.zero-and-identity
  (let ((big (expt 2 90)))
    (list (* big 0) (* 0 big) (+ big 0) (- big 0) (* big 1)))
  (0 0 1237940039285380274899124224 1237940039285380274899124224 1237940039285380274899124224))
