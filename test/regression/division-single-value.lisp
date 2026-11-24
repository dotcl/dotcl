;;; FLOOR, TRUNCATE, CEILING and ROUND of two fixnums where only the quotient is
;;; used take a fixnum quotient without building the remainder. Every operation, every
;;; sign of the operands, the one quotient that does not fit, and a divisor of zero.

(defvar *dsv-values*
  (list 0 1 -1 2 -2 3 -3 7 -7 10 -10 1000003 -1000003
        most-positive-fixnum most-negative-fixnum
        (1- most-positive-fixnum) (1+ most-negative-fixnum)
        4611686018427387904 -4611686018427387904 4294967296 -4294967296))

(defun %dsv-oracle (op a b)
  "The quotient by rational arithmetic, which takes none of the fixnum routes."
  (let ((r (/ a b)))
    (ecase op (:floor (floor r)) (:truncate (truncate r)) (:ceiling (ceiling r)) (:round (round r)))))

(defun %dsv-one (op a b)
  (ecase op
    (:floor (+ 0 (floor a b))) (:truncate (+ 0 (truncate a b)))
    (:ceiling (+ 0 (ceiling a b))) (:round (+ 0 (round a b)))))

(defun %dsv-let (op a b)
  (ecase op
    (:floor (let ((x (floor a b))) x)) (:truncate (let ((x (truncate a b))) x))
    (:ceiling (let ((x (ceiling a b))) x)) (:round (let ((x (round a b))) x))))

(deftest division-single-value.table
  (list (list (%dsv-one :floor 7 2) (%dsv-one :floor -7 2) (%dsv-one :floor 7 -2) (%dsv-one :floor -7 -2))
        (list (%dsv-one :truncate 7 2) (%dsv-one :truncate -7 2) (%dsv-one :truncate 7 -2) (%dsv-one :truncate -7 -2))
        (list (%dsv-one :ceiling 7 2) (%dsv-one :ceiling -7 2) (%dsv-one :ceiling 7 -2) (%dsv-one :ceiling -7 -2))
        (list (%dsv-one :round 7 2) (%dsv-one :round -7 2) (%dsv-one :round 5 2) (%dsv-one :round -5 2)
              (%dsv-one :round 3 2) (%dsv-one :round 9 4) (%dsv-one :round -9 4))
        (list (%dsv-one :floor 6 3) (%dsv-one :ceiling -6 3) (%dsv-one :truncate 0 5) (%dsv-one :round 0 -5)))
  ((3 -4 -4 3) (3 -3 -3 3) (4 -3 -3 4) (4 -4 2 -2 2 2 -2) (2 -2 0 0)))

(deftest division-single-value.agrees-with-rational-arithmetic
  (let ((bad nil))
    (dolist (op '(:floor :truncate :ceiling :round))
      (dolist (a *dsv-values*)
        (dolist (b *dsv-values*)
          (unless (zerop b)
            (let ((want (%dsv-oracle op a b)))
              (unless (and (eql want (%dsv-one op a b)) (eql want (%dsv-let op a b)))
                (push (list op a b want (%dsv-one op a b)) bad)))))))
    (length bad))
  0)

(deftest division-single-value.the-quotient-that-does-not-fit
  (list (%dsv-one :floor most-negative-fixnum -1)
        (%dsv-one :truncate most-negative-fixnum -1)
        (%dsv-one :ceiling most-negative-fixnum -1)
        (%dsv-one :round most-negative-fixnum -1)
        (typep (%dsv-one :floor most-negative-fixnum -1) 'bignum)
        (%dsv-one :floor (1+ most-negative-fixnum) -1)
        (%dsv-one :ceiling most-positive-fixnum 1)
        (%dsv-one :floor most-negative-fixnum most-negative-fixnum)
        (%dsv-one :ceiling most-negative-fixnum most-positive-fixnum))
  (#.(- most-negative-fixnum) #.(- most-negative-fixnum) #.(- most-negative-fixnum) #.(- most-negative-fixnum)
   t #.most-positive-fixnum #.most-positive-fixnum 1 -1))

(deftest division-single-value.division-by-zero
  (list (handler-case (%dsv-one :floor 5 0) (division-by-zero () :zero))
        (handler-case (%dsv-one :truncate -5 0) (division-by-zero () :zero))
        (handler-case (%dsv-one :ceiling 0 0) (division-by-zero () :zero))
        (handler-case (%dsv-one :round most-negative-fixnum 0) (division-by-zero () :zero))
        (handler-case (%dsv-let :floor 5 0) (division-by-zero () :zero)))
  (:zero :zero :zero :zero :zero))

(deftest division-single-value.both-values-are-still-there
  (loop for a in *dsv-values*
        always (loop for b in '(3 -3 7 -7 1000003)
                     always (and (multiple-value-bind (q r) (floor a b) (= a (+ (* q b) r)))
                                 (multiple-value-bind (q r) (truncate a b) (= a (+ (* q b) r)))
                                 (multiple-value-bind (q r) (ceiling a b) (= a (+ (* q b) r)))
                                 (multiple-value-bind (q r) (round a b) (= a (+ (* q b) r)))
                                 (= (nth-value 0 (floor a b)) (%dsv-one :floor a b)))))
  t)

(deftest division-single-value.other-number-types-unchanged
  (list (%dsv-one :floor 7.5d0 2) (%dsv-one :ceiling 7/2 1) (%dsv-one :truncate -7.9 1)
        (%dsv-one :round (expt 10 30) 7) (%dsv-one :floor 7 2.0d0))
  (3 4 -7 #.(round (expt 10 30) 7) 3))

(defun %dsv-bytes (op a b)
  (min-allocated-bytes (lambda () (dotimes (i 1000) (%dsv-one op a b))) 5))

(deftest-compiled-only division-single-value.no-pair-of-values
  (let ((base (%dsv-bytes :floor 6 3)))
    (list (<= (%dsv-bytes :truncate 7 3) base) (<= (%dsv-bytes :ceiling 7 3) base)
          (<= (%dsv-bytes :round 7 3) base) (<= (%dsv-bytes :floor -7 3) base)))
  (t t t t))
