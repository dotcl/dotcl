;;; + - * / and the reciprocal of rationals give the exact result in lowest terms, whether the
;;; operands are small ratios, ratios at the edge of 31 bits, or ratios of a hundred bits; the result
;;; is checked against the same operation spelled with integers only.

(defparameter *rso-operands*
  (list 3/7 -5/11 1/2 -1/2 2/3 7 -7 0 1 -1
        2147483647/2 -2147483647/3 2147483648/3 4294967296/3 4294967297/5 1/4294967297
        (/ (1+ (expt 10 30)) (+ 3 (expt 7 40)))
        (/ (+ 5 (expt 3 70)) (+ 11 (expt 5 50)))
        (- (/ (expt 2 100) 3)) (/ 1 (expt 2 100))))

(defun %rso-parts (x) (values (numerator x) (denominator x)))

(defun %rso-ref (op a b)
  "The result of OP on A and B built from integers only, or :ZERO for a zero divisor."
  (multiple-value-bind (na da) (%rso-parts a)
    (multiple-value-bind (nb db) (%rso-parts b)
      (ecase op
        (:+ (/ (+ (* na db) (* nb da)) (* da db)))
        (:- (/ (- (* na db) (* nb da)) (* da db)))
        (:* (/ (* na nb) (* da db)))
        (:/ (if (zerop nb) :zero (/ (* na db) (* da nb))))))))

(defun %rso-do (op a b)
  (handler-case (ecase op (:+ (+ a b)) (:- (- a b)) (:* (* a b)) (:/ (/ a b)))
    (division-by-zero () :zero)))

(defun %rso-lowest-terms-p (x)
  (or (integerp x)
      (and (rationalp x) (plusp (denominator x)) (= 1 (gcd (numerator x) (denominator x))))))

(deftest rational-ops-small-and-big.binary-ops
  (let ((bad '()))
    (dolist (op '(:+ :- :* :/))
      (dolist (a *rso-operands*)
        (dolist (b *rso-operands*)
          (let ((got (%rso-do op a b)) (want (%rso-ref op a b)))
            (unless (and (eql got want) (or (eq got :zero) (%rso-lowest-terms-p got)))
              (push (list op a b got want) bad))))))
    bad)
  nil)

(deftest rational-ops-small-and-big.reciprocal
  (let ((bad '()))
    (dolist (a *rso-operands*)
      (let ((got (handler-case (/ a) (division-by-zero () :zero)))
            (want (if (zerop a) :zero (/ (denominator a) (numerator a)))))
        (unless (eql got want) (push (list a got want) bad)))
      (let ((got (handler-case (/ 1 a) (division-by-zero () :zero)))
            (want (if (zerop a) :zero (/ (denominator a) (numerator a)))))
        (unless (eql got want) (push (list :one-over a got want) bad))))
    bad)
  nil)

(deftest rational-ops-small-and-big.reduction-and-sign
  (list (+ 1/2 1/2) (* 2/3 3/2) (- 1/3 1/3) (/ 6 4) (/ 1/2 1/4) (/ 3/7 -5/11) (/ -3/7) (* 4 1/4)
        (+ 1/2 3) (- 3 1/2) (* 0 5/7) (/ 0 -5/7)
        (integerp (* 2147483647/2147483646 2147483646/3))
        (* (/ (1+ (expt 10 30)) (+ 3 (expt 7 40))) (/ (+ 3 (expt 7 40)) (1+ (expt 10 30)))))
  (1 1 0 3/2 2 -33/35 -7/3 1 7/2 5/2 0 0 nil 1))

(deftest rational-ops-small-and-big.division-by-zero
  (list (handler-case (/ 3/7 0) (division-by-zero () :zero))
        (handler-case (/ 0) (division-by-zero () :zero))
        (handler-case (/ 1 0) (division-by-zero () :zero))
        (handler-case (/ (/ (1+ (expt 10 30)) (+ 3 (expt 7 40))) 0) (division-by-zero () :zero))
        (handler-case (/ 3/7 (- 3/7 3/7)) (division-by-zero () :zero)))
  (:zero :zero :zero :zero :zero))
