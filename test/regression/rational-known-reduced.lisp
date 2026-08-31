;;; Rational results that cannot need reducing skip the GCD.
;;;
;;; Ratio construction always reduced by a GCD. For two of the operations that
;;; is provably wasted work:
;;;
;;;   integer +/- ratio   the result keeps the ratio's own denominator, and the
;;;                       integer contributes a multiple of it to the numerator,
;;;                       so the two stay as coprime as they already were
;;;   1 / ratio           turning a reduced ratio upside down leaves it reduced
;;;
;;; On the hundred-digit rationals these operations exist for, that GCD was the
;;; whole cost -- fib-ratio is (1+ (/ x)) at every step of a continued fraction.
;;;
;;; The answers must not change, and above all must stay in lowest terms.
;;; Expected values from SBCL.

(defvar *rkr-pairs*
  '((1 1/3) (1/3 1) (-1 1/3) (5 7/2) (7/2 5) (-5 -7/2)
    (1 6/4) (12 100/7) (100/7 12)))

(deftest rational-known-reduced.add
  (mapcar (lambda (c) (+ (first c) (second c))) *rkr-pairs*)
  (4/3 4/3 -2/3 17/2 17/2 -17/2 5/2 184/7 184/7))

(deftest rational-known-reduced.subtract
  (mapcar (lambda (c) (- (first c) (second c))) *rkr-pairs*)
  (2/3 -2/3 -4/3 3/2 -3/2 -3/2 -1/2 -16/7 16/7))

(deftest rational-known-reduced.divide
  (mapcar (lambda (c) (/ (first c) (second c))) *rkr-pairs*)
  (3 1/3 -3 10/7 7/10 10/7 2/3 21/25 25/21))

(deftest rational-known-reduced.reciprocal
  (mapcar (lambda (x) (/ x)) '(1/3 -1/3 7/2 6/4 100/7 355/113 7 -7))
  (3 -3 2/7 2/3 7/100 113/355 1/7 -1/7))

;;; The continued fraction fib-ratio computes: the ratio of consecutive
;;; Fibonacci numbers, which only comes out right if every step stays reduced.
(deftest rational-known-reduced.continued-fraction
  (let ((x 1)) (dotimes (i 12 x) (setq x (1+ (/ x)))))
  377/233)

;;; The invariant the skipped GCD is trusted to preserve, over a wider set:
;;; every rational result is in lowest terms and has a positive denominator.
(deftest rational-known-reduced.always-in-lowest-terms
  (let ((vals (list 1 -1 7 -7 12 -12 1/3 -1/3 7/2 -7/2 6/4 100/7 -100/7
                    355/113 (/ (expt 10 30) 7) (expt 10 20)))
        (bad '()))
    (dolist (a vals (or bad t))
      (dolist (b vals)
        (dolist (r (list (+ a b) (- a b) (* a b)
                         (if (zerop b) 0 (/ a b))
                         (if (zerop a) 0 (/ a))))
          (unless (and (= 1 (gcd (abs (numerator r)) (denominator r)))
                       (plusp (denominator r))
                       (rationalp r))
            (push (list a b r) bad))))))
  t)

;;; An integer result still collapses to an integer rather than staying a ratio.
(deftest rational-known-reduced.integer-results-collapse
  (list (+ 1/2 1/2) (- 3/2 1/2) (/ 1 1/3) (/ 2/3 2/3) (/ 1/4)
        (integerp (+ 1/2 1/2)) (integerp (/ 1/4)))
  (1 1 3 1 4 t t))

;;; Denominators that share a factor: the case the general algorithm exists for.
;;; The special cases it replaced (integer +/- ratio, 1 / ratio) never saw these.
(deftest rational-known-reduced.shared-denominators
  (list (+ 1/6 1/4) (- 1/6 1/4) (* 5/12 8/15) (/ 5/12 8/15)
        (+ 1/6 1/6) (- 1/6 1/6) (+ 5/12 7/12) (* 6/4 2/3)
        (+ 1/1000000007 1/1000000007) (* 355/113 113/355))
  (5/12 -1/12 2/9 25/32 1/3 0 1 1 2/1000000007 1))

;;; A zero result is the integer zero however it arises, not a ratio with a
;;; denominator: the cancelling sum reaches the constructor as 0 over something.
(deftest rational-known-reduced.zero-collapses
  (list (- 1/6 1/6) (+ 1/6 -1/6) (* 0 5/12) (- 5/12 5/12)
        (integerp (- 1/6 1/6)) (integerp (* 0 5/12)))
  (0 0 0 0 t t))
