;;; FLOOR / CEILING / TRUNCATE / ROUND, their F variants, MOD and REM with a
;;; float operand.
;;;
;;; CLHS says MOD is the second value of FLOOR and REM the second value of
;;; TRUNCATE. dotcl computed FLOOR and friends on the exact rational ratio of the
;;; two binary values ((floor 1d0 0.1d0) => 9, 0.09999999999999998d0, since
;;; 0.1d0 is a little above 1/10) but MOD and REM in floating point
;;; ((mod 1d0 0.1d0) => 0d0), so the two disagreed. All of them now divide in the
;;; float format and round the float quotient, which is what SBCL does: 1d0 /
;;; 0.1d0 rounds to exactly 10d0, so FLOOR is 10 and 0d0.
;;;
;;; The expected values were produced by SBCL 2.6.8. A float is compared by its
;;; format and INTEGER-DECODE-FLOAT (a zero by its sign), so equal digits are
;;; not enough and a single differing bit fails the test.

(defun ffm-enc (x)
  (if (floatp x)
      (list (if (typep x 'double-float) :d :s)
            (if (zerop x)
                (list :zero (floor (float-sign x)))
                (multiple-value-list (integer-decode-float x))))
      x))

(defparameter *ffm-cases*
  '(
  ((FLOOR 1.0d0 0.1d0) (10 (:D (:ZERO 1))))
  ((CEILING 1.0d0 0.1d0) (10 (:D (:ZERO 1))))
  ((TRUNCATE 1.0d0 0.1d0) (10 (:D (:ZERO 1))))
  ((ROUND 1.0d0 0.1d0) (10 (:D (:ZERO 1))))
  ((FFLOOR 1.0d0 0.1d0) ((:D (5629499534213120 -49 1)) (:D (:ZERO 1))))
  ((FTRUNCATE 1.0d0 0.1d0) ((:D (5629499534213120 -49 1)) (:D (:ZERO 1))))
  ((MOD 1.0d0 0.1d0) ((:D (:ZERO 1))))
  ((REM 1.0d0 0.1d0) ((:D (:ZERO 1))))
  ((FLOOR 1.0 0.1) (10 (:S (:ZERO 1))))
  ((MOD 1.0 0.1) ((:S (:ZERO 1))))
  ((FLOOR 1 0.1) (10 (:S (:ZERO 1))))
  ((FLOOR 1.0d0 1/10) (10 (:D (:ZERO 1))))
  ((FLOOR 1/3 0.1d0) (3 (:D (4803839602528520 -57 1))))
  ((FLOOR -1.0d0 0.1d0) (-10 (:D (:ZERO 1))))
  ((MOD -1.0d0 0.1d0) ((:D (:ZERO 1))))
  ((FLOOR 0.3d0 0.1d0) (2 (:D (7205759403792792 -56 1))))
  ((MOD 0.3d0 0.1d0) ((:D (7205759403792792 -56 1))))
  ((FLOOR 0.7d0 0.1d0) (6 (:D (7205759403792784 -56 1))))
  ((MOD 0.7d0 0.1d0) ((:D (7205759403792784 -56 1))))
  ((REM -0.7d0 0.1d0) ((:D (7205759403792784 -56 -1))))
  ((FLOOR 3.7d0 -1.1d0) (-4 (:D (6305039478318696 -53 -1))))
  ((CEILING 3.7 1.1) (4 (:S (11744052 -24 -1))))
  ((ROUND 2.5d0 1) (2 (:D (4503599627370496 -53 1))))
  ((ROUND 3.5d0 1) (4 (:D (4503599627370496 -53 -1))))
  ((ROUND -2.5 1) (-2 (:S (8388608 -24 -1))))
  ((FROUND 1.0 -2.0) ((:S (:ZERO -1)) (:S (8388608 -23 1))))
  ((FROUND 0.5d0 1) ((:D (:ZERO 1)) (:D (4503599627370496 -53 1))))
  ((FTRUNCATE -0.5d0 1) ((:D (:ZERO -1)) (:D (4503599627370496 -53 -1))))
  ((FFLOOR 5.5 0.3) ((:S (9437184 -19 1)) (:S (13421760 -27 1))))
  ((FLOOR 1.0 0.1d0) (10 (:D (:ZERO 1))))
  ((FLOOR 123.456d0 0.001d0) (123456 (:D (:ZERO 1))))
  ((MOD 123.456 0.001) ((:S (8519680 -33 1))))
  ((FLOOR 1.0d10 0.3d0) (33333333333 (:D (7205786891583488 -56 1))))
  ((MOD 1.0d10 0.3d0) ((:D (7205786891583488 -56 1))))
    ))

(deftest float-floor-mod.matches-sbcl
  (loop for (form expected) in *ffm-cases*
        for got = (mapcar #'ffm-enc
                          (multiple-value-list (apply (first form) (rest form))))
        unless (equal got expected)
          collect (list form got expected))
  nil)

;;; The consistency CLHS requires, over a spread of float, mixed and
;;; single/double pairs: MOD is FLOOR's remainder and REM is TRUNCATE's, bit
;;; for bit, and each F variant has the same remainder as its integer twin.
(defparameter *ffm-numbers*
  '(1.0d0 -1.0d0 0.3d0 -0.7d0 2.5d0 3.7d0 1d10 123.456d0
    1.0 -1.0 0.3 -0.7 2.5 3.7 1e10 123.456
    1 -3 10 1/3 -7/2))

(defparameter *ffm-divisors*
  '(0.1d0 -0.1d0 0.3d0 1.1d0 -2.0d0 1d-3
    0.1 -0.1 0.3 1.1 -2.0 1e-3
    2 -3 1/10 7/2))

(deftest float-floor-mod.second-values-agree
  (loop for a in *ffm-numbers*
        nconc (loop for b in *ffm-divisors*
                    when (and (or (floatp a) (floatp b))
                              (not (and (equal (ffm-enc (mod a b))
                                               (ffm-enc (nth-value 1 (floor a b))))
                                        (equal (ffm-enc (rem a b))
                                               (ffm-enc (nth-value 1 (truncate a b))))
                                        (equal (ffm-enc (nth-value 1 (ffloor a b)))
                                               (ffm-enc (nth-value 1 (floor a b))))
                                        (equal (ffm-enc (nth-value 1 (fceiling a b)))
                                               (ffm-enc (nth-value 1 (ceiling a b))))
                                        (equal (ffm-enc (nth-value 1 (ftruncate a b)))
                                               (ffm-enc (nth-value 1 (truncate a b))))
                                        (equal (ffm-enc (nth-value 1 (fround a b)))
                                               (ffm-enc (nth-value 1 (round a b)))))))
                      collect (list a b)))
  nil)

;;; The case the discrepancy was reported with, spelled out.
(deftest float-floor-mod.floor-1d0-0.1d0
  (multiple-value-list (floor 1d0 0.1d0))
  (10 0d0))

(deftest float-floor-mod.mod-1d0-0.1d0
  (mod 1d0 0.1d0)
  0d0)
