;;; MOD and REM of a fixnum by a bignum did a BigInteger division. A bignum
;;; whose magnitude is over 2^63 is larger than any fixnum, so the answer is
;;; the fixnum, or for MOD with opposite signs the fixnum plus the bignum.
;;; Coalton keeps its UFix arithmetic in 64 bits with (mod x (expt 2 64)) after
;;; every operation. 2^63 itself is at the edge: (rem most-negative-fixnum
;;; (expt 2 63)) is 0.

(defun %mrf (a b) (list (mod a b) (rem a b)))

(deftest mod-rem-fixnum-by-bignum.values
  (loop for b in (list (expt 2 64) (- (expt 2 64)) (expt 2 63) (- (expt 2 63)) (1- (- (expt 2 63))))
        collect (loop for a in (list 0 1 -1 7 -7 most-positive-fixnum most-negative-fixnum)
                      collect (%mrf a b)))
  (((0 0) (1 1) (18446744073709551615 -1) (7 7) (18446744073709551609 -7)
    (9223372036854775807 9223372036854775807) (9223372036854775808 -9223372036854775808))
   ((0 0) (-18446744073709551615 1) (-1 -1) (-18446744073709551609 7) (-7 -7)
    (-9223372036854775809 9223372036854775807) (-9223372036854775808 -9223372036854775808))
   ((0 0) (1 1) (9223372036854775807 -1) (7 7) (9223372036854775801 -7)
    (9223372036854775807 9223372036854775807) (0 0))
   ((0 0) (-9223372036854775807 1) (-1 -1) (-9223372036854775801 7) (-7 -7)
    (-1 9223372036854775807) (0 0))
   ((0 0) (-9223372036854775808 1) (-1 -1) (-9223372036854775802 7) (-7 -7)
    (-2 9223372036854775807) (-9223372036854775808 -9223372036854775808))))

(defun %mrf-bytes () (nth 4 (dotcl:gc-stats)))
(defun %mrf-wrap (x) (mod x 18446744073709551616))
(defvar *mrf-v* #x123456789A)

(deftest-compiled-only mod-rem-fixnum-by-bignum.no-allocation
  (let ((v *mrf-v*) (s 0))
    (dotimes (k 100) (%mrf-wrap v))
    (let ((b0 (%mrf-bytes)))
      (dotimes (k 100000) (setq s (logand (+ s (if (eql (%mrf-wrap v) v) 1 0)) 1023)))
      (list s (< (/ (- (%mrf-bytes) b0) 100000) 8))))
  (672 t))
