;;; Regression: (typep X '(signed-byte N)) / (unsigned-byte N) must respect the
;;; exact bit-width bounds even for large N near the fixnum width. A dotcl Fixnum
;;; is a full 64-bit signed long (most-positive-fixnum = 2^63-1), so a large
;;; fixnum can overflow a narrower (signed-byte N). The old code short-circuited
;;; "bits >= 63 => all fixnums fit", which wrongly admitted 2^63-1 into
;;; (signed-byte 63). This surfaced in the SBCL cross-build: genesis cored the
;;; bound 2^63-1 as a truncated fixnum -1 (via %fixnum-descriptor-if-possible),
;;; collapsing distinct integer types onto (integer 0 -1) and tripping a
;;; preload-ctype-hashsets aver.

;;; (signed-byte 63): range [-2^62, 2^62-1]
(deftest sb63.upper.in    (typep 4611686018427387903 '(signed-byte 63)) t)   ; 2^62-1 fits
(deftest sb63.upper.out   (typep 4611686018427387904 '(signed-byte 63)) nil) ; 2^62 does not
(deftest sb63.lower.in    (typep -4611686018427387904 '(signed-byte 63)) t)  ; -2^62 fits
(deftest sb63.lower.out   (typep -4611686018427387905 '(signed-byte 63)) nil); -2^62-1 does not
(deftest sb63.maxfix.out  (typep 9223372036854775807 '(signed-byte 63)) nil) ; 2^63-1 (fixnum max)
(deftest sb63.minfix.out  (typep -9223372036854775808 '(signed-byte 63)) nil); -2^63

;;; (signed-byte 64): range == long range, every fixnum fits
(deftest sb64.maxfix.in   (typep 9223372036854775807 '(signed-byte 64)) t)
(deftest sb64.minfix.in   (typep -9223372036854775808 '(signed-byte 64)) t)

;;; (unsigned-byte 63): range [0, 2^63-1]
(deftest ub63.max.in      (typep 9223372036854775807 '(unsigned-byte 63)) t)   ; 2^63-1 fits
(deftest ub63.over.out    (typep 9223372036854775808 '(unsigned-byte 63)) nil) ; 2^63 (bignum) does not
(deftest ub63.neg.out     (typep -1 '(unsigned-byte 63)) nil)

;;; smaller widths still correct
(deftest sb8.in           (typep 127 '(signed-byte 8)) t)
(deftest sb8.out          (typep 128 '(signed-byte 8)) nil)
(deftest ub8.in           (typep 255 '(unsigned-byte 8)) t)
(deftest ub8.out          (typep 256 '(unsigned-byte 8)) nil)

;;; bignum path across the boundary
(deftest sb100.big.in     (typep (expt 2 98) '(signed-byte 100)) t)
(deftest sb100.big.out    (typep (expt 2 99) '(signed-byte 100)) nil)
