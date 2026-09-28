;;; (+ a b) / (- a b) / (* a b) of fixnum-typed operands is computed with the
;;; raw int64 op, which wraps. Such a value used to count as fixnum-typed and
;;; was fed to LOGIOR / LOGXOR / LOGNOT / MOD / REM / LOGAND, to the native
;;; comparisons, and to an index, all of which then saw the wrapped value.
;;; x y are (signed-byte 64); the products and sums here leave int64.
;;; Expected values are SBCL's.

(defun %fwc-ior (x y) (declare (type (signed-byte 64) x y)) (logior (* x y) 1))
(deftest fixnum-wrap-consumers.ior
  (list (%fwc-ior 4611686018427387904 4611686018427387904) (%fwc-ior -9223372036854775808 -9223372036854775808) (%fwc-ior -2305843009213693952 5))
  (21267647932558653966460912964485513217 85070591730234615865843651857942052865 -11529215046068469759))

(defun %fwc-xor (x y) (declare (type (signed-byte 64) x y)) (logxor (+ x y) 0))
(deftest fixnum-wrap-consumers.xor
  (list (%fwc-xor 4611686018427387904 4611686018427387904) (%fwc-xor -9223372036854775808 -9223372036854775808) (%fwc-xor -2305843009213693952 5))
  (9223372036854775808 -18446744073709551616 -2305843009213693947))

(defun %fwc-not (x y) (declare (type (signed-byte 64) x y)) (lognot (* x y)))
(deftest fixnum-wrap-consumers.not
  (list (%fwc-not 4611686018427387904 4611686018427387904) (%fwc-not -9223372036854775808 -9223372036854775808) (%fwc-not -2305843009213693952 5))
  (-21267647932558653966460912964485513217 -85070591730234615865843651857942052865 11529215046068469759))

(defun %fwc-mod (x y) (declare (type (signed-byte 64) x y)) (mod (* x y) 7))
(deftest fixnum-wrap-consumers.mod
  (list (%fwc-mod 4611686018427387904 4611686018427387904) (%fwc-mod -9223372036854775808 -9223372036854775808) (%fwc-mod -2305843009213693952 5))
  (2 1 4))

(defun %fwc-rem (x y) (declare (type (signed-byte 64) x y)) (rem (+ x y) 1000))
(deftest fixnum-wrap-consumers.rem
  (list (%fwc-rem 4611686018427387904 4611686018427387904) (%fwc-rem -9223372036854775808 -9223372036854775808) (%fwc-rem -2305843009213693952 5))
  (808 -616 -947))

(defun %fwc-and-the (x y) (declare (type (signed-byte 64) x y)) (logand (the (signed-byte 64) x) (* x y)))
(deftest fixnum-wrap-consumers.and-the
  (list (%fwc-and-the 4611686018427387904 4611686018427387904) (%fwc-and-the -9223372036854775808 -9223372036854775808) (%fwc-and-the -2305843009213693952 5))
  (0 85070591730234615865843651857942052864 -11529215046068469760))

(defun %fwc-and-m1 (x y) (declare (type (signed-byte 64) x y)) (logand (* x y) -1))
(deftest fixnum-wrap-consumers.and-m1
  (list (%fwc-and-m1 4611686018427387904 4611686018427387904) (%fwc-and-m1 -9223372036854775808 -9223372036854775808) (%fwc-and-m1 -2305843009213693952 5))
  (21267647932558653966460912964485513216 85070591730234615865843651857942052864 -11529215046068469760))

(defun %fwc-lt (x y) (declare (type (signed-byte 64) x y)) (if (< (* x y) 0) :neg :nonneg))
(deftest fixnum-wrap-consumers.lt
  (list (%fwc-lt 4611686018427387904 4611686018427387904) (%fwc-lt -9223372036854775808 -9223372036854775808) (%fwc-lt -2305843009213693952 5))
  (:NONNEG :NONNEG :NEG))

(defun %fwc-zerop (x y) (declare (type (signed-byte 64) x y)) (if (zerop (* x y)) :z :nz))
(deftest fixnum-wrap-consumers.zerop
  (list (%fwc-zerop 4611686018427387904 4611686018427387904) (%fwc-zerop -9223372036854775808 -9223372036854775808) (%fwc-zerop -2305843009213693952 5))
  (:NZ :NZ :NZ))

(defun %fwc-minusp (x y) (declare (type (signed-byte 64) x y)) (if (minusp (+ x y)) :m :nm))
(deftest fixnum-wrap-consumers.minusp
  (list (%fwc-minusp 4611686018427387904 4611686018427387904) (%fwc-minusp -9223372036854775808 -9223372036854775808) (%fwc-minusp -2305843009213693952 5))
  (:NM :M :M))

(defun %fwc-gt2 (x y) (declare (type (signed-byte 64) x y)) (if (> (* x x) (+ y y)) :gt :le))
(deftest fixnum-wrap-consumers.gt2
  (list (%fwc-gt2 4611686018427387904 4611686018427387904) (%fwc-gt2 -9223372036854775808 -9223372036854775808) (%fwc-gt2 -2305843009213693952 5))
  (:GT :GT :GT))

(defun %fwc-eq-xor (x y) (declare (type (signed-byte 64) x y)) (if (= (logxor (* x y) 0) (* x y)) :same :diff))
(deftest fixnum-wrap-consumers.eq-xor
  (list (%fwc-eq-xor 4611686018427387904 4611686018427387904) (%fwc-eq-xor -9223372036854775808 -9223372036854775808) (%fwc-eq-xor -2305843009213693952 5))
  (:SAME :SAME :SAME))

(defun %fwc-let-ior (x y) (declare (type (signed-byte 64) x y)) (let ((z (logior (* x y) 1))) (list z (1+ z))))
(deftest fixnum-wrap-consumers.let-ior
  (list (%fwc-let-ior 4611686018427387904 4611686018427387904) (%fwc-let-ior -9223372036854775808 -9223372036854775808) (%fwc-let-ior -2305843009213693952 5))
  ((21267647932558653966460912964485513217 21267647932558653966460912964485513218) (85070591730234615865843651857942052865 85070591730234615865843651857942052866) (-11529215046068469759 -11529215046068469758)))

(defun %fwc-svref-mod (x y) (declare (type (signed-byte 64) x y)) (svref (vector 0 1 2 3) (mod (* x y) 4)))
(deftest fixnum-wrap-consumers.svref-mod
  (list (%fwc-svref-mod 4611686018427387904 4611686018427387904) (%fwc-svref-mod -9223372036854775808 -9223372036854775808) (%fwc-svref-mod -2305843009213693952 5))
  (0 0 0))

(defun %fwc-ash-ior (x y) (declare (type (signed-byte 64) x y)) (ash (logior (* x y) 1) -1))
(deftest fixnum-wrap-consumers.ash-ior
  (list (%fwc-ash-ior 4611686018427387904 4611686018427387904) (%fwc-ash-ior -9223372036854775808 -9223372036854775808) (%fwc-ash-ior -2305843009213693952 5))
  (10633823966279326983230456482242756608 42535295865117307932921825928971026432 -5764607523034234880))

(defun %fwc-dpb (x y) (declare (type (signed-byte 64) x y)) (dpb 1 (byte 1 0) (* x y)))
(deftest fixnum-wrap-consumers.dpb
  (list (%fwc-dpb 4611686018427387904 4611686018427387904) (%fwc-dpb -9223372036854775808 -9223372036854775808) (%fwc-dpb -2305843009213693952 5))
  (21267647932558653966460912964485513217 85070591730234615865843651857942052865 -11529215046068469759))

(defun %fwc-add-xor (x y) (declare (type (signed-byte 64) x y)) (+ (logxor (* x y) 0) 0))
(deftest fixnum-wrap-consumers.add-xor
  (list (%fwc-add-xor 4611686018427387904 4611686018427387904) (%fwc-add-xor -9223372036854775808 -9223372036854775808) (%fwc-add-xor -2305843009213693952 5))
  (21267647932558653966460912964485513216 85070591730234615865843651857942052864 -11529215046068469760))

(defun %fwc-if-arms (x y) (declare (type (signed-byte 64) x y)) (logior (if (> x 0) (* x y) (+ x y)) 0))
(deftest fixnum-wrap-consumers.if-arms
  (list (%fwc-if-arms 4611686018427387904 4611686018427387904) (%fwc-if-arms -9223372036854775808 -9223372036854775808) (%fwc-if-arms -2305843009213693952 5))
  (21267647932558653966460912964485513216 -18446744073709551616 -2305843009213693947))

(defun %fwc-and-small (x y) (declare (type (signed-byte 64) x y)) (logand (+ x y) 65535))
(deftest fixnum-wrap-consumers.and-small
  (list (%fwc-and-small 4611686018427387904 4611686018427387904) (%fwc-and-small -9223372036854775808 -9223372036854775808) (%fwc-and-small -2305843009213693952 5))
  (0 0 5))

;;; A mask wider than int64 is not a non-negative fixnum operand.
(defun %fwc-bigmask (x y) (declare (type (signed-byte 64) x y)) (logand (* x y) #xFFFFFFFFFFFFFFFFF))
(deftest fixnum-wrap-consumers.bignum-mask
  (list (%fwc-bigmask 4611686018427387904 4611686018427387904) (%fwc-bigmask -5 7))
  (0 295147905179352825821))

;;; What must stay native: a LOGAND with a non-negative mask is exact whatever
;;; the other operand wrapped to, and a declared FIXNUM accumulator keeps its
;;; Int64 slot.
(setf dotcl:*save-sil* t)
(defun %fwc-sil (fn) (princ-to-string (dotcl:function-sil fn)))
(defun %fwc-mask-sil (x y)
  (declare (fixnum x y) (optimize (speed 3) (safety 0)))
  (logand (+ x y) #xFFFFFFFF))
(defun %fwc-acc-sil (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0)))
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i n s)
      (setq s (logand (+ s (* i 3)) #xFFFFFF)))))
(deftest-emitting-only fixnum-wrap-consumers.mask-stays-native
  (let ((a (%fwc-sil #'%fwc-mask-sil)) (b (%fwc-sil #'%fwc-acc-sil)))
    (list (and (search "(AND)" a) t) (search "Runtime.Logand" a)
          (and (search "(AND)" b) t) (search "Runtime.Logand" b)))
  (t nil t nil))
(setf dotcl:*save-sil* nil)

(deftest fixnum-wrap-consumers.mask-values
  (list (%fwc-mask-sil most-positive-fixnum most-positive-fixnum)
        (%fwc-acc-sil 1000))
  (4294967294 1498500))
