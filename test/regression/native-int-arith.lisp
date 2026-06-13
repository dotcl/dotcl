;;; Regression tests for native int64 arithmetic on bounded-integer-typed
;;; values ((signed-byte N) / (unsigned-byte N) / bit declarations and
;;; let-inferred ranges). The native path must stay CL-compliant: any
;;; result that can exceed fixnum must fall back to the promoting path and
;;; yield a bignum, never a silently-wrapped int64. (D-native-int-arith)
;;;
;;; Strategy: each native-typed function is compared against an undeclared
;;; twin that always takes the generic (boxed, promoting) path, so a silent
;;; native wrap shows up as typed /= generic. Magnitude / hardcoded-bignum
;;; checks catch a bug that would corrupt both paths identically.

;;; ---- helpers: typed (native-eligible) vs generic (boxed) twins ----

(defun %sb56-mul-typed (a b)
  (declare (type (signed-byte 56) a b))
  (* a b))
(defun %sb56-mul-generic (a b)
  (* a b))

(defun %sb56-times2-typed (a)
  (declare (type (signed-byte 56) a))
  (* a 2))

(defun %sb56-add-typed (a b)
  (declare (type (signed-byte 56) a b))
  (+ a b))
(defun %sb56-add-generic (a b)
  (+ a b))

(defun %ub32-mul-typed (a b)
  (declare (type (unsigned-byte 32) a b))
  (* a b))
(defun %ub32-mul-generic (a b)
  (* a b))

;;; ---- (signed-byte 56) multiply: 56*56 = up to 111 bits, must promote ----

;; (2^55-1)^2 is far outside int64; native wrap would give garbage.
(deftest native-sb56-mul-overflow-matches-generic
  (equal (%sb56-mul-typed 36028797018963967 36028797018963967)
         (%sb56-mul-generic 36028797018963967 36028797018963967))
  t)

(deftest native-sb56-mul-overflow-is-bignum
  (> (%sb56-mul-typed 36028797018963967 36028797018963967)
     most-positive-fixnum)
  t)

;; Negative operands.
(deftest native-sb56-mul-neg-matches-generic
  (equal (%sb56-mul-typed -36028797018963968 36028797018963967)
         (%sb56-mul-generic -36028797018963968 36028797018963967))
  t)

;; Small products stay correct (whichever path is taken).
(deftest native-sb56-mul-small
  (%sb56-mul-typed 123456 -654321)
  -80779853376)

;;; ---- native-safe path: (* a 2) of a (signed-byte 56) always fits int64 ----

(deftest native-sb56-times2-max
  (%sb56-times2-typed 36028797018963967)
  72057594037927934)

(deftest native-sb56-times2-min
  (%sb56-times2-typed -36028797018963968)
  -72057594037927936)

;;; ---- addition near the fixnum boundary must promote ----

(deftest native-sb56-add-matches-generic
  (equal (%sb56-add-typed 36028797018963967 36028797018963967)
         (%sb56-add-generic 36028797018963967 36028797018963967))
  t)

;;; ---- (unsigned-byte 32) multiply: 64 bits, fits int64, value correct ----

(deftest native-ub32-mul-max
  (%ub32-mul-typed 4294967295 4294967295)
  18446744065119617025)

(deftest native-ub32-mul-matches-generic
  (equal (%ub32-mul-typed 4294967295 4294967295)
         (%ub32-mul-generic 4294967295 4294967295))
  t)

;;; ---- let-inferred ranges (the new-rmdr pattern from crc40) ----

(defun %let-infer-typed (a)
  (declare (type (signed-byte 56) a))
  (let ((y (logior 1 (* a 2))))   ; init range provable, fits int64
    (logand y #xff)))
(defun %let-infer-generic (a)
  (let ((y (logior 1 (* a 2))))
    (logand y #xff)))

(deftest native-let-infer-matches-generic
  (equal (%let-infer-typed 36028797018963967)
         (%let-infer-generic 36028797018963967))
  t)

;;; ---- mutation soundness: a mutated let var must NOT keep its init range ----
;;; If the compiler treated X as small-range from its init (1) after the setf,
;;; (* x x) would wrap in int64. 5e9^2 = 2.5e19 > 2^63, so it must promote.

(defun %let-mut-mul-overflow ()
  (let ((x 1))
    (setf x 5000000000)
    (* x x)))

(deftest native-let-mut-mul-promotes
  (%let-mut-mul-overflow)
  25000000000000000000)

;; Reading a mutated small-init var that now holds a bignum must not unbox-fixnum.
(defun %let-mut-bignum ()
  (let ((x 5))
    (setf x 1000000000000000000000000)
    (+ x 1)))

(deftest native-let-mut-bignum-safe
  (%let-mut-bignum)
  1000000000000000000000001)

;;; ---- (the (signed-byte N) E) with N wider than int64 must NOT native-unbox ----
;;; A (signed-byte 100) value can be a bignum; bitwise ops skip the overflow
;;; gate, so treating it as int64-unboxable would corrupt the value.

(defun %the-wide-logand (x)
  (logand (the (signed-byte 100) x) 255))

(deftest native-the-wide-logand-no-corrupt
  (%the-wide-logand 1267650600228229401496703205376)  ; 2^100, low byte = 0
  0)

(defun %the-wide-add (x)
  (+ (the (signed-byte 100) x) 1))

(deftest native-the-wide-add-no-corrupt
  (%the-wide-add 1267650600228229401496703205376)
  1267650600228229401496703205377)

;;; (the (signed-byte 56) E) — fits int64, native path, value correct.
(defun %the-narrow-logand (x)
  (logand (the (signed-byte 56) x) 255))

(deftest native-the-narrow-logand
  (%the-narrow-logand 36028797018963967)  ; (2^55-1) low byte = 255
  255)

;;; ---- crc40 reproduction: typed (native) vs untyped (generic) twins ----

(defun %crc-step-typed (bit rmdr poly msb-mask)
  (declare (type (signed-byte 56) rmdr poly msb-mask)
           (type bit bit))
  (let ((new-rmdr (logior bit (* rmdr 2))))
    (if (zerop (logand msb-mask new-rmdr))
        new-rmdr
        (logxor new-rmdr poly))))

(defun %crc-step-generic (bit rmdr poly msb-mask)
  (let ((new-rmdr (logior bit (* rmdr 2))))
    (if (zerop (logand msb-mask new-rmdr))
        new-rmdr
        (logxor new-rmdr poly))))

(defun %compute-adjustment-typed (poly n)
  (declare (type (signed-byte 56) poly) (fixnum n))
  (let* ((mask (ash 1 (1- (integer-length poly))))
         (rmdr (%crc-step-typed 1 0 poly mask)))
    (dotimes (k (- n 1))
      (setf rmdr (%crc-step-typed 0 rmdr poly mask)))
    rmdr))

(defun %compute-adjustment-generic (poly n)
  (let* ((mask (ash 1 (1- (integer-length poly))))
         (rmdr (%crc-step-generic 1 0 poly mask)))
    (dotimes (k (- n 1))
      (setf rmdr (%crc-step-generic 0 rmdr poly mask)))
    rmdr))

(deftest native-crc-step-matches-generic
  (equal (%crc-step-typed 1 123456789 1099587256329 (ash 1 40))
         (%crc-step-generic 1 123456789 1099587256329 (ash 1 40)))
  t)

(deftest native-crc-adjustment-matches-generic
  (equal (%compute-adjustment-typed 1099587256329 3014633)
         (%compute-adjustment-generic 1099587256329 3014633))
  t)

;; crc remainder stays within the polynomial's bit width (a positive fixnum).
(deftest native-crc-adjustment-in-range
  (let ((r (%compute-adjustment-typed 1099587256329 3014633)))
    (and (integerp r) (>= r 0) (< r 1099587256329)))
  t)
