;;; Modular arithmetic reached through declarations the range machinery used to
;;; miss: (AND <integer type> UNSIGNED-BYTE), a DEFTYPE naming an integer type,
;;; (MOD N), an integer literal above 2^62, an n-ary LOGXOR, and a LET* binding
;;; whose inferred range a later init in the same LET* uses. The shape of the
;;; first test is SBCL's MIX hash. Masks are literals so the expected values
;;; (SBCL's) do not depend on MOST-POSITIVE-FIXNUM.

(deftype %mad-fix62 () '(signed-byte 62))

(defun %mad-mix (x y)
  (declare (type (and %mad-fix62 unsigned-byte) x y))
  (let* ((mul (logand 3622009729038463111 #x3fffffffffffffff))
         (xor (logand 608948948376289905 #x3fffffffffffffff))
         (xy (logand (+ (* x mul) y) #x3fffffffffffffff)))
    (logand (logxor xor xy (ash xy -5)) #x3fffffffffffffff)))
(deftest modular-arith-declared-types.mix
  (list (%mad-mix 0 0) (%mad-mix 123456789 987654321)
        (%mad-mix 2305843009213693951 2305843009213693951) (%mad-mix 1 2305843009213693951))
  (608948948376289905 2118281863048926019 406030721959556978 1919240196756043635))

(defun %mad-and-fixnum (x y)
  (declare (type (and fixnum unsigned-byte) x y))
  (logand (+ (* x 3622009729038463111) y) #x7fffffffffffffff))
(deftest modular-arith-declared-types.and-fixnum
  (list (%mad-and-fixnum 5 7) (%mad-and-fixnum 4611686018427387903 4611686018427387903))
  (8886676608337539754 5601362307816312696))

(defun %mad-big-literal (x)
  (declare (fixnum x))
  (logand (+ x 5000000000000000000) #xffffffff))
(deftest modular-arith-declared-types.big-literal
  (list (%mad-big-literal 0) (%mad-big-literal 4611686018427387903) (%mad-big-literal -4611686018427387904))
  (1156841472 1156841471 1156841472))

;; A LET* binding with an inferred range, used by a later init whose product
;; does not fit: the product must promote, not wrap.
(defun %mad-let*-promote (x)
  (declare (type %mad-fix62 x))
  (let* ((a (* x 3))
         (b (* a a)))
    b))
(deftest modular-arith-declared-types.let*-promote
  (list (%mad-let*-promote 7) (%mad-let*-promote 2305843009213693951) (%mad-let*-promote -2305843009213693952))
  (441 47852207848256971383031880004245913609 47852207848256971424537054170092404736))

;; n-ary LOGIOR with an operand that is not a fixnum stays exact.
(defun %mad-nary-bignum (x y)
  (declare (fixnum x y))
  (logior x y #x10000000000000000))
(deftest modular-arith-declared-types.nary-bignum
  (list (%mad-nary-bignum 1 2) (%mad-nary-bignum -1 0))
  (18446744073709551619 -1))

(defun %mad-the (x)
  (logand (* (the (and fixnum unsigned-byte) x) 1000003) #xffff))
(deftest modular-arith-declared-types.the
  (list (%mad-the 0) (%mad-the 99999999999) (%mad-the 4611686018427387903))
  (0 30141 48573))

(defun %mad-mod-type (i)
  (declare (type (mod 10) i))
  (* i 1000000000000000000))
(deftest modular-arith-declared-types.mod-type
  (list (%mad-mod-type 0) (%mad-mod-type 9))
  (0 9000000000000000000))

(defun %mad-half-open (x)
  (declare (type (and (integer 0 *) (signed-byte 32)) x))
  (* x x x))
(deftest modular-arith-declared-types.half-open
  (list (%mad-half-open 3) (%mad-half-open 2147483647))
  (27 9903520300447984150353281023))
