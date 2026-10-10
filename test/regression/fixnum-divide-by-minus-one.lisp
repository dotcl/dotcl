;;; Dividing most-negative-fixnum by -1.
;;;
;;; A fixnum is an int64, so (- most-negative-fixnum) is a bignum, and the int64
;;; division and remainder the fixnum fast paths use overflow on exactly this
;;; pair. FLOOR / CEILING / TRUNCATE / FFLOOR / FCEILING / FTRUNCATE / MOD / REM
;;; and two-argument / signalled "Arithmetic operation resulted in an overflow."
;;; instead of answering. ROUND and FROUND took the BigInteger path and were
;;; right. A divisor of -1 now negates.
;;;
;;; Expected values read off SBCL 2.6.8 (-2^63 is a bignum there; the answers
;;; are the same).

(defvar *fdm1-fns* '(floor ceiling truncate round ffloor fceiling ftruncate fround mod rem /))

(defun %fdm1-apply (a b)
  (mapcar (lambda (f) (multiple-value-list (funcall f a b))) *fdm1-fns*))

(deftest fixnum-divide-by-minus-one.generic
  (%fdm1-apply most-negative-fixnum -1)
  ((9223372036854775808 0) (9223372036854775808 0) (9223372036854775808 0)
   (9223372036854775808 0)
   (9.223372e18 0) (9.223372e18 0) (9.223372e18 0) (9.223372e18 0)
   (0) (0) (9223372036854775808)))

(deftest fixnum-divide-by-minus-one.by-one
  (%fdm1-apply most-negative-fixnum 1)
  ((-9223372036854775808 0) (-9223372036854775808 0) (-9223372036854775808 0)
   (-9223372036854775808 0)
   (-9.223372e18 0) (-9.223372e18 0) (-9.223372e18 0) (-9.223372e18 0)
   (0) (0) (-9223372036854775808)))

(deftest fixnum-divide-by-minus-one.small
  (%fdm1-apply -4611686018427387904 -1)
  ((4611686018427387904 0) (4611686018427387904 0) (4611686018427387904 0)
   (4611686018427387904 0)
   (4.611686e18 0) (4.611686e18 0) (4.611686e18 0) (4.611686e18 0)
   (0) (0) (4611686018427387904)))

;; Declared fixnum operands: both values, and the first value alone. Defined
;; at top level (no COMPILE) so the emit-free build runs it too.
(defmacro %fdm1-define (f)
  (let ((mv (intern (format nil "%FDM1-MV-~a" f)))
        (sv (intern (format nil "%FDM1-SV-~a" f))))
    `(progn
       (defun ,mv (a b) (declare (fixnum a b)) (,f a b))
       (defun ,sv (a b) (declare (fixnum a b)) (values (,f a b)))
       (list ',mv ',sv))))

(defvar *fdm1-declared*
  (list (%fdm1-define floor) (%fdm1-define ceiling) (%fdm1-define truncate)
        (%fdm1-define round) (%fdm1-define ffloor) (%fdm1-define fceiling)
        (%fdm1-define ftruncate) (%fdm1-define fround) (%fdm1-define mod)
        (%fdm1-define rem) (%fdm1-define /)))

(deftest fixnum-divide-by-minus-one.declared
  (mapcar (lambda (fs)
            (list (multiple-value-list (funcall (first fs) most-negative-fixnum -1))
                  (funcall (second fs) most-negative-fixnum -1)))
          *fdm1-declared*)
  (((9223372036854775808 0) 9223372036854775808)
   ((9223372036854775808 0) 9223372036854775808)
   ((9223372036854775808 0) 9223372036854775808)
   ((9223372036854775808 0) 9223372036854775808)
   ((9.223372e18 0) 9.223372e18)
   ((9.223372e18 0) 9.223372e18)
   ((9.223372e18 0) 9.223372e18)
   ((9.223372e18 0) 9.223372e18)
   ((0) 0) ((0) 0)
   ((9223372036854775808) 9223372036854775808)))

;; MOD / REM inside a fixnum-typed expression stay in int64.
(defun %fdm1-mod-sum (a b)
  (declare (fixnum a b))
  (+ (the fixnum (mod a b)) (the fixnum (rem a b)) 1))

(deftest fixnum-divide-by-minus-one.typed-mod-rem
  (list (%fdm1-mod-sum most-negative-fixnum -1)
        (%fdm1-mod-sum -7 -1)
        (%fdm1-mod-sum -7 2))
  (1 1 1))
