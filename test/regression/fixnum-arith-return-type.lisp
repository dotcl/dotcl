;;; What a function whose body is (+ a b) returns.
;;;
;;; Declaring the PARAMETERS fixnum says nothing about the result: FIXNUM +
;;; FIXNUM spans one bit more than a fixnum, so the value may be a bignum, and
;;; in Common Lisp it simply is one. The return-type inference used to answer
;;; FIXNUM for it anyway. That answer reached *FUNCTION-RETURN-TYPES*, so a
;;; caller binding the call in a LET took an Int64 slot for it and then could
;;; not unbox the bignum the callee had correctly returned -- the call worked,
;;; and (LET ((R (F ...))) ...) around the very same call raised a cast error.
;;;
;;; These run at SAFETY 1. At SAFETY 0 a writer may license the narrower type
;;; with (THE FIXNUM ...), and what happens when that assertion is violated is
;;; undefined, so there is nothing to pin down there.

(defun fart-add (a b)
  (declare (fixnum a b) (optimize (speed 3) (safety 1) (debug 0)))
  (+ a b))

(defun fart-sub (a b)
  (declare (fixnum a b) (optimize (speed 3) (safety 1) (debug 0)))
  (- a b))

(defun fart-mul (a b)
  (declare (fixnum a b) (optimize (speed 3) (safety 1) (debug 0)))
  (* a b))

;;; Ordinary values are unaffected.

(deftest fixnum-arith-return-ordinary-values
  (list (fart-add 3 4) (fart-sub 10 3) (fart-mul 6 7))
  (7 7 42))

;;; The shape that used to fail: bind the overflowing result to a variable.

(deftest fixnum-arith-return-let-bound-overflow-is-a-bignum
  (let ((r (fart-add most-positive-fixnum 1)))
    (list (integerp r) (= r (1+ most-positive-fixnum)) (typep r 'fixnum)))
  (t t nil))

(deftest fixnum-arith-return-let-bound-negative-overflow
  (let ((r (fart-sub most-negative-fixnum 1)))
    (list (integerp r) (= r (1- most-negative-fixnum))))
  (t t))

(deftest fixnum-arith-return-let-bound-product-overflow
  (let ((r (fart-mul most-positive-fixnum 2)))
    (list (integerp r) (= r (* most-positive-fixnum 2))))
  (t t))

;;; Calling without binding was always right, and has to stay right.

(deftest fixnum-arith-return-unbound-call-still-right
  (= (fart-add most-positive-fixnum 1) (1+ most-positive-fixnum))
  t)

;;; Arithmetic that cannot leave the range keeps the narrow type. A bitwise op
;;; on two fixnums is a fixnum by construction, and so is the smaller of two.

(defun fart-mask (a b)
  (declare (fixnum a b) (optimize (speed 3) (safety 1) (debug 0)))
  (logand (+ a b) 255))

(defun fart-min (a b)
  (declare (fixnum a b) (optimize (speed 3) (safety 1) (debug 0)))
  (min a b))

(deftest fixnum-arith-return-closed-operations
  (let ((m (fart-mask most-positive-fixnum 1))
        (n (fart-min 3 4)))
    (list m n (typep m 'fixnum) (typep n 'fixnum)))
  (0 3 t t))

;;; (THE FIXNUM ...) still licenses the narrow type: the assertion is the
;;; writer's, not something inference invented.

(defun fart-the (a b)
  (declare (fixnum a b) (optimize (speed 3) (safety 1) (debug 0)))
  (the fixnum (+ a b)))

(deftest fixnum-arith-return-the-still-narrows
  (let ((r (fart-the 3 4))) (list r (typep r 'fixnum)))
  (7 t))

;;; ABS is in the same family: (abs most-negative-fixnum) is not a fixnum.

(defun fart-abs (a)
  (declare (fixnum a) (optimize (speed 3) (safety 1) (debug 0)))
  (abs a))

(deftest fixnum-arith-return-abs-of-most-negative
  (let ((r (fart-abs most-negative-fixnum)))
    (list (integerp r) (= r (- most-negative-fixnum))))
  (t t))
