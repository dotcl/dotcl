;;; (THE FIXNUM <arith>) licenses the native int64 path.
;;;
;;; The range prover cannot show that FIXNUM + FIXNUM fits int64 -- the sum
;;; spans one bit more -- so an addition of two values known only to be fixnums
;;; fell to the generic promoting add, boxing both operands and the result.
;;; That is right for a bare (+ A B), whose value may legitimately be a bignum.
;;;
;;; THE is not a declaration about a variable: it is the writer's assertion
;;; about this result. So it licenses the native path, and the two safety
;;; levels differ in what happens when the assertion is wrong:
;;;
;;;   SAFETY 0      believe it; a violated assertion wraps (undefined per the
;;;                 standard, and what SBCL does)
;;;   SAFETY 1 and up  compute the same op with an overflow check and signal a
;;;                 TYPE-ERROR, so the assertion is never silently wrong
;;;
;;; A bare (+ A B) with no THE keeps promoting to a bignum at every safety.

(defun the-lic-add-safe (a b)
  (declare (fixnum a b) (optimize (speed 3) (safety 1) (debug 0)))
  (the fixnum (+ a b)))

(defun the-lic-add-unsafe (a b)
  (declare (fixnum a b) (optimize (speed 3) (safety 0) (debug 0)))
  (the fixnum (+ a b)))

(defun the-lic-add-plain (a b)
  (declare (fixnum a b) (optimize (speed 3) (safety 1) (debug 0)))
  (+ a b))

(deftest the-fixnum-license-ordinary-values
  (list (the-lic-add-safe 3 4) (the-lic-add-unsafe 3 4) (the-lic-add-plain 3 4))
  (7 7 7))

(deftest the-fixnum-license-negative-and-zero
  (list (the-lic-add-safe -5 5) (the-lic-add-safe -5 -6) (the-lic-add-unsafe -5 -6))
  (0 -11 -11))

;;; The assertion holds right up to the edge, so no check may fire there.

(deftest the-fixnum-license-at-the-edge
  (list (the-lic-add-safe most-positive-fixnum 0)
        (the-lic-add-safe most-negative-fixnum 0)
        (the-lic-add-safe (1- most-positive-fixnum) 1))
  (#.most-positive-fixnum #.most-negative-fixnum #.most-positive-fixnum))

;;; Violated at SAFETY 1: a TYPE-ERROR, not a wrapped value and not a bignum.

(deftest the-fixnum-license-overflow-signals-at-safety-1
  (handler-case (the-lic-add-safe most-positive-fixnum 1)
    (type-error () :type-error)
    (error () :other-error))
  :type-error)

;;; Without the assertion the same sum still promotes to a bignum.

(deftest plain-fixnum-add-still-promotes-to-bignum
  (list (typep (the-lic-add-plain most-positive-fixnum 1) 'integer)
        (> (the-lic-add-plain most-positive-fixnum 1) most-positive-fixnum))
  (t t))

;;; The license must not change what a provable expression compiles to, and
;;; must not change its value either.

(defun the-lic-proven (a)
  (declare (type (integer 0 100) a) (optimize (speed 3) (safety 1) (debug 0)))
  (the fixnum (+ a 1)))

(deftest the-fixnum-license-leaves-provable-arithmetic-alone
  (list (the-lic-proven 0) (the-lic-proven 100))
  (1 101))

;;; Subtraction and multiplication take the same path.

(defun the-lic-sub (a b)
  (declare (fixnum a b) (optimize (speed 3) (safety 1) (debug 0)))
  (the fixnum (- a b)))

(defun the-lic-mul (a b)
  (declare (fixnum a b) (optimize (speed 3) (safety 1) (debug 0)))
  (the fixnum (* a b)))

(deftest the-fixnum-license-sub-and-mul
  (list (the-lic-sub 10 3) (the-lic-mul 6 7)
        (handler-case (the-lic-sub most-negative-fixnum 1)
          (type-error () :type-error))
        (handler-case (the-lic-mul most-positive-fixnum 2)
          (type-error () :type-error)))
  (7 42 :type-error :type-error))

;;; The shape the license exists for: self-recursion whose result is asserted
;;; to be a fixnum. Its value must be unchanged.

(defun the-lic-fib (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (if (< n 2)
      n
      (the fixnum (+ (the-lic-fib (the fixnum (- n 1)))
                     (the-lic-fib (the fixnum (- n 2)))))))

(deftest the-fixnum-license-self-recursion-value
  (list (the-lic-fib 0) (the-lic-fib 1) (the-lic-fib 10) (the-lic-fib 25))
  (0 1 55 75025))

;;; A non-fixnum THE type must not reach the fixnum license at all.

(defun the-lic-not-fixnum (a b)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (the number (+ a b)))

(deftest the-non-fixnum-type-still-promotes
  (let ((r (the-lic-not-fixnum most-positive-fixnum most-positive-fixnum)))
    (= r (* 2 most-positive-fixnum)))
  t)
