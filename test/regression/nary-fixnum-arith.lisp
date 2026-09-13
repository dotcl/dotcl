;;; N-ary +, -, * on fixnum-declared arguments past the arity cutoff.
;;;
;;; Nine or more arguments used to fall to Runtime.AddN and friends, which take
;;; a LispObject[]. Building that array allocates, while the chain of binary
;;; calls it replaced does not once the arguments are fixnum-typed -- so a
;;; (declare (fixnum x)) made the code allocate where leaving the declaration
;;; off allocated nothing. The declaration is now allowed to keep folding.
;;;
;;; What matters here is that folding did not change any ANSWER. Each binary
;;; step decides for itself whether the raw int64 path is provably safe and
;;; takes the promoting call when it is not, so an intermediate that leaves
;;; fixnum range still has to produce a bignum rather than wrap.

(defun nfa-sum9 (x) (declare (fixnum x)) (+ x x x x x x x x x))
(defun nfa-sum10 (x) (declare (fixnum x)) (+ x x x x x x x x x x))
(defun nfa-sum9-undeclared (x) (+ x x x x x x x x x))
(defun nfa-diff9 (x) (declare (fixnum x)) (- x x x x x x x x x))
(defun nfa-prod9 (x) (declare (fixnum x)) (* x x x x x x x x x))
(defun nfa-quot9 (x) (declare (fixnum x)) (/ x x x x x x x x x))

;;; --- ordinary values -------------------------------------------------------

(deftest nary-fixnum-sum9
  (nfa-sum9 3)
  27)

(deftest nary-fixnum-sum10
  (nfa-sum10 3)
  30)

(deftest nary-fixnum-diff9
  (nfa-diff9 3)
  -21)

(deftest nary-fixnum-prod9
  (nfa-prod9 2)
  512)

;;; Division of fixnums is not fixnum arithmetic: the exact rational is the
;;; answer, and the fold has to preserve that.
(deftest nary-fixnum-quot9
  (nfa-quot9 2)
  1/128)

;;; --- leaving fixnum range --------------------------------------------------

;;; The sum of nine most-positive-fixnums does not fit int64. It has to promote,
;;; not wrap, and has to agree with the undeclared form.
(deftest nary-fixnum-sum-overflows-to-bignum
  (= (nfa-sum9 most-positive-fixnum) (* 9 most-positive-fixnum))
  t)

(deftest nary-fixnum-sum-overflow-matches-undeclared
  (= (nfa-sum9 most-positive-fixnum) (nfa-sum9-undeclared most-positive-fixnum))
  t)

(deftest nary-fixnum-sum-overflow-is-larger-than-fixnum
  (> (nfa-sum9 most-positive-fixnum) most-positive-fixnum)
  t)

(deftest nary-fixnum-negative-overflow
  (= (nfa-diff9 most-negative-fixnum) (- most-negative-fixnum (* 8 most-negative-fixnum)))
  t)

(deftest nary-fixnum-product-overflows-to-bignum
  (= (nfa-prod9 most-positive-fixnum) (expt most-positive-fixnum 9))
  t)

;;; --- an argument that is not a fixnum -------------------------------------

;;; One float argument puts the whole thing on the generic path; the answer is
;;; still the ordinary contagion result.
(deftest nary-fixnum-mixed-float
  (labels ((f (x) (declare (fixnum x)) (+ x x x x x x x x x 0.5d0)))
    (f 1))
  9.5d0)

;;; Zero is the identity, and the fold must not lose it.
(deftest nary-fixnum-zero
  (nfa-sum9 0)
  0)
