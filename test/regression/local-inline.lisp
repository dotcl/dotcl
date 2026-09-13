;;; (declare (inline f)) for a local FLET/LABELS function.
;;;
;;; The point is not saving a call. A local function is compiled to its own
;;; method, so a RETURN-FROM out of one crosses a frame boundary, and dotcl
;;; spends a .NET exception on that -- hundreds of bytes per escape. Substituted
;;; into the caller, the same RETURN-FROM is a branch within one method.
;;;
;;; These tests are about the ANSWER, which must not depend on whether the
;;; substitution happened: every refusal below leaves an ordinary call, and the
;;; result has to be identical either way.

;;; --- what the substitution is for -----------------------------------------

;;; Escaping past the local function to an outer block.
(deftest local-inline-escape
  (block scan
    (labels ((adv (i) (when (> i 3) (return-from scan :escaped)) i))
      (declare (inline adv))
      (adv 99)))
  :escaped)

;;; The branch that does not escape still returns normally.
(deftest local-inline-no-escape
  (block scan
    (labels ((adv (i) (when (> i 300) (return-from scan :escaped)) i))
      (declare (inline adv))
      (adv 5)))
  5)

;;; RETURN-FROM naming the function itself means its own implicit block, which
;;; the expansion re-establishes.
(deftest local-inline-own-block
  (labels ((adv (i) (return-from adv :own) (+ i 1000)))
    (declare (inline adv))
    (adv 1))
  :own)

;;; --- the expansion must not change what names mean ------------------------

;;; A parameter shadows an outer variable of the same name.
(deftest local-inline-parameter-shadows
  (let ((x 1))
    (labels ((f (x) x))
      (declare (inline f))
      (list (f 9) x)))
  (9 1))

;;; The body's free variable is the caller's, and reads its current value.
(deftest local-inline-free-variable
  (let ((n 1))
    (labels ((f () n))
      (declare (inline f))
      (let ((a (f)))
        (setq n 2)
        (list a (f)))))
  (1 2))

;;; Arguments are evaluated once, left to right, in the caller's scope.
(deftest local-inline-argument-order
  (let ((log '()))
    (labels ((f (a b) (+ a b)))
      (declare (inline f))
      (list (f (progn (push 1 log) 10) (progn (push 2 log) 20))
            (reverse log))))
  (30 (1 2)))

;;; A declaration written in the definition survives the substitution.
(deftest local-inline-keeps-declaration
  (labels ((f (i) (declare (fixnum i)) (* i 2)))
    (declare (inline f))
    (f 3))
  6)

;;; Multiple values pass through.
(deftest local-inline-multiple-values
  (labels ((f () (values 1 2)))
    (declare (inline f))
    (multiple-value-list (f)))
  (1 2))

;;; FLET, not only LABELS.
(deftest local-inline-flet
  (flet ((f (x) (* x 2)))
    (declare (inline f))
    (f 4))
  8)

;;; --- the refusals, which must still produce the right answer --------------

;;; A recursive local function has no finite expansion, so it is declined.
(deftest local-inline-recursive-declined
  (labels ((fact (n) (if (<= n 1) 1 (* n (fact (1- n))))))
    (declare (inline fact))
    (fact 5))
  120)

;;; Used as a value as well as called: both reach the same function.
(deftest local-inline-used-as-value
  (labels ((f (x) (1+ x)))
    (declare (inline f))
    (list (f 1) (funcall #'f 1)))
  (2 2))

;;; NOTINLINE in an inner scope overrides the request (CLHS 3.2.2.1.1).
(deftest local-inline-notinline-wins
  (labels ((f (x) (1+ x)))
    (declare (inline f))
    (locally (declare (notinline f)) (f 3)))
  4)

;;; Called from inside a nested local function, the body would land in a
;;; different method and the escape would leave the wrong frame. Declined, so
;;; the escape still reaches the outer block.
(deftest local-inline-under-nested-function
  (block outer
    (labels ((a () (return-from outer :out)))
      (declare (inline a))
      (labels ((b () (a)))
        (b)
        :not-this)))
  :out)

;;; The same, with the nested function being a LAMBDA.
(deftest local-inline-under-lambda
  (block outer
    (labels ((a () (return-from outer :out)))
      (declare (inline a))
      (funcall (lambda () (a)))
      :not-this))
  :out)

;;; An undeclared local function is untouched.
(deftest local-inline-not-declared
  (block scan
    (labels ((adv (i) (when (> i 3) (return-from scan :escaped)) i))
      (adv 99)))
  :escaped)
