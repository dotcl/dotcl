;;; Regression: the native-representation tables (*long-locals*,
;;; *native-double-locals*, *native-single-locals*) and the proven-range table
;;; (*small-int-locals*) record facts about a SLOT, and must not be confused by
;;; another variable that merely shares the name.
;;;
;;; They were keyed by name. Because the fact is about the slot, a same-named
;;; inner binding could not be handled by dropping the entry — dropping
;;; un-declared a slot that was still live and still native, and the next read of
;;; it emitted a boxed load against a raw Int64/Double slot. Symptom:
;;; NullReferenceException, or AccessViolationException when the mismatch reached
;;; castclass on a raw double.
;;;
;;; The mirror image came from *small-int-locals*: an inner (let ((f 3))) proved
;;; the range of ITS f, and an outer f holding a native double inherited it by
;;; name — native fixnum multiply against an r8 slot.

(defpackage :nss-a (:use))
(defpackage :nss-b (:use))

;;; Int64-slot local read from inside a scope binding the same name elsewhere.
(deftest native-slot-fixnum-outer
  (let ((nss-a::n 1))
    (declare (fixnum nss-a::n))
    (let ((nss-b::n 1000000000000))
      (+ nss-a::n 2)))
  3)

(deftest native-slot-fixnum-both
  (let ((nss-a::n 1))
    (declare (fixnum nss-a::n))
    (let ((nss-b::n 1000000000000))
      (list (+ nss-a::n 2) (* nss-b::n 2))))
  (3 2000000000000))

;;; Native r8 slot with an integer-valued same-named binding inside: the inner
;;; binding's proven range must not reach the double slot.
(deftest native-slot-double-outer
  (let ((nss-a::f 1.25d0))
    (declare (double-float nss-a::f))
    (let ((nss-b::f 3))
      (* nss-a::f 2)))
  2.5d0)

(deftest native-slot-double-both
  (let ((nss-a::f 1.25d0))
    (declare (double-float nss-a::f))
    (let ((nss-b::f 3))
      (list (* nss-a::f 2) (+ nss-b::f 4))))
  (2.5d0 7))

;;; Assignment into the outer native slot from the shadowing scope.
(deftest native-slot-setq-outer
  (let ((nss-a::m 1))
    (declare (fixnum nss-a::m))
    (let ((nss-b::m 5))
      (setq nss-a::m (+ nss-a::m 10))
      (list nss-a::m nss-b::m)))
  (11 5))

;;; Same-package shadowing keeps working: the inner binding owns its own slot.
(deftest native-slot-same-package-shadow
  (let ((n 1))
    (declare (fixnum n))
    (let ((n 2.5d0))
      (* n 2)))
  5.0d0)

;;; let* sequential bindings — the second binding shadows by name only.
(deftest native-slot-let*
  (let ((nss-a::k 2))
    (declare (fixnum nss-a::k))
    (let* ((nss-b::k 1.5d0)
           (r (* nss-b::k 2)))
      (list (+ nss-a::k 1) r)))
  (3 3.0d0))

;;; Native r4 slot — the single-float table is the third of the same shape.
(deftest native-slot-single-outer
  (let ((nss-a::s 1.5))
    (declare (single-float nss-a::s))
    (let ((nss-b::s 3))
      (list (+ nss-a::s 0.5) (+ nss-b::s 4))))
  (2.0 7))

;;; The outer slot is still native after the shadowing scope closes — the fix is
;;; "the inner binding never reached the entry", not "the entry was restored".
(deftest native-slot-restore-after-shadow
  (let ((n 3))
    (declare (fixnum n))
    (let ((n 2.5d0)) (declare (ignorable n)) nil)
    (+ n 1))
  4)

;;; A local function's parameter shadowing an enclosing native slot. The params
;;; of a labels group are plain shared LispObject slots, and that body no longer
;;; filters the native tables by name — it relies on the params' fresh keys.
(deftest native-slot-labels-param-shadow
  (let ((n 5))
    (declare (fixnum n))
    (labels ((twice (n) (* n 2))
             (thrice (n) (* n 3)))
      (+ (twice 3) (thrice 2) n)))
  17)

(deftest native-slot-flet-param-shadow-double
  (let ((d 1.5d0))
    (declare (double-float d))
    (flet ((half (d) (/ d 2)))
      (+ (half 3) d)))
  3.0d0)

;;; A long-rep loop counter: the increment and compare must stay native across a
;;; same-named binding in the body (this is the shape that actually shows up in
;;; numeric code, where the slot is written every iteration).
(defun %nss-count (limit)
  (declare (fixnum limit))
  (let ((total 0))
    (declare (fixnum total))
    (dotimes (nss-a::i limit)
      (declare (fixnum nss-a::i))
      (let ((nss-b::i :ignored))
        (declare (ignorable nss-b::i))
        (setq total (+ total nss-a::i))))
    total))

(deftest native-slot-loop-counter-shadow
  (%nss-count 100)
  4950)
