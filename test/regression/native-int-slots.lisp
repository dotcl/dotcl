;;; Regression tests for RANGE-PROVEN NATIVE INT64 LOCAL SLOTS: a plain lexical
;;; LET binding with no fixnum declaration, whose init has a statically proven
;;; int64 range, gets a raw Int64 slot instead of a boxed Fixnum. This is the
;;; integer analog of the native float/decimal slots, and the motivating case is
;;; crc-division-step's (let ((new-rmdr (logior bit (* rmdr 2)))) ...), where the
;;; init is already computed with native int64 ops and boxed only to be unboxed
;;; again by the body.
;;;
;;; Contract, in order of importance:
;;;   1. Results are identical to the boxed path — in particular the TIGHT range
;;;      proved for the binding survives the promotion, so a product that leaves
;;;      int64 still promotes to a bignum instead of wrapping.
;;;   2. The promotion is refused wherever an Int64 slot could not hold the value
;;;      (mutated, captured, special) or would not pay (atom init, no native use).

(setf dotcl:*save-sil* t)

(defun %nis-int64-slot-p (fn)
  "True when FN's SIL declares at least one Int64 local."
  (and (search "Int64" (princ-to-string (dotcl:function-sil fn))) t))

;;; ---- 1. the crc shape: promoted, and box-free at the store ----

(defun %nis-crc-step (bit rmdr poly msb-mask)
  (declare (type (signed-byte 56) rmdr poly msb-mask) (type bit bit))
  (let ((new-rmdr (logior bit (* rmdr 2))))
    (if (zerop (logand msb-mask new-rmdr))
        new-rmdr
        (logxor new-rmdr poly))))

(defun %nis-crc-step-generic (bit rmdr poly msb-mask)
  (let ((new-rmdr (logior bit (* rmdr 2))))
    (if (zerop (logand msb-mask new-rmdr))
        new-rmdr
        (logxor new-rmdr poly))))

;;; DEFTEST-COMPILED-ONLY below: a native (unboxed Int64) slot representation is
;;; chosen by the compiler from the declared slot type. An emit-free build stores
;;; the ordinary boxed value, so these ask a question that build cannot answer.

(deftest-compiled-only nis-crc-step-native-slot
  (%nis-int64-slot-p #'%nis-crc-step)
  t)

;; The store into the slot no longer boxes: the only Fixnum.Make left is the one
;; that hands the result back to the caller.
(deftest-compiled-only nis-crc-step-one-box
  (let ((sil (princ-to-string (dotcl:function-sil #'%nis-crc-step)))
        (n 0)
        (start 0))
    (loop for p = (search "Fixnum.Make" sil :start2 start)
          while p do (incf n) (setf start (+ p 11)))
    n)
  2)

(deftest nis-crc-step-matches-generic
  (equal (%nis-crc-step 1 123456789 1099587256329 (ash 1 40))
         (%nis-crc-step-generic 1 123456789 1099587256329 (ash 1 40)))
  t)

;; Both branches out of the native slot: the untouched value and the logxor'd one.
(deftest nis-crc-step-then-branch
  (%nis-crc-step 0 5 7 (ash 1 40))
  10)

(deftest nis-crc-step-else-branch
  (equal (%nis-crc-step 1 (ash 1 39) 7 (ash 1 40))
         (%nis-crc-step-generic 1 (ash 1 39) 7 (ash 1 40)))
  t)

;;; ---- 2. the tight range survives promotion (no silent wrap) ----

;; V's proven range is that of (* a 2), NOT the full int64 range the raw slot
;; could physically hold. (* v v) therefore fails the range proof and takes the
;; promoting path, yielding a bignum. If the promotion widened the range to full
;; int64, this would wrap and disagree with the generic twin.
(defun %nis-tight (a)
  (declare (type (signed-byte 56) a))
  (let ((v (* a 2)))
    (if (zerop (logand v 1)) (* v v) (- (* v v)))))

(defun %nis-tight-generic (a)
  (let ((v (* a 2)))
    (if (zerop (logand v 1)) (* v v) (- (* v v)))))

(deftest-compiled-only nis-tight-native-slot
  (%nis-int64-slot-p #'%nis-tight)
  t)

(deftest nis-tight-no-wrap
  (equal (%nis-tight 36028797018963967) (%nis-tight-generic 36028797018963967))
  t)

(deftest nis-tight-exact
  (%nis-tight 36028797018963967)
  5192296858534827340300120177508356)

;; Same one level down: a product of two promoted slots must still promote.
(defun %nis-sum-promotes (a b)
  (declare (type (signed-byte 62) a b))
  (let ((x (* a 2))
        (y (* b 2)))
    (if (zerop (logand x 1)) (* x y) (+ x y))))

(defun %nis-sum-promotes-generic (a b)
  (let ((x (* a 2))
        (y (* b 2)))
    (if (zerop (logand x 1)) (* x y) (+ x y))))

(deftest nis-sum-promotes-matches
  (equal (%nis-sum-promotes 2305843009213693951 2305843009213693951)
         (%nis-sum-promotes-generic 2305843009213693951 2305843009213693951))
  t)

;;; ---- 3. promotion is refused where the slot could not hold the value ----

;; Mutated: a SETQ could store a value outside the proven range (here a bignum),
;; which an Int64 slot cannot hold. Must stay boxed — and stay correct.
(defun %nis-mutated (a)
  (declare (type (signed-byte 56) a))
  (let ((v (* a 2)))
    (setq v (* v v v v))
    (if (zerop (logand a 1)) v (- v))))

(defun %nis-mutated-generic (a)
  (let ((v (* a 2)))
    (setq v (* v v v v))
    (if (zerop (logand a 1)) v (- v))))

(deftest nis-mutated-not-promoted
  (%nis-int64-slot-p #'%nis-mutated)
  nil)

(deftest nis-mutated-matches-generic
  (equal (%nis-mutated 36028797018963967) (%nis-mutated-generic 36028797018963967))
  t)

;; Captured by a closure: the env capture loads the slot as an object reference.
(defun %nis-captured (a)
  (declare (type (signed-byte 56) a))
  (let ((v (* a 2)))
    (list (funcall (lambda () (logand v 255))) v)))

(deftest nis-captured-not-promoted
  (%nis-int64-slot-p #'%nis-captured)
  nil)

(deftest nis-captured-value
  (%nis-captured 300)
  (88 600))

;; Special: the binding goes through DynamicBindings, not a slot.
(defvar *nis-special* 0)

(defun %nis-special (a)
  (declare (type (signed-byte 56) a))
  (let ((*nis-special* (* a 2)))
    (logand *nis-special* 255)))

(deftest nis-special-not-promoted
  (%nis-int64-slot-p #'%nis-special)
  nil)

(deftest nis-special-value
  (%nis-special 300)
  88)

;;; ---- 4. promotion is refused where it would not pay ----

;; Atom init: storing a literal boxed costs nothing, so promoting would only add
;; a Fixnum.Make to every read.
(defun %nis-atom-init (a)
  (declare (type (signed-byte 56) a))
  (let ((v 7))
    (logand a v)))

(deftest nis-atom-init-not-promoted
  (%nis-int64-slot-p #'%nis-atom-init)
  nil)

(deftest nis-atom-init-value
  (%nis-atom-init 300)
  4)

;; No native integer use in the body: every read would have to re-box.
(defun %nis-no-native-use (a)
  (declare (type (signed-byte 56) a))
  (let ((v (* a 2)))
    (list v v)))

(deftest nis-no-native-use-not-promoted
  (%nis-int64-slot-p #'%nis-no-native-use)
  nil)

(deftest nis-no-native-use-value
  (%nis-no-native-use 21)
  (42 42))

;;; ---- 5. LET* is left alone (its inits do not see this scope) ----

(defun %nis-big-value ()
  (* 1099511627776 1099511627776))

;; A LET*'s later inits are evaluated with the earlier siblings bound, but a
;; range proof run at the head of the form sees the ENCLOSING scope. Promoting
;; there would hand the outer V's range to the inner V, and W's init would be
;; lowered to a raw int64 store that the runtime bignum cannot satisfy. The outer
;; LET is still promoted (it has a native use), so this pins the LET* refusal by
;; value, not by slot count.
(defun %nis-let*-shadow (a)
  (declare (type (signed-byte 56) a))
  (let ((v (* a 2)))
    (let ((k (logand v 3)))
      (let* ((v (%nis-big-value))
             (w (* v 2)))
        (list k w)))))

(deftest nis-let*-shadow-value
  (%nis-let*-shadow 3)
  (2 2417851639229258349412352))

;;; ---- 6. the promoted value read generically is still the right object ----

(defun %nis-generic-read (a)
  (declare (type (signed-byte 56) a))
  (let ((v (logior 1 (* a 2))))
    (if (zerop (logand v 1)) nil (format nil "~a" v))))

(deftest-compiled-only nis-generic-read-native-slot
  (%nis-int64-slot-p #'%nis-generic-read)
  t)

(deftest nis-generic-read-value
  (%nis-generic-read 21)
  "43")

;; Negative values round-trip through the raw slot unchanged.
(defun %nis-negative (a)
  (declare (type (signed-byte 56) a))
  (let ((v (* a 3)))
    (if (zerop (logand v 1)) v (1+ v))))

(deftest nis-negative-value
  (%nis-negative -7)
  -20)

(setf dotcl:*save-sil* nil)
