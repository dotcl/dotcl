;;; (MOD X N) and (REM X N) with a constant divisor carry the divisor's range
;;; into EXPR-INT-RANGE.
;;;
;;; The fourth of the same omission, and the only one where the lowering was
;;; ALREADY reachable. Runtime.ModFixnumL is emitted today for a mod whose
;;; operands are fixnum-typed. What was unreachable is the arithmetic AROUND
;;; it: (* 3 (mod i 10)) lowered the mod natively and then called the generic
;;; promoting multiply, because EXPR-INT-RANGE had nothing to say about the
;;; mod's result and so could not prove the product fits int64.
;;;
;;; That is why this leaf has to answer the TIGHT range. A conservative full
;;; int64 range would buy nothing at all -- [int64min, int64max] times [3,3]
;;; does not fit, so the enclosing proof fails exactly as it did before. It is
;;; also why this is the first of the four whose instruction count goes DOWN:
;;; the others replaced a call with inline work, this one replaces a generic
;;; call with a specialised one.
;;;
;;; The bounds are CLHS 12.1.3.1's sign rules, not a guess about magnitude:
;;; MOD takes the sign of the DIVISOR, REM the sign of the NUMBER. The value
;;; tests below walk the whole sign matrix, because a range with the sign
;;; backwards would license a proof about values that cannot occur.

(setf dotcl:*save-sil* t)

(defun %flrm-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %flrm-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; ---- the shapes ----

(defun %flrm-mod (i)
  (declare (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (* 3 (mod i 10)))

(defun %flrm-rem (i)
  (declare (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (* 3 (rem i 10)))

;; A negative divisor: MOD's range is [N+1, 0], REM's is unchanged in shape.
(defun %flrm-mod-neg (i)
  (declare (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (* 2 (mod i -10)))

(defun %flrm-rem-neg (i)
  (declare (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (* 2 (rem i -10)))

;; A variable divisor gives no constant to bound, so nothing is claimed.
(defun %flrm-var-divisor (i d)
  (declare (fixnum i d) (optimize (speed 3) (safety 0) (debug 0)))
  (* 3 (mod i d)))

;; The product of the TRUE range overflows int64, so no proof is available and
;; the generic path must answer with an exact bignum. A range narrower than the
;; truth would prove here and wrap, so this is where a wrong bound shows.
(defun %flrm-overflows (i)
  (declare (fixnum i) (optimize (speed 3) (safety 0) (debug 0)))
  (* (mod i 1000000) 100000000000000))

;; A float dividend has no integer range at all -- (mod 5.5 2) is 1.5 -- and
;; the clause requires a fixnum-typed dividend for exactly that reason.
(defun %flrm-float (x)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (mod x 2))

;;; ---- values: the whole sign matrix ----

(deftest fixnum-leaf-range-mod-rem.mod-signs
  (list (mod 7 3) (mod -7 3) (mod 7 -3) (mod -7 -3))
  (1 2 -2 -1))

(deftest fixnum-leaf-range-mod-rem.rem-signs
  (list (rem 7 3) (rem -7 3) (rem 7 -3) (rem -7 -3))
  (1 -1 1 -1))

(deftest fixnum-leaf-range-mod-rem.mod-through-arithmetic
  (list (%flrm-mod 7) (%flrm-mod -7) (%flrm-mod 0))
  (21 9 0))

(deftest fixnum-leaf-range-mod-rem.rem-through-arithmetic
  (list (%flrm-rem 7) (%flrm-rem -7) (%flrm-rem 0))
  (21 -21 0))

;; The negative-divisor cases are where a sign-swapped range would be wrong.
(deftest fixnum-leaf-range-mod-rem.mod-negative-divisor
  (list (%flrm-mod-neg 7) (%flrm-mod-neg -7) (%flrm-mod-neg 0))
  (-6 -14 0))

(deftest fixnum-leaf-range-mod-rem.rem-negative-divisor
  (list (%flrm-rem-neg 7) (%flrm-rem-neg -7))
  (14 -14))

(deftest fixnum-leaf-range-mod-rem.variable-divisor
  (list (%flrm-var-divisor 7 3) (%flrm-var-divisor -7 3))
  (3 6))

;; Exact, not wrapped. 999999 * 100000000000000 exceeds int64.
(deftest fixnum-leaf-range-mod-rem.no-proof-means-no-wrap
  (list (%flrm-overflows 999999) (%flrm-overflows 1000000))
  (99999900000000000000 0))

(deftest fixnum-leaf-range-mod-rem.float-dividend
  (list (%flrm-float 5.5d0) (%flrm-float 4.0d0))
  (1.5d0 0.0d0))

;;; ---- emitted code ----
;;;
;;; Needles end at the closing paren: Runtime.Multiply is a prefix of
;;; Runtime.MultiplyFixnum, and PRINC-TO-STRING prints neither the operand's
;;; quotes nor a keyword's colon. Each test names what should be gone and what
;;; replaced it.

(deftest-emitting-only fixnum-leaf-range-mod-rem.enclosing-multiply-specialises
  (let ((m (%flrm-sil #'%flrm-mod))
        (r (%flrm-sil #'%flrm-rem)))
    (list (%flrm-count "Runtime.MultiplyFixnum)" m)
          (%flrm-count "Runtime.Multiply)" m)
          (%flrm-count "Runtime.MultiplyFixnum)" r)
          (%flrm-count "Runtime.Multiply)" r)))
  (1 0 1 0))

;; The mod itself was already lowered natively before this change, and still
;; is. That is what makes this leaf different from the other three: the clause
;; buys the context, not the operation.
(deftest-emitting-only fixnum-leaf-range-mod-rem.mod-itself-was-already-native
  (let ((m (%flrm-sil #'%flrm-mod))
        (r (%flrm-sil #'%flrm-rem)))
    (list (%flrm-count "Runtime.ModFixnumL)" m)
          (%flrm-count "Runtime.RemFixnumL)" r)))
  (1 1))

(deftest-emitting-only fixnum-leaf-range-mod-rem.negative-divisor-specialises
  (let ((m (%flrm-sil #'%flrm-mod-neg)))
    (list (%flrm-count "Runtime.MultiplyFixnum)" m)
          (%flrm-count "Runtime.Multiply)" m)))
  (1 0))

;; A variable divisor bounds nothing, so the enclosing multiply stays generic.
;; This is the control: it shares every needle with the tests above and must
;; come out the other way round.
(deftest-emitting-only fixnum-leaf-range-mod-rem.variable-divisor-stays-generic
  (let ((v (%flrm-sil #'%flrm-var-divisor)))
    (list (%flrm-count "Runtime.Multiply)" v)
          (%flrm-count "Runtime.MultiplyFixnum)" v)))
  (1 0))

(setf dotcl:*save-sil* nil)
