;;; Native int64 arithmetic on operands that are already raw.
;;;
;;; A function whose parameters and result are all declared FIXNUM gets an
;;; Int64 slot per parameter. Reading such a slot in a generic position costs a
;;; Fixnum.Make, so (- N 1) used to box N back up only for Runtime.Decrement to
;;; take it apart again -- declaring the FTYPE made the function SLOWER than
;;; leaving it undeclared, where the parameter sits in a boxed slot and is
;;; passed to the generic operation as it stands.
;;;
;;; The raw path was refused because it is gated on a value-range proof, and no
;;; proof can show that FIXNUM minus one fits int64. That gate is about
;;; INTERMEDIATES: raw +/-/* wrap silently, so a nested expression must be
;;; proven before its parts may be computed raw. Operands that are already raw
;;; -- a literal, an Int64 slot, a slot with a proven range -- compute nothing,
;;; and the operation itself goes through Runtime.{Add,Subtract,Multiply}Fixnum,
;;; which promotes to a bignum on overflow. So that shape needs no proof.
;;;
;;; What the tests pin: the boxing disappears where the operands are raw leaves,
;;; the generic path is still taken for undeclared code and for non-leaf
;;; operands, overflow still promotes instead of wrapping, and the values are
;;; unchanged including well outside the small-integer cache.

(setf dotcl:*save-sil* t)

(defun %fpn-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %fpn-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; ---- the shapes ----

;; FTYPE declaimed: N is an Int64 slot.
(declaim (ftype (function (fixnum) fixnum) %fpn-fib-ftype))
(defun %fpn-fib-ftype (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (if (< n 2) n (+ (%fpn-fib-ftype (- n 1)) (%fpn-fib-ftype (- n 2)))))

;; Same body, body declaration only. The declaration alone also earns N an
;; Int64 slot (see fixnum-param-long-slot), so this shape reaches the same
;; native subtractions by the other road.
(defun %fpn-fib-decl (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (if (< n 2) n (+ (%fpn-fib-decl (- n 1)) (%fpn-fib-decl (- n 2)))))

;; No declaration at all: everything generic, as before.
(defun %fpn-fib-plain (n)
  (if (< n 2) n (+ (%fpn-fib-plain (- n 1)) (%fpn-fib-plain (- n 2)))))

(declaim (ftype (function (fixnum fixnum) fixnum) %fpn-sub2 %fpn-add2 %fpn-mul2))
(defun %fpn-sub2 (a b) (declare (fixnum a b)) (- a b))
(defun %fpn-add2 (a b) (declare (fixnum a b)) (+ a b))
(defun %fpn-mul2 (a b) (declare (fixnum a b)) (* a b))

;; An ARITHMETIC operand. The inner (* a b) is itself two raw leaves, so it goes
;; native; the outer + then has a computed operand, which is exactly the
;; unproven intermediate the range gate exists for, so the outer operation stays
;; on the generic promoting path.
(declaim (ftype (function (fixnum fixnum) fixnum) %fpn-nested))
(defun %fpn-nested (a b) (declare (fixnum a b)) (+ (* a b) 1))

;;; ---- SIL shape ----
;;;
;;; DEFTEST-EMITTING-ONLY, not DEFTEST: an emit-free build stores no SIL, so
;;; FUNCTION-SIL answers NIL there and every count taken from it is 0. That
;;; makes an assertion of "this instruction is gone" pass for the wrong reason
;;; and an assertion of "this slot is native" fail for the wrong reason. The
;;; values below are the part that is a statement about the language, and they
;;; keep running everywhere.

;; Both subtractions are native now, and the only box left is the one the
;; function has to pay to return N from the base case.
(deftest-emitting-only fixnum-param-native-arith.ftype-fib-is-native
  (let ((d (%fpn-sil #'%fpn-fib-ftype)))
    (list (%fpn-count "Fixnum.Make" d)
          (%fpn-count "Runtime.Decrement" d)
          (%fpn-count "Runtime.Subtract)" d)
          (%fpn-count "Runtime.SubtractFixnum" d)))
  (1 0 0 2))

;; A body declaration without the FTYPE reaches the same native subtractions:
;; the declaration is what makes the slot raw, and the FTYPE only adds the
;; native entry point on top of it.
(deftest-emitting-only fixnum-param-native-arith.body-declared-fib-is-native-too
  (let ((d (%fpn-sil #'%fpn-fib-decl)))
    (list (%fpn-count "Runtime.Decrement" d)
          (%fpn-count "Runtime.Subtract)" d)
          (%fpn-count "Runtime.SubtractFixnum" d)))
  (0 0 2))

(deftest-emitting-only fixnum-param-native-arith.plain-fib-unchanged
  (let ((d (%fpn-sil #'%fpn-fib-plain)))
    (list (%fpn-count "Runtime.Decrement" d)
          (%fpn-count "Runtime.SubtractFixnum" d)))
  (1 0))

(deftest-emitting-only fixnum-param-native-arith.two-int64-slots-are-native
  (list (%fpn-count "Runtime.SubtractFixnum" (%fpn-sil #'%fpn-sub2))
        (%fpn-count "Runtime.AddFixnum" (%fpn-sil #'%fpn-add2))
        (%fpn-count "Runtime.MultiplyFixnum" (%fpn-sil #'%fpn-mul2)))
  (1 1 1))

;; A raw operand is a LEAF or nothing. The inner multiply qualifies; the outer
;; add does not and keeps its generic operation.
(deftest-emitting-only fixnum-param-native-arith.nested-operand-stays-generic
  (let ((d (%fpn-sil #'%fpn-nested)))
    (list (%fpn-count "Runtime.MultiplyFixnum" d)
          (%fpn-count "Runtime.Increment" d)
          (%fpn-count "Runtime.AddFixnum" d)
          (%fpn-count "Runtime.Add)" d)))
  (1 1 0 0))

;;; ---- values ----

(deftest fixnum-param-native-arith.fib-values-agree
  (list (%fpn-fib-ftype 20) (%fpn-fib-decl 20) (%fpn-fib-plain 20))
  (6765 6765 6765))

;; Well outside the small-integer cache, on both the operand and the result.
(deftest fixnum-param-native-arith.large-values
  (list (%fpn-sub2 1000000000000 1) (%fpn-add2 1000000000000 999)
        (%fpn-mul2 1000000007 1000000009))
  (999999999999 1000000000999 1000000016000000063))

(deftest fixnum-param-native-arith.negative-values
  (list (%fpn-sub2 -1000000000000 1) (%fpn-add2 -1000000000000 -999)
        (%fpn-mul2 -1000000007 1000000009))
  (-1000000000001 -1000000000999 -1000000016000000063))

;; The promoting helper is the whole reason the proof can be skipped: an int64
;; overflow must become a bignum, never a wrapped negative.
(deftest fixnum-param-native-arith.overflow-promotes-not-wraps
  (list (%fpn-add2 9223372036854775807 1)
        (%fpn-sub2 -9223372036854775808 1)
        (%fpn-mul2 4611686018427387904 4))
  (9223372036854775808 -9223372036854775809 18446744073709551616))

(deftest fixnum-param-native-arith.nested-value
  (list (%fpn-nested 3 4) (%fpn-nested 1000000007 1000000009))
  (13 1000000016000000064))

(setf dotcl:*save-sil* nil)
