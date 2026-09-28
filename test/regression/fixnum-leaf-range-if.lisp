;;; (if c a b) has an integer range when both arms do: the union of the two.
;;;
;;; Whether integer arithmetic may be computed in raw int64 is decided in three
;;; places, FIXNUM-TYPED-P, EXPR-INT-RANGE and COMPILE-AS-LONG. The first and
;;; the last already took a two-armed IF whose arms are fixnum-typed, and
;;; COMPILE-AS-LONG branches with a raw int64 on each path. EXPR-INT-RANGE did
;;; not, and every consumer that asks it for a proof -- 1+ / 1-, the binop fast
;;; path, the init of an Int64 slot -- compiled the IF boxed, including arms
;;; that were already raw reads (a declared structure slot, (length x), a
;;; constant-divisor MOD).
;;;
;;; The tests are written without THE on purpose: a (the fixnum ...) around the
;;; IF would have taken the typed path before this change too.

(setf dotcl:*save-sil* t)

(defun %flri-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %flri-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defstruct flri (a 0 :type fixnum) (b 0 :type fixnum))

;; An Int64 local assigned from an IF of two slot reads.
(defun %flri-set (c s)
  (let ((acc 0))
    (declare (fixnum acc))
    (setq acc (if c (flri-a s) (flri-b s)))
    acc))

;; 1- of an IF of two lengths: [0, 2^31-1] each, so the decrement is provable.
(defun %flri-len (c v w) (1- (if c (length v) (length w))))

;; A multiply whose right operand is an IF of a constant-divisor MOD and a
;; literal: [0, 9] u [7, 7].
(defun %flri-mul (c i) (declare (fixnum i)) (* 3 (if c (mod i 10) 7)))

;; Nested IFs.
(defun %flri-nested (c d v i)
  (declare (fixnum i))
  (1+ (if c (length v) (if d (mod i 4) -2))))

;; Controls. An arm with no range: the whole IF has none.
(defun %flri-no-range (c s x) (* 3 (if c (flri-a s) x)))
;; A one-armed IF answers NIL when the test fails.
(defun %flri-one-armed (c v) (let ((r (if c (length v)))) r))
;; A union that does not fit: the multiply can leave int64 and must promote.
(defun %flri-ovf (c s) (* (if c (flri-a s) 1) 100))

;; An arm that calls a function whose inferred FIXNUM return type is not usable
;; here (a REPL / source LOAD call to another function, see
;; inferred-return-type-scope.lisp): the IF gets no range from it, so the
;; redefinition below cannot make the 1+ wrap or fail.
(defun %flri-f (x) (declare (fixnum x)) x)
(defun %flri-f (x) (* x most-positive-fixnum))
(defun %flri-call-arm (c y) (declare (fixnum y)) (1+ (if c (%flri-f y) 0)))

;;; ---- values ----

(deftest fixnum-leaf-range-if.set
  (let ((s (make-flri :a 5 :b -6)))
    (list (%flri-set t s) (%flri-set nil s)))
  (5 -6))

(deftest fixnum-leaf-range-if.set-extremes
  (let ((s (make-flri :a most-positive-fixnum :b most-negative-fixnum)))
    (list (%flri-set t s) (%flri-set nil s)))
  (#.most-positive-fixnum #.most-negative-fixnum))

(deftest fixnum-leaf-range-if.len
  (list (%flri-len t "abc" "") (%flri-len nil "abc" "") (%flri-len nil '(1 2) nil))
  (2 -1 -1))

(deftest fixnum-leaf-range-if.mul
  (list (%flri-mul t 23) (%flri-mul t -23) (%flri-mul nil 23))
  (9 21 21))

(deftest fixnum-leaf-range-if.nested
  (list (%flri-nested t nil "ab" 0) (%flri-nested nil t "" 7) (%flri-nested nil nil "" 7))
  (3 4 -1))

(deftest fixnum-leaf-range-if.no-range-arm
  (list (%flri-no-range t (make-flri :a 2) 1.5) (%flri-no-range nil (make-flri :a 2) 1.5))
  (6 4.5))

(deftest fixnum-leaf-range-if.one-armed
  (list (%flri-one-armed t "abc") (%flri-one-armed nil "abc"))
  (3 nil))

(deftest fixnum-leaf-range-if.overflow-promotes
  (list (%flri-ovf t (make-flri :a most-positive-fixnum))
        (%flri-ovf nil (make-flri :a 0)))
  (#.(* most-positive-fixnum 100) 100))

(deftest fixnum-leaf-range-if.redefined-call-arm
  (list (%flri-call-arm t 3) (%flri-call-arm nil 3))
  (#.(1+ (* 3 most-positive-fixnum)) 1))

;;; ---- emitted code ----

;; Both slot reads are raw (StructRefL) and nothing is boxed or unwrapped on
;; the way into the Int64 local.
(deftest-emitting-only fixnum-leaf-range-if.set-is-raw
  (let ((r (%flri-sil #'%flri-set)))
    (list (%flri-count "Runtime.StructRefL" r)
          (%flri-count "Runtime.StructRefI" r)
          (%flri-count "Runtime.UnwrapMv" r)))
  (2 0 0))

(deftest-emitting-only fixnum-leaf-range-if.len-is-raw
  (let ((r (%flri-sil #'%flri-len)))
    (list (%flri-count "Runtime.Decrement" r) (%flri-count "(SUB)" r)))
  (0 1))

(deftest-emitting-only fixnum-leaf-range-if.mul-is-raw
  (let ((r (%flri-sil #'%flri-mul)))
    (list (%flri-count "Runtime.Multiply)" r) (%flri-count "Runtime.MultiplyFixnum" r)))
  (0 1))

(deftest-emitting-only fixnum-leaf-range-if.nested-is-raw
  (%flri-count "Runtime.Increment" (%flri-sil #'%flri-nested))
  0)

(deftest-emitting-only fixnum-leaf-range-if.controls-stay-generic
  (list (%flri-count "Runtime.Multiply)" (%flri-sil #'%flri-no-range))
        (%flri-count "Runtime.Multiply)" (%flri-sil #'%flri-ovf)))
  (1 1))
