;;; Float literals in native float arithmetic are IL immediates.
;;;
;;; A DOUBLE-FLOAT (or SINGLE-FLOAT) local declared as such gets a native r8
;;; (r4) slot, and arithmetic on it lowers to bare IL mul/div/add. A literal
;;; operand had no clause of its own on that path, so it fell through to the
;;; generic compiler and came back as a constant-pool reference that then had
;;; to be unboxed: ldc.i4 idx; call GetConstant; castclass LispObject;
;;; castclass DoubleFloat; call get_Value. Nothing is allocated by that -- the
;;; pool shares one object per literal -- but the whole sequence is re-executed
;;; on every evaluation, so a declared float loop paid it once per literal per
;;; iteration while the same loop over fixnums already had its literal as a
;;; plain ldc.i8.
;;;
;;; What the tests pin: the constant-pool read is gone from the shape where the
;;; operand is a literal, it is still there where the value genuinely is an
;;; object, and the literals keep their exact values -- the immediate is
;;; written into the IL and read back out of the SIL text, so a printer or
;;; reader that lost a digit would show up here.

(setf dotcl:*save-sil* t)

(defun %fln-sil (fn) (princ-to-string (dotcl:function-sil fn)))

(defun %fln-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

;;; ---- the shapes ----

;; The parity kernel: two literals inside a loop over native double slots.
(defun %fln-double-loop (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((x 1.0d0)
        (acc 0.0d0))
    (declare (double-float x acc))
    (do ((i 0 (the fixnum (1+ i))))
        ((>= i n) acc)
      (declare (fixnum i))
      (setq x (the double-float (* x 1.0000001d0)))
      (setq acc (the double-float (+ acc (the double-float (/ 1.0d0 x))))))))

(defun %fln-single (a)
  (declare (single-float a) (optimize (speed 3) (safety 0) (debug 0)))
  (the single-float (- (* a 2.5f0) 0.5f0)))

;; No literal anywhere: the operands are slots, so this shape was already
;; native and must stay exactly as it was.
(defun %fln-double-vars (a b)
  (declare (double-float a b) (optimize (speed 3) (safety 0) (debug 0)))
  (the double-float (/ (+ a b) (- a b))))

;; A literal in a generic (non-float-typed) position still goes through the
;; constant pool -- the immediate only exists where the context wants a raw r8.
(defun %fln-generic (a)
  (list a 1.0000001d0))

;;; ---- SIL shape ----
;;;
;;; DEFTEST-EMITTING-ONLY: an emit-free build stores no SIL, so FUNCTION-SIL
;;; answers NIL and every count below would be 0 for the wrong reason.

;; Four literals in the kernel (two initialisers, two in the loop body), all
;; four immediates now, and no constant-pool read or unbox left. The single
;; DoubleFloat allocation is the boxed value the function returns.
(deftest-emitting-only float-literal-native-arith.double-loop-uses-immediates
  (let ((d (%fln-sil #'%fln-double-loop)))
    (list (%fln-count "(LDC-R8 " d)
          (%fln-count "(LOAD-CONST " d)
          (%fln-count "(UNBOX-DOUBLE)" d)
          (%fln-count "DoubleFloat" d)))
  (4 0 0 1))

;; The loop body proper: one MUL, one DIV, one ADD on raw r8, and nothing that
;; reaches for an object. Taken from the text after the end-test's box, which
;; is the only DoubleFloat construction in the function.
(deftest-emitting-only float-literal-native-arith.loop-body-is-raw
  (let* ((d (%fln-sil #'%fln-double-loop))
         (body (subseq d (search "DoubleFloat" d))))
    (list (%fln-count "(MUL)" body)
          (%fln-count "(DIV)" body)
          (%fln-count "(LDC-R8 " body)
          (%fln-count "Runtime.Multiply" body)
          (%fln-count "Runtime.Divide" body)
          (%fln-count "(LOAD-CONST " body)
          (%fln-count "(NEWOBJ " body)))
  (1 1 2 0 0 0 0))

;; Both single-float literals are immediates. The one UNBOX-SINGLE left is the
;; parameter: it arrives in a boxed LispObject slot, so reading it is still an
;; unbox, and only the literals were ever in question here.
(deftest-emitting-only float-literal-native-arith.single-uses-immediates
  (let ((d (%fln-sil #'%fln-single)))
    (list (%fln-count "(LDC-R4 " d)
          (%fln-count "(LOAD-CONST " d)
          (%fln-count "(UNBOX-SINGLE)" d)))
  (2 0 1))

(deftest-emitting-only float-literal-native-arith.no-literal-no-change
  (let ((d (%fln-sil #'%fln-double-vars)))
    (list (%fln-count "(LDC-R8 " d)
          (%fln-count "(LOAD-CONST " d)
          (%fln-count "(DIV)" d)
          (%fln-count "(ADD)" d)))
  (0 0 1 1))

(deftest-emitting-only float-literal-native-arith.generic-position-still-pooled
  (let ((d (%fln-sil #'%fln-generic)))
    (list (%fln-count "(LDC-R8 " d)
          (%fln-count "(LOAD-CONST " d)))
  (0 1))

;;; ---- values ----
;;;
;;; The immediate travels as text through the SIL file, so these also pin that
;;; the literal survives print and read without losing a digit.

;; Every iteration multiplies by the literal, so a literal that lost a digit
;; moves this value after the first few; 10000 iterations are plenty, and the
;; loop is interpreted on the emit-free build.
(deftest float-literal-native-arith.double-loop-value
  (%fln-double-loop 10000)
  9995.001166746946d0)

(deftest float-literal-native-arith.double-loop-small-agrees
  (list (%fln-double-loop 0) (%fln-double-loop 1) (%fln-double-loop 2))
  (0.0d0 0.9999999000000099d0 1.9999997000000398d0))

(deftest float-literal-native-arith.single-value
  (list (%fln-single 1.0f0) (%fln-single 0.0f0) (%fln-single -2.0f0))
  (2.0f0 -0.5f0 -5.5f0))

(deftest float-literal-native-arith.double-vars-value
  (list (%fln-double-vars 3.0d0 1.0d0) (%fln-double-vars 1.0d0 3.0d0))
  (2.0d0 -2.0d0))

;; The literal itself, round-tripped: an immediate that lost precision would
;; stop being EQL to the same literal read here.
(deftest float-literal-native-arith.literal-is-exact
  (let ((x 1.0d0))
    (declare (double-float x))
    (list (eql (the double-float (* x 1.0000001d0)) 1.0000001d0)
          (eql (the double-float (* x 0.1d0)) 0.1d0)
          (eql (the double-float (* x 1.7976931348623157d308))
               1.7976931348623157d308)))
  (t t t))

(setf dotcl:*save-sil* nil)
