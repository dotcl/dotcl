;;; SQRT of a declared DOUBLE-FLOAT.
;;;
;;; The square root of a negative double is a COMPLEX, so (SQRT X) does not have
;;; a known type even when X does. Only a program that ALSO declares the result
;;; -- (THE DOUBLE-FLOAT (SQRT X)) -- has said enough for the call to be lowered
;;; to a machine square root; without the declaration the generic entry, which
;;; can return the complex, is the only correct one.
;;;
;;; That is a semantic difference, not just a speed one, and it is the point of
;;; these tests: what the lowered form answers has to be what the generic form
;;; answers, on every input where the declaration holds. Where it does not hold
;;; -- the negative -- the declaration is false, and from (safety 1) up a false
;;; declaration is a TYPE-ERROR, the same treatment declared FIXNUM arithmetic
;;; gets when it overflows. At (safety 0) a declaration is a license and the
;;; answer on a negative is undefined.

;;; --- declared, and the same value the generic call gives ---

(defun sdd-decl-0 (x)
  (declare (double-float x) (optimize (speed 3) (safety 0) (debug 0)))
  (the double-float (sqrt x)))

(defun sdd-decl-1 (x)
  (declare (double-float x) (optimize (speed 3) (safety 1) (debug 0)))
  (the double-float (sqrt x)))

(defun sdd-plain (x) (sqrt x))

(deftest sqrt-declared-double-exact-roots
  (list (sdd-decl-0 4.0d0) (sdd-decl-1 4.0d0) (sdd-plain 4.0d0))
  (2.0d0 2.0d0 2.0d0))

(deftest sqrt-declared-double-agrees-with-generic
  (list (= (sdd-decl-0 2.0d0) (sqrt 2.0d0))
        (= (sdd-decl-1 2.0d0) (sqrt 2.0d0))
        (= (sdd-decl-0 1.0d-300) (sqrt 1.0d-300)))
  (t t t))

(deftest sqrt-declared-double-is-a-double
  (let ((r (sdd-decl-1 9.0d0)))
    (list (typep r 'double-float) (= r 3.0d0)))
  (t t))

;;; Zero keeps its sign, as it does for the generic call.

(deftest sqrt-declared-double-zero
  (list (sdd-decl-0 0.0d0) (sdd-decl-1 0.0d0) (sdd-plain 0.0d0))
  (0.0d0 0.0d0 0.0d0))

;;; --- the declaration that does not hold ---
;;;
;;; A negative argument makes the result a COMPLEX, so the DOUBLE-FLOAT
;;; declaration is false. At (safety 1) that is reported rather than quietly
;;; becoming a NaN that travels through the rest of the computation. The
;;; behaviour at (safety 0) is undefined and is deliberately not pinned here.

(deftest sqrt-declared-double-negative-is-a-type-error
  (handler-case (progn (sdd-decl-1 -4.0d0) :no-error)
    (type-error () :type-error)
    (error () :other-error))
  :type-error)

;;; Undeclared, the complex is the answer and nothing signals.

(deftest sqrt-undeclared-negative-is-a-complex
  (let ((r (sdd-plain -4.0d0)))
    (list (complexp r) (= r (complex 0.0d0 2.0d0))))
  (t t))

;;; --- single-float arguments ---
;;;
;;; SQRT of a single is a single: the lowering must decline, not widen behind
;;; the program's back. Widening is something the program asks for, and when it
;;; does the declared form applies again.

(defun sdd-single (s)
  (declare (single-float s) (optimize (speed 3) (safety 1) (debug 0)))
  (sqrt s))

(defun sdd-single-widened (s)
  (declare (single-float s) (optimize (speed 3) (safety 1) (debug 0)))
  (the double-float (sqrt (float s 1.0d0))))

(deftest sqrt-single-float-stays-single
  (let ((r (sdd-single 4.0f0)))
    (list (typep r 'single-float) (typep r 'double-float) (= r 2.0f0)))
  (t nil t))

(deftest sqrt-single-float-widened-is-double
  (let ((r (sdd-single-widened 4.0f0)))
    (list (typep r 'double-float) (= r 2.0d0)))
  (t t))

;;; --- the shape this exists for ---
;;;
;;; A square root inside a larger declared double computation: the argument is
;;; built by declared arithmetic and must reach the root without being boxed to
;;; get there. Only the value is asserted; the instruction counts are the
;;; il-parity harness's business.

(defun sdd-hypot (x y)
  (declare (double-float x y) (optimize (speed 3) (safety 0) (debug 0)))
  (the double-float (sqrt (the double-float
                               (+ (the double-float (* x x))
                                  (the double-float (* y y)))))))

(deftest sqrt-declared-double-in-a-computation
  (list (sdd-hypot 3.0d0 4.0d0) (sdd-hypot 0.0d0 0.0d0)
        (= (sdd-hypot 1.0d0 1.0d0) (sqrt 2.0d0)))
  (5.0d0 0.0d0 t))

;;; Bound to a declared double local rather than returned: the same lowering is
;;; reached from the binding's own declaration.

(defun sdd-hypot-bound (x y)
  (declare (double-float x y) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((len (the double-float (sqrt (the double-float (+ (* x x) (* y y)))))))
    (declare (double-float len))
    (if (= len 0.0d0) 0.0d0 (the double-float (/ len 2.0d0)))))

(deftest sqrt-declared-double-bound-to-a-local
  (list (sdd-hypot-bound 3.0d0 4.0d0) (sdd-hypot-bound 0.0d0 0.0d0))
  (2.5d0 0.0d0))

;;; A non-constant argument that is not statically a double keeps the generic
;;; call: an integer argument still answers the integer-root the standard asks
;;; for, not a double.

(defun sdd-any (x) (sqrt x))

(deftest sqrt-undeclared-integer-argument
  (list (sdd-any 4) (sdd-any 2.25d0))
  (2.0 1.5d0))
