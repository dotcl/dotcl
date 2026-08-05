;;; Regression: a type declaration must cover only the binding it was written
;;; for. An inner binding of the same name is a different variable.
;;;
;;; The type-local tables (*fixnum-locals*, *double-float-locals*, …) are keyed
;;; by variable NAME string and were only appended to when entering a binding
;;; form, never filtered. So (let ((n 1)) (declare (fixnum n)) (let ((n 2.5d0))
;;; (* n 2))) still saw "N" as declared fixnum inside the inner LET and unboxed
;;; the DoubleFloat slot as a Fixnum — a hard InvalidCastException, not a wrong
;;; value.

(deftest type-decl-shadow-double
  (let ((n 1))
    (declare (fixnum n))
    (let ((n 2.5d0))
      (* n 2)))
  5.0d0)

(deftest type-decl-shadow-ratio
  (let ((n 1))
    (declare (fixnum n))
    (let ((n 1/3))
      (+ n 1)))
  4/3)

(deftest type-decl-shadow-bignum
  (let ((n 1))
    (declare (fixnum n))
    (let ((n (* 4611686018427387903 4)))
      (+ n 1)))
  18446744073709551613)

;;; let* binds sequentially — same rule.
(deftest type-decl-shadow-let*
  (let ((n 2))
    (declare (fixnum n))
    (let* ((m (+ n 1))
           (n 1.5d0))
      (list m (* n 2))))
  (3 3.0d0))

;;; A lambda parameter shadowing an enclosing declared name.
(deftest type-decl-shadow-param
  (let ((n 3))
    (declare (fixnum n))
    (list n (funcall (lambda (n) (* n 2)) 1.25d0)))
  (3 2.5d0))

;;; A double-float declaration must not leak into an inner integer binding.
(deftest type-decl-shadow-double-decl
  (let ((d 1.5d0))
    (declare (double-float d))
    (let ((d 3))
      (+ d 1)))
  4)

;;; The outer declared binding still works after the shadowing scope closes.
(deftest type-decl-shadow-restore
  (let ((n 3))
    (declare (fixnum n))
    (let ((n 2.5d0)) (declare (ignorable n)) nil)
    (+ n 1))
  4)
