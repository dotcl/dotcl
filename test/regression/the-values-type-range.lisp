;;; (THE (VALUES T1) F) and (THE (VALUES T1 &OPTIONAL) F) assert one value of type T1, so they carry the
;;; range of T1 as (THE T1 F) does: a variable bound to the result of an INLINE function whose body ends
;;; in such a THE (jzon's %rop3) is an int64 slot. A VALUES type that asserts more than one value, or no
;;; first value, tells nothing about the primary value.

(setf dotcl:*save-sil* t)

(declaim (inline tvr-mix))
(defun tvr-mix (a b)
  (declare (type (signed-byte 64) a b))
  (the (values (signed-byte 64) &optional)
    (logxor (logand a #x7FFFFFFF) (ash b -3))))

(defun tvr-use (a b c)
  (declare (type (signed-byte 64) a b c))
  (let* ((v (tvr-mix a b)) (w (tvr-mix v c)))
    (list v w (+ v w))))

(defun tvr-ref (a b c)
  (let* ((v (logxor (logand a #x7FFFFFFF) (ash b -3))) (w (logxor (logand v #x7FFFFFFF) (ash c -3))))
    (list v w (+ v w))))

(defun tvr-the-one (x)
  (declare (type (signed-byte 64) x))
  (let ((v (the (values (signed-byte 32)) (logand x #xFFFF))))
    (+ (* v 3) v 1)))

(defun tvr-the-two (x)
  (declare (type (signed-byte 64) x))
  (let ((v (the (values (signed-byte 32) fixnum) (values (logand x #xFFFF) 7))))
    (+ (* v 3) v 1)))

(setf dotcl:*save-sil* nil)

(deftest the-values-type-range.inline-values-type-matches-the-reference
  (loop for a in (list 0 1 -1 (1- (expt 2 63)) (- (expt 2 63)) 123456789012345)
        always (loop for b in (list 0 7 -9 (1- (expt 2 63)) (- (expt 2 63)))
                     always (loop for c in (list 0 -1 (expt 2 40))
                                  always (equal (tvr-use a b c) (tvr-ref a b c)))))
  t)

(deftest the-values-type-range.the-with-one-or-two-values
  (list (tvr-the-one 5) (tvr-the-one 70000) (tvr-the-two 5) (tvr-the-two 70000))
  (21 17857 21 17857))

(defun tvr-slot-int64-p (sil name)
  (let ((p (search name sil)))
    (and p (search "Int64" sil :start2 p :end2 (min (length sil) (+ p 40))) t)))

(deftest-emitting-only the-values-type-range.the-variables-are-int64-slots
  (let ((a (%flat-text (dotcl:function-sil #'tvr-use)))
        (b (%flat-text (dotcl:function-sil #'tvr-the-one))))
    (list (tvr-slot-int64-p a "(DECLARE-LOCAL V_") (tvr-slot-int64-p a "(DECLARE-LOCAL W_")
          (tvr-slot-int64-p b "(DECLARE-LOCAL V_")))
  (t t t))
