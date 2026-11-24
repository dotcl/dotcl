;;; The assigned-binding solver bounds a variable that is multiplied by a constant in straight-line
;;; code (jzon's (setf m2 (* m2 10)) in a WHEN): the multiplications run a counted number of times, so
;;; the value ends at most K^TRIPS times what it was. A multiplication in a loop the solver cannot
;;; count still grows without a bound: the variable stays boxed and becomes a bignum, as it must.

(setf dotcl:*save-sil* t)

(defun asm-w1 (a mant)
  (declare (type (unsigned-byte 11) a) (type (unsigned-byte 52) mant))
  (let* ((m2 (logior mant #x10000000000000)))
    (when (zerop a) (setf m2 mant) (when (< mant 3) (setf m2 (* m2 10))))
    (+ (* m2 4) 1)))

(defun asm-w1-ref (a mant)
  (let ((m2 (logior mant #x10000000000000)))
    (when (zerop a) (setf m2 mant) (when (< mant 3) (setf m2 (* m2 10))))
    (+ (* m2 4) 1)))

(defun asm-two (n)
  (declare (type (unsigned-byte 8) n))
  (let ((m n))
    (when (> n 5) (setq m (* m 10)))
    (when (> n 9) (setq m (* m 10)))
    (+ m 1)))

(defun asm-add-then-mul (n)
  (declare (type (unsigned-byte 8) n))
  (let ((m n))
    (setq m (+ m 1000))
    (setq m (* m 8))
    (setq m (+ m 7))
    m))

(defun asm-loop (n)
  (declare (type (unsigned-byte 8) n))
  (let ((m 1))
    (dotimes (i n) (setq m (* m 10)))
    m))

(defun asm-while (n)
  (declare (type (unsigned-byte 8) n))
  (let ((m 3) (i 0))
    (loop (when (>= i n) (return m)) (setq m (* m 10)) (incf i))))

(defun asm-negative (n)
  (declare (type (signed-byte 8) n))
  (let ((m n))
    (when (zerop (logand n 1)) (setq m (* m 1000000)))
    (list m (* m 3))))

(setf dotcl:*save-sil* nil)

(deftest avt-self-multiply-range.values-equal-the-boxed-reference
  (loop for a in '(0 1 2047) always
        (loop for mant in '(0 1 2 3 4 (ash 1 51) 4503599627370495) always
              (let ((mant (if (consp mant) (ash 1 51) mant)))
                (eql (asm-w1 a mant) (asm-w1-ref a mant)))))
  t)

(deftest avt-self-multiply-range.compounding-and-additions
  (list (asm-two 3) (asm-two 7) (asm-two 10) (asm-two 255) (asm-add-then-mul 0) (asm-add-then-mul 255))
  (4 71 1001 25501 8007 10047))

(deftest avt-self-multiply-range.loops-still-grow-into-bignums
  (list (asm-loop 0) (asm-loop 3) (asm-loop 25) (asm-while 20) (typep (asm-loop 25) 'bignum))
  (1 1000 10000000000000000000000000 300000000000000000000 t))

(deftest avt-self-multiply-range.negative-values-are-left-alone
  (list (asm-negative -3) (asm-negative -4) (asm-negative 100) (asm-negative -128))
  ((-3 -9) (-4000000 -12000000) (100000000 300000000) (-128000000 -384000000)))

(defun asm-slot-int64-p (sil name)
  (let ((p (search name sil)))
    (and p (search "Int64" sil :start2 p :end2 (min (length sil) (+ p 40))) t)))

(deftest-emitting-only avt-self-multiply-range.the-straight-line-variable-is-an-int64-slot
  (let ((a (%flat-text (dotcl:function-sil #'asm-w1)))
        (b (%flat-text (dotcl:function-sil #'asm-loop))))
    (list (asm-slot-int64-p a "(DECLARE-LOCAL M2_") (asm-slot-int64-p b "(DECLARE-LOCAL M_")))
  (t nil))
