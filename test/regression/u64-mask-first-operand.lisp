;;; (logand #xFFFFFFFFFFFFFFFF X) with the mask FIRST is the same 64-bit modular expression as
;;; (logand X #xFFFFFFFFFFFFFFFF): the u64 lane takes it unboxed. The values must not change.

(defun %umf-first (n a)
  (declare (fixnum n) (type (simple-array (unsigned-byte 64) (4)) a))
  (let ((s 0)) (declare (type (unsigned-byte 64) s))
    (dotimes (i n) (setq s (logand #xFFFFFFFFFFFFFFFF (+ s (aref a (logand i 3))))))
    s))

(defun %umf-second (n a)
  (declare (fixnum n) (type (simple-array (unsigned-byte 64) (4)) a))
  (let ((s 0)) (declare (type (unsigned-byte 64) s))
    (dotimes (i n) (setq s (logand (+ s (aref a (logand i 3))) #xFFFFFFFFFFFFFFFF)))
    s))

(defun %umf-mix (n a)
  (declare (fixnum n) (type (simple-array (unsigned-byte 64) (4)) a))
  (let ((s 1)) (declare (type (unsigned-byte 64) s))
    (dotimes (i n)
      (setq s (logand #xFFFFFFFFFFFFFFFF (* s 31)))
      (setq s (logxor #xF0F0F0F0F0F0F0F0 (logand #xFFFFFFFFFFFFFFFF (+ s (aref a (logand i 3))))))
      (setq s (logior #x0000000000000001 (logand #xFFFFFFFFFFFFFFFF (+ s i)))))
    s))

(defun %umf-model (n elts)
  (let ((s 0)) (dotimes (i n s) (setq s (ldb (byte 64 0) (+ s (nth (logand i 3) elts)))))))

(defun %umf-mix-model (n elts)
  (let ((s 1))
    (dotimes (i n s)
      (setq s (ldb (byte 64 0) (* s 31)))
      (setq s (logxor #xF0F0F0F0F0F0F0F0 (ldb (byte 64 0) (+ s (nth (logand i 3) elts)))))
      (setq s (logior 1 (ldb (byte 64 0) (+ s i)))))))

(deftest u64-mask-first-operand.sum
  (let* ((elts (list #xFFFFFFFFFFFFFFFF #x8000000000000001 7 #x123456789ABCDEF0))
         (a (make-array 4 :element-type '(unsigned-byte 64) :initial-contents elts)))
    (list (= (%umf-first 11 a) (%umf-model 11 elts))
          (= (%umf-second 11 a) (%umf-model 11 elts))
          (= (%umf-first 1000 a) (%umf-model 1000 elts))))
  (t t t))

(deftest u64-mask-first-operand.mix
  (let* ((elts (list #xFFFFFFFFFFFFFFFF #x8000000000000001 7 #x123456789ABCDEF0))
         (a (make-array 4 :element-type '(unsigned-byte 64) :initial-contents elts)))
    (= (%umf-mix 50 a) (%umf-mix-model 50 elts)))
  t)
