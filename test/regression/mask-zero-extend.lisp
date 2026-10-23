;;; An int64 masked to its low 8, 16 or 32 bits is compiled as zero extension
;;; (conv.u1 / conv.u2 / conv.u4 then conv.u8) instead of ldc.i8 and AND.

(setf dotcl:*save-sil* t)

(defun %mze-masks (x)
  (declare (type (signed-byte 64) x))
  (list (logand x #xFF) (logand x #xFFFF) (logand x #xFFFFFFFF)
        (ldb (byte 32 0) x) (logand #xFFFFFFFF (+ x 1))))

(defun %mze-sum (a b)
  (declare (type (unsigned-byte 32) a b))
  (let ((s 0))
    (declare (type (unsigned-byte 32) s))
    (dotimes (i 5 s)
      (setq s (ldb (byte 32 0) (+ s (ldb (byte 32 0) (* a b)) i))))))

(deftest mask-zero-extend.values
  (list (%mze-masks -1)
        (%mze-masks most-negative-fixnum)
        (%mze-masks #x123456789ABCDEF0)
        (%mze-masks 0)
        (%mze-masks -4294967296)
        (%mze-sum #xFFFFFFFF #xFFFFFFFE))
  ((255 65535 4294967295 4294967295 0)
   (0 0 0 0 1)
   (240 57072 2596069104 2596069104 2596069105)
   (0 0 0 0 1)
   (0 0 0 0 1)
   20))

(deftest-emitting-only mask-zero-extend.sil
  (let ((s (%flat-text (dotcl:function-sil #'%mze-masks))))
    (list (and (search "CONV-U4" s) t) (and (search "CONV-U2" s) t) (and (search "CONV-U1" s) t)))
  (t t t))
