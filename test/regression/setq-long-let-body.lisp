;;; (SETQ V (LET (...) ... E)) with V in an int64 slot assigns E inside the LET,
;;; so an inline call in E stays raw instead of being boxed and unboxed.

(setf dotcl:*save-sil* t)

(declaim (inline %slb-rol) (ftype (function ((unsigned-byte 32) (unsigned-byte 5)) (unsigned-byte 32)) %slb-rol))
(defun %slb-rol (a s)
  (declare (type (unsigned-byte 32) a) (type (integer 0 32) s))
  (logior (ldb (byte 32 0) (ash a s)) (ash a (- s 32))))

(defun %slb-let (x)
  (declare (type (unsigned-byte 32) x))
  (let ((b 7))
    (declare (type (unsigned-byte 32) b))
    (setf b (let ((y (ldb (byte 32 0) (+ x b))))
              (declare (type (unsigned-byte 32) y))
              (%slb-rol y (mod y 32))))
    b))

(defun %slb-forms (x)
  (declare (type (unsigned-byte 32) x))
  (let ((b 1) (log '()))
    (declare (type (unsigned-byte 32) b))
    (setq b (let* ((p (ldb (byte 32 0) (* x 3))) (q (logxor p b)))
              (push p log)
              (%slb-rol q 3)))
    (setq b (progn (push b log) (%slb-rol b 1)))
    (setq b (locally (declare (optimize speed)) (%slb-rol b 31)))
    (list b (reverse log))))

;; The LET binds B itself, or declares it: B stays the outer variable.
(defun %slb-shadow (x)
  (declare (type (unsigned-byte 32) x))
  (let ((b 5))
    (declare (type (unsigned-byte 32) b))
    (let ((r (setq b (let ((b (+ x 1))) (* b 2)))))
      (list r b (setq b (let ((y 3)) (declare (ignorable b)) (+ b y)))))))

(deftest setq-long-let-body.values
  (list (%slb-let 0) (%slb-let #xfffffff0) (%slb-let 12345)
        (%slb-forms 5) (%slb-forms #xffffffff) (%slb-shadow 10))
  (896 4227858431 12352 (112 (15 112)) (4294967271 (4294967293 4294967271))
   (22 22 25)))

(deftest-compiled-only setq-long-let-body.raw
  (search "unbox-fixnum" (%flat-text (dotcl:function-sil #'%slb-let)))
  nil)
