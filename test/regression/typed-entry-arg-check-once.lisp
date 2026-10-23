;;; A typed entry checks each integer argument against the range its FTYPE
;;; declares; the body does not check it again when its own declaration admits
;;; that whole range. A narrower declaration in the body is still checked, and
;;; a tail self-call still checks the arguments it loops back with.

(setf dotcl:*save-sil* t)

(declaim (ftype (function ((unsigned-byte 32)) (unsigned-byte 8)) %teco-red))
(defun %teco-red (c) (ldb (byte 8 16) c))
(declaim (ftype (function ((unsigned-byte 32)) (unsigned-byte 8)) %teco-narrow))
(defun %teco-narrow (c) (declare (type (unsigned-byte 16) c)) (ldb (byte 8 8) c))
(declaim (ftype (function ((unsigned-byte 8)) fixnum) %teco-wide))
(defun %teco-wide (c) (declare (type (unsigned-byte 32) c)) (* c 2))
(declaim (ftype (function ((integer 0 100)) fixnum) %teco-loop))
(defun %teco-loop (n) (if (> n 50) n (%teco-loop (+ n 60))))

(defun %teco-sil-count (fn what)
  (let ((s (%flat-text (dotcl:function-sil fn))) (n 0) (i 0))
    (loop (let ((p (search what s :start2 i)))
            (unless p (return n))
            (incf n)
            (setq i (1+ p))))))

(deftest typed-entry-arg-check-once.values
  (flet ((te (thunk) (handler-case (funcall thunk) (type-error () :type-error))))
    (list (%teco-red #x00ab0000)
          (te (lambda () (%teco-red (expt 2 32))))
          (%teco-narrow #x1234)
          (te (lambda () (%teco-narrow #x10000)))
          (%teco-wide 200)
          (te (lambda () (%teco-wide 300)))
          (%teco-loop 10)
          (te (lambda () (%teco-loop 45)))
          (te (lambda () (%teco-loop 101)))
          (funcall #'%teco-red #x00cd0000)
          (te (lambda () (funcall #'%teco-red -1)))))
  (#xab :type-error #x12 :type-error 400 :type-error 70 :type-error :type-error #xcd :type-error))

(deftest-compiled-only typed-entry-arg-check-once.count
  (list (%teco-sil-count #'%teco-red "CheckLongRange")
        (%teco-sil-count #'%teco-narrow "CheckLongRange")
        (%teco-sil-count #'%teco-wide "CheckLongRange"))
  (1 2 1))

(setf dotcl:*save-sil* nil)
