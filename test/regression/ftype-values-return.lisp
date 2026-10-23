;;; A declaimed return type written (VALUES X &OPTIONAL) or (VALUES X), the way
;;; SBCL-style code spells a single result, is X for the caller: an integer
;;; range, a fixnum, a float. Other VALUES types say nothing about the call.

(setf dotcl:*save-sil* t)

(defun %fvr-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %fvr-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(declaim (ftype (function ((unsigned-byte 32)) (values (unsigned-byte 8) &optional)) %fvr-red))
(defun %fvr-red (c) (ldb (byte 8 16) c))
(defun %fvr-sum (v)
  (declare (type (simple-array (unsigned-byte 32) (*)) v))
  (let ((s 0)) (declare (fixnum s))
    (dotimes (i (length v)) (incf s (%fvr-red (aref v i))))
    s))

(declaim (inline %fvr-green)
         (ftype (function ((unsigned-byte 32)) (values (unsigned-byte 8) &optional)) %fvr-green))
(defun %fvr-green (c) (ldb (byte 8 8) c))
(defun %fvr-sum-inline (v)
  (declare (type (simple-array (unsigned-byte 32) (*)) v))
  (let ((s 0)) (declare (fixnum s))
    (dotimes (i (length v)) (incf s (%fvr-green (aref v i))))
    s))

(declaim (ftype (function (double-float) (values double-float)) %fvr-twice))
(defun %fvr-twice (x) (* x 2d0))
(defun %fvr-dsum (a b) (declare (double-float a b)) (+ (%fvr-twice a) (%fvr-twice b)))

(declaim (ftype (function (fixnum) (values fixnum fixnum)) %fvr-two))
(defun %fvr-two (x) (values x (1+ x)))
(defun %fvr-two-use (a) (declare (fixnum a)) (+ (%fvr-two a) 1))

(defun %fvr-vec ()
  (let ((v (make-array 3 :element-type '(unsigned-byte 32))))
    (setf (aref v 0) #xff102030 (aref v 1) #x00405060 (aref v 2) #xffffffff)
    v))

(deftest ftype-values-return.call
  (%fvr-sum (%fvr-vec))
  335)

(deftest ftype-values-return.inline
  (%fvr-sum-inline (%fvr-vec))
  367)

(deftest ftype-values-return.double
  (%fvr-dsum 1d0 2.5d0)
  7d0)

(deftest ftype-values-return.two-values
  (list (%fvr-two-use 4) (multiple-value-list (%fvr-two 4)))
  (5 (4 5)))

(deftest-emitting-only ftype-values-return.call-is-fixnum
  (%fvr-count "Runtime.Add)" (%fvr-sil #'%fvr-sum))
  0)

(deftest-emitting-only ftype-values-return.inline-is-fixnum
  (%fvr-count "Runtime.Add)" (%fvr-sil #'%fvr-sum-inline))
  0)

(deftest-emitting-only ftype-values-return.double-is-r8
  (let ((r (%fvr-sil #'%fvr-dsum)))
    (list (%fvr-count "Runtime.Add)" r) (%fvr-count "UNBOX-DOUBLE" r)))
  (0 2))

(deftest-emitting-only ftype-values-return.two-values-stays-generic
  (plusp (%fvr-count "Runtime.Increment" (%fvr-sil #'%fvr-two-use)))
  t)
