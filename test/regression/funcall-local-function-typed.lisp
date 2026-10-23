;;; (FUNCALL #'F ...) of a local function is typed as the call (F ...): bound to
;;; the parameter of an INLINE function declared (UNSIGNED-BYTE 32), its value
;;; stays a raw integer instead of being boxed for every call.

(setf dotcl:*save-sil* t)

(declaim (inline %flft-m32+))
(defun %flft-m32+ (a b)
  (declare (type (unsigned-byte 32) a b))
  (ldb (byte 32 0) (+ a b)))

(defun %flft-rounds (v n)
  (declare (type (simple-array (unsigned-byte 32) (4)) v) (fixnum n))
  (let ((a (aref v 0)) (b (aref v 1)) (c (aref v 2)))
    (declare (type (unsigned-byte 32) a b c))
    (flet ((f (x y z)
             (declare (type (unsigned-byte 32) x y z))
             (ldb (byte 32 0) (logxor (ash x 3) y z))))
      (declare (inline f))
      (dotimes (i n)
        (setf a (%flft-m32+ a (%flft-m32+ (funcall #'f b c a) (%flft-m32+ (aref v 3) #x5a827999))))
        (setf b (%flft-m32+ b (funcall (function f) a c b)))
        (setf c (%flft-m32+ c (f a b c)))))
    (setf (aref v 0) a (aref v 1) b (aref v 2) c)
    (values)))

(defun %flft-not-inline (x)
  (flet ((f (y) (* y y)))
    (declare (notinline f))
    (%flft-m32+ (funcall #'f x) 1)))

(deftest funcall-local-function-typed.values
  (list (let ((v (make-array 4 :element-type '(unsigned-byte 32)
                                 :initial-contents '(1 #x12345678 #x80000000 7))))
          (%flft-rounds v 1000)
          (coerce (subseq v 0 3) 'list))
        (%flft-not-inline #xFFFF))
  ((1385336016 3602407856 1110181680) 4294836226))

(deftest-emitting-only funcall-local-function-typed.unboxed
  (search "Fixnum.Make" (%flat-text (dotcl:function-sil #'%flft-rounds)))
  nil)
