;;; (FUNCALL #'F ...) for a local function F is the call (F ...): an INLINE F
;;; is substituted there too (ironclad's RIPEMD rounds call their round
;;; functions this way), and the values, the arity check and recursion are
;;; those of the call.

(setf dotcl:*save-sil* t)

(defun %flf-inline (a b c)
  (declare (type (unsigned-byte 32) a b c))
  (flet ((f (x y z)
           (declare (type (unsigned-byte 32) x y z))
           (ldb (byte 32 0) (logxor z (logand x (logxor y z))))))
    (declare (inline f))
    (list (funcall #'f a b c) (funcall (function f) c b a) (f b a c))))

(defun %flf-values ()
  (flet ((f (x) (values x (1+ x))))
    (multiple-value-list (funcall #'f 1))))

(defun %flf-labels (n)
  (labels ((fact (k) (if (<= k 1) 1 (* k (funcall #'fact (1- k))))))
    (funcall #'fact n)))

(defun %flf-arity ()
  (flet ((f (x) x))
    (declare (notinline f))
    (handler-case (funcall #'f 1 2)
      (program-error () :program-error))))

(defun %flf-g (y) (list :global y))

(defun %flf-shadowed-global (x)
  (flet ((%flf-g (y) (list :local y)))
    (list (funcall #'%flf-g x) (funcall '%flf-g x))))

(deftest funcall-local-function.values
  (list (%flf-inline #xFFFF0000 #x12345678 #xDEADBEEF)
        (%flf-values)
        (%flf-labels 10)
        (%flf-arity)
        (%flf-shadowed-global 7))
  ((305446639 863377000 3736971399) (1 2) 3628800 :program-error ((:local 7) (:global 7))))

(deftest-emitting-only funcall-local-function.inlined
  (search "LispFunction.Invoke" (%flat-text (dotcl:function-sil #'%flf-inline)))
  nil)
