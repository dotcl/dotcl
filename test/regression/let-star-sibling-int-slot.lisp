;;; A LET* binding declared an integer type whose init is an earlier sibling in
;;; a raw Int64 slot gets a raw slot too (ironclad's RIPEMD-160 starts its
;;; second line of registers as (a2 a1)). The init used to be judged in the
;;; scope outside the LET*, where the sibling is not bound, so the slot was a
;;; LispObject and every assignment boxed.

(setf dotcl:*save-sil* t)

(defun %lsis-rounds (v n)
  (declare (type (simple-array (unsigned-byte 32) (4)) v) (fixnum n))
  (let* ((a1 (aref v 0)) (a2 a1)
         (b1 (aref v 1)) (b2 b1))
    (declare (type (unsigned-byte 32) a1 a2 b1 b2))
    (dotimes (i n)
      (setf a1 (ldb (byte 32 0) (+ a1 (aref v 3) b2)))
      (setf a2 (ldb (byte 32 0) (logxor (ash a2 5) a1)))
      (setf b1 (ldb (byte 32 0) (+ b1 a2)))
      (setf b2 (ldb (byte 32 0) (- b2 b1))))
    (setf (aref v 0) a1 (aref v 1) a2 (aref v 2) (logxor b1 b2))
    (values)))

;;; The sibling is special, or not declared: the later binding is not raw on its
;;; strength, and its value is still right.
(defvar *lsis-x* 5)

(defun %lsis-special ()
  (let* ((*lsis-x* 7) (y *lsis-x*))
    (declare (type (unsigned-byte 32) y))
    (list y (+ y 1))))

(defun %lsis-undeclared (x)
  (let* ((a x) (b a))
    (declare (type (unsigned-byte 32) b))
    (list a b)))

(defun %lsis-shadow (a1)
  (declare (fixnum a1))
  (let* ((a1 (1+ a1)) (a2 a1) (a1 (* a2 2)) (a3 a1))
    (declare (type (unsigned-byte 32) a2 a3))
    (list a2 a3)))

(deftest let-star-sibling-int-slot.values
  (list (let ((v (make-array 4 :element-type '(unsigned-byte 32)
                               :initial-contents '(1 #x12345678 0 #x9e3779b9))))
          (%lsis-rounds v 1000)
          (coerce (subseq v 0 3) 'list))
        (%lsis-special)
        (%lsis-undeclared 9)
        (%lsis-shadow 3))
  ((4253427950 1250890478 2684796857) (7 8) (9 9) (4 8)))

(deftest-emitting-only let-star-sibling-int-slot.unboxed
  (search "Fixnum.Make" (%flat-text (dotcl:function-sil #'%lsis-rounds)))
  nil)
