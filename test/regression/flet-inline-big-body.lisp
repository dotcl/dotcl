;;; An FLET whose INLINE functions are called at many sites is substituted at
;;; every one when the body, after the peephole pass, stays under the JIT's
;;; limits. The shape is ironclad's RIPEMD-160 block: 160 rounds that each call
;;; a small round function on (unsigned-byte 32) words (240 here).

(setf dotcl:*save-sil* t)

(defmacro %fib-rounds (n)
  `(progn
     ,@(loop for k below n
             for (a b c d e) = (nth (mod k 5) '((a b c d e) (e a b c d) (d e a b c)
                                                 (c d e a b) (b c d e a)))
             collect `(setf ,a (ldb (byte 32 0)
                                    (+ ,a (funcall #',(if (evenp k) 'f 'g) ,b ,c ,d)
                                       (aref blk ,(mod k 16)) ,(* k 7919))))
             collect `(setf ,a (ldb (byte 32 0)
                                    (+ (logior (ldb (byte 32 0) (ash ,a ,(1+ (mod k 13))))
                                               (ash ,a ,(- (1+ (mod k 13)) 32)))
                                       ,e)))
             collect `(setf ,c (logior (ldb (byte 32 0) (ash ,c 10)) (ash ,c -22))))))

(defun %fib-block (blk)
  (declare (type (simple-array (unsigned-byte 32) (16)) blk)
           (optimize (speed 3) (safety 0)))
  (let ((a 1) (b 2) (c 3) (d 4) (e 5))
    (declare (type (unsigned-byte 32) a b c d e))
    (flet ((f (x y z)
             (declare (type (unsigned-byte 32) x y z))
             (ldb (byte 32 0) (logxor x y z)))
           (g (x y z)
             (declare (type (unsigned-byte 32) x y z))
             (ldb (byte 32 0) (logxor z (logand x (logxor y z))))))
      (declare (inline f g))
      (%fib-rounds 240))
    (list a b c d e)))

(deftest flet-inline-big-body.values
  (%fib-block (make-array 16 :element-type '(unsigned-byte 32)
                             :initial-contents (loop for i below 16 collect (* i #x01010101))))
  (137862136 901406442 430767854 303152996 3746323017))

(deftest-emitting-only flet-inline-big-body.all-substituted
  (search "LispFunction.Invoke" (%flat-text (dotcl:function-sil #'%fib-block)))
  nil)
