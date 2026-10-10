;;; LOGANDC1 / LOGANDC2 of (unsigned-byte 64) words are computed in the 64-bit
;;; lane as (LOGAND (LOGNOT A) B) / (LOGAND A (LOGNOT B)). They used to be full
;;; calls on boxed operands (ironclad's SHA-512 choice function, 80 a block),
;;; and their result an operand the lane had to check at run time.

(setf dotcl:*save-sil* t)

(defun %ulc2-sil-count (fn text)
  (let ((s (%flat-text (dotcl:function-sil fn))) (n 0) (p 0))
    (loop (let ((q (search text s :start2 p)))
            (unless q (return n))
            (incf n)
            (setf p (1+ q))))))

(defun %ulc2-ch (w)
  (declare (type (simple-array (unsigned-byte 64) (*)) w))
  (let ((e (aref w 0)) (f (aref w 1)) (g (aref w 2)))
    (declare (type (unsigned-byte 64) e f g))
    (setf (aref w 3) (logxor (logand e f) (logandc1 e g))
          (aref w 4) (logandc2 e g))
    w))

;; LOGNOT bound by an FLET: not the lane's LOGNOT.
(defun %ulc2-shadowed (a b)
  (declare (type (unsigned-byte 64) a b))
  (flet ((lognot (x) (list :local x)))
    (list (logandc1 a b) (lognot 1))))

(deftest-compiled-only ulc2-no-call
  (list (%ulc2-sil-count #'%ulc2-ch "LOGANDC1")
        (%ulc2-sil-count #'%ulc2-ch "LOGANDC2"))
  (0 0))

(deftest ulc2-values
  (let ((w (make-array 5 :element-type '(unsigned-byte 64) :initial-element 0)))
    (setf (aref w 0) #xF0F0F0F0F0F0F0F0
          (aref w 1) #xFF00FF00FF00FF00
          (aref w 2) #x123456789ABCDEF0)
    (%ulc2-ch w)
    (list (aref w 3) (aref w 4)
          (logandc1 #xF0F0F0F0F0F0F0F0 #x123456789ABCDEF0)
          (%ulc2-shadowed #xFFFFFFFFFFFFFFFF #x8000000000000001)))
  (#xF204F608FA0CFE00 #xE0C0A08060402000 #x020406080A0C0E00 (0 (:local 1))))
