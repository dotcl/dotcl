;;; (LOGAND X #xFFFFFFFFFFFFFFFF), the mask of every 64-bit modular sum
;;; ((LDB (BYTE 64 0) (+ A B)), ironclad's MOD64+), is X itself in the 64-bit
;;; lane: the lane computes with int64 wraparound, so ANDing with all 64 bits set
;;; changes nothing. It was emitted as (:LDC-I8 -1) (:AND), 10 IL bytes and two
;;; instructions per sum; ironclad's THREEFISH1024-ENCRYPT-BLOCK had 1,679 of them.

(setf dotcl:*save-sil* t)

(defun %ufm-sil-count (fn text)
  (let ((s (%flat-text (dotcl:function-sil fn))) (n 0) (p 0))
    (loop (let ((q (search text s :start2 p)))
            (unless q (return n))
            (incf n)
            (setf p (1+ q))))))

(defun %ufm-sum3 (w)
  (declare (type (simple-array (unsigned-byte 64) (*)) w)
           (optimize speed (safety 0)))
  (setf (aref w 0) (ldb (byte 64 0) (+ (aref w 1) (aref w 2) (aref w 3))))
  (setf (aref w 4) (logand (+ (aref w 0) (aref w 1)) #xFFFFFFFFFFFFFFFF))
  w)

(deftest-compiled-only ufm-sum-no-all-ones-mask
  (%ufm-sil-count #'%ufm-sum3 "(LDC-I8 -1) (AND)")
  0)

(deftest ufm-sum-value
  (let ((w (make-array 5 :element-type '(unsigned-byte 64) :initial-element 0)))
    (setf (aref w 1) #xFFFFFFFFFFFFFFFF
          (aref w 2) #x8000000000000001
          (aref w 3) 5)
    (%ufm-sum3 w)
    (list (aref w 0) (aref w 4)))
  (#x8000000000000005 #x8000000000000004))
