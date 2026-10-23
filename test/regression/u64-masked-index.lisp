;;; A subscript that is a masked piece of an (UNSIGNED-BYTE 64) word, such as
;;; (LOGAND (ASH W -8) #xFF) or (LDB (BYTE 8 16) W), is computed in the 64-bit
;;; lane and used raw: its value fits an int64 whatever the word. It used to be
;;; boxed (BoxU64) and unboxed again (IndexL) for every table lookup, the shape
;;; of ironclad's Kupyna / Kalyna / Streebog rounds.

(setf dotcl:*save-sil* t)

(defconstant +umi-t+
  (let ((a (make-array '(2 256) :element-type '(unsigned-byte 64))))
    (dotimes (i 2 a)
      (dotimes (j 256)
        (setf (aref a i j) (ldb (byte 64 0) (* (+ j 1 (* i 977)) #x9E3779B97F4A7C15)))))))

(declaim (inline %umi-ash))
(defun %umi-ash (num count)
  (declare (type (unsigned-byte 64) num) (type (integer -63 63) count))
  (ldb (byte 64 0) (ash num count)))

(defun %umi-mix (x)
  (declare (type (simple-array (unsigned-byte 64) (*)) x))
  (setf (aref x 1)
        (logxor (aref +umi-t+ 1 (logand (%umi-ash (aref x 0) -56) #xff))
                (aref +umi-t+ 0 (logand (ash (aref x 0) -8) #xff))
                (aref +umi-t+ 1 (ldb (byte 8 16) (aref x 0)))
                (aref +umi-t+ 0 (logand #xff (aref x 0)))))
  (values))

(deftest u64-masked-index.values
  (let ((x (make-array 2 :element-type '(unsigned-byte 64)
                         :initial-contents '(#xF1E2D3C4B5A69788 0))))
    (%umi-mix x)
    (aref x 1))
  4507831506992062818)

(deftest-emitting-only u64-masked-index.raw
  (let ((s (%flat-text (dotcl:function-sil #'%umi-mix))))
    (list (search "Runtime.IndexL" s) (search "Runtime.BoxU64" s)))
  (nil nil))
