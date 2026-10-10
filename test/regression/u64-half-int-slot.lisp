;;; Regression tests for a LET binding declared to hold a 32-bit (or any
;;; int64-sized) integer whose init is a part of an (unsigned-byte 64) word:
;;; (LOGAND W #xFFFFFFFF), (LDB (BYTE 32 32) W). The word is held as its bits in
;;; an Int64 slot; the half used to be computed in the 64-bit lane, boxed with
;;; Runtime.BoxU64 into a LispObject slot, and unboxed again by every read in the
;;; body (ironclad's (SETF UB64REF/LE) passing each half to (SETF UB32REF/LE)).
;;; The binding is now a raw Int64 slot initialised from the lane directly.

(setf dotcl:*save-sil* t)

(defun %uhs-sil-count (fn text)
  (let ((s (%flat-text (dotcl:function-sil fn))) (n 0) (p 0))
    (loop (let ((q (search text s :start2 p)))
            (unless q (return n))
            (incf n)
            (setf p (1+ q))))))

(declaim (inline (setf %uhs-u32) (setf %uhs-u64)))
(defun (setf %uhs-u32) (val v off)
  (declare (type (unsigned-byte 32) val)
           (type (simple-array (unsigned-byte 8) (*)) v)
           (type (mod 4611686018427387901) off))
  (setf (aref v off) (ldb (byte 8 0) val)
        (aref v (+ off 1)) (ldb (byte 8 8) val)
        (aref v (+ off 2)) (ldb (byte 8 16) val)
        (aref v (+ off 3)) (ldb (byte 8 24) val))
  val)
(defun (setf %uhs-u64) (val v off)
  (declare (type (unsigned-byte 64) val)
           (type (simple-array (unsigned-byte 8) (*)) v)
           (type (mod 4611686018427387901) off))
  (setf (%uhs-u32 v off) (logand val #xffffffff)
        (%uhs-u32 v (+ off 4)) (ldb (byte 32 32) val))
  val)

;; XOR-BLOCK eight octets at a time: the word is (unsigned-byte 64) bits.
(defun %uhs-store-words (out words)
  (declare (type (simple-array (unsigned-byte 8) (*)) out)
           (type (simple-array (unsigned-byte 64) (*)) words)
           (optimize speed (safety 0)))
  (dotimes (k (length words))
    (setf (%uhs-u64 out (* 8 k)) (logxor (aref words k) #x0101010101010101)))
  out)

(deftest-compiled-only uhs-store-words-no-box
  (%uhs-sil-count #'%uhs-store-words "BoxU64")
  0)

(deftest uhs-store-words-value
  (let ((words (make-array 2 :element-type '(unsigned-byte 64)
                             :initial-contents '(#xFEDCBA9876543210 #x8000000000000001)))
        (out (make-array 16 :element-type '(unsigned-byte 8))))
    (coerce (%uhs-store-words out words) 'list))
  (#x11 #x33 #x55 #x77 #x99 #xBB #xDD #xFF #x00 #x01 #x01 #x01 #x01 #x01 #x01 #x81))

;; The same binding written directly, reading the halves both natively and
;; generically.
(defun %uhs-halves (w)
  (declare (type (unsigned-byte 64) w))
  (let ((w2 (logxor w 1)))
    (declare (type (unsigned-byte 64) w2))
    (let ((lo (logand w2 #xffffffff))
          (hi (ldb (byte 32 32) w2))
          (mid (ldb (byte 16 24) w2)))
      (declare (type (unsigned-byte 32) lo hi) (type (unsigned-byte 16) mid))
      (list lo hi mid (+ lo 1) (logxor lo hi)))))

(deftest uhs-halves-value
  (list (%uhs-halves #xFEDCBA9876543210)
        (%uhs-halves 0)
        (%uhs-halves #xFFFFFFFFFFFFFFFF))
  ((#x76543211 #xFEDCBA98 #x9876 #x76543212 #x88888889)
   (1 0 0 2 1)
   (#xFFFFFFFE #xFFFFFFFF #xFFFF #xFFFFFFFF 1)))

(deftest-compiled-only uhs-halves-no-box
  (%uhs-sil-count #'%uhs-halves "BoxU64")
  0)

;; A half read from an (unsigned-byte 64) array element.
(defun %uhs-aref-half (v i)
  (declare (type (simple-array (unsigned-byte 64) (*)) v) (fixnum i))
  (let ((hi (ldb (byte 32 32) (aref v i))))
    (declare (type (unsigned-byte 32) hi))
    (+ hi 1)))

(deftest uhs-aref-half-value
  (let ((v (make-array 2 :element-type '(unsigned-byte 64)
                         :initial-contents '(#xFFFFFFFF00000000 #x123456789))))
    (list (%uhs-aref-half v 0) (%uhs-aref-half v 1)))
  (#x100000000 2))
