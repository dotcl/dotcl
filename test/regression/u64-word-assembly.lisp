;;; A 64-bit word assembled from two 32-bit halves, the way ironclad's portable
;;; UB64REF/LE reads eight octets: (dpb hi (byte 32 32) lo), with each half an
;;; INLINE call that reads the array through its own parameters. The word is
;;; computed in the 64-bit lane, not with generic LOGIOR / ASH on bignums. The
;;; answers have to be the generic ones: at the boundaries, with the high bit
;;; set, and when a shift does leave 64 bits.

(deftype %u64w-index () '(mod #.array-dimension-limit))
(declaim (inline %u64w-ub16 %u64w-ub32 %u64w-ub64 (setf %u64w-ub64) %u64w-wide))
(defun %u64w-ub16 (v o)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (type %u64w-index o))
  (dpb (aref v (1+ o)) (byte 8 8) (aref v o)))
(defun %u64w-ub32 (v o)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (type %u64w-index o))
  (dpb (%u64w-ub16 v (+ o 2)) (byte 16 16) (%u64w-ub16 v o)))
(defun %u64w-ub64 (v o)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (type %u64w-index o))
  (dpb (%u64w-ub32 v (+ o 4)) (byte 32 32) (%u64w-ub32 v o)))
(defun (setf %u64w-ub64) (value v o)
  (declare (type (unsigned-byte 64) value)
           (type (simple-array (unsigned-byte 8) (*)) v) (type %u64w-index o))
  (dotimes (i 8) (setf (aref v (+ o i)) (ldb (byte 8 (* 8 i)) value)))
  value)
;; Not a word: the high octet shifted past bit 63. Compiled as a value.
(defun %u64w-wide (v o)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (type %u64w-index o))
  (logior (ash (aref v o) 60) (aref v (1+ o))))

(defun %u64w-ref (v o)
  (let ((w 0)) (dotimes (i 8 w) (setf w (+ w (* (aref v (+ o i)) (expt 256 i)))))))

(defun %u64w-read (v o)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (type %u64w-index o)
           (optimize speed (safety 0)))
  (%u64w-ub64 v o))
(defun %u64w-xor (len a b c)
  (declare (type (simple-array (unsigned-byte 8) (*)) a b c) (type %u64w-index len)
           (optimize speed (safety 0)))
  (dotimes (k (floor len 8))
    (let ((o (* 8 k)))
      (setf (%u64w-ub64 c o) (logxor (%u64w-ub64 a o) (%u64w-ub64 b o)))))
  c)
(defun %u64w-sum-wide (v)
  (declare (type (simple-array (unsigned-byte 8) (*)) v))
  (logxor (%u64w-wide v 0) (%u64w-ub64 v 0)))
(defun %u64w-halves (hi lo)
  (declare (type (unsigned-byte 32) hi lo))
  (logior (ash (logand hi #xffffffff) 32) lo))
;; The mask lets 33 bits through: shifted by 32 that is past 2^64.
(defun %u64w-too-wide (x)
  (declare (type (unsigned-byte 40) x))
  (ash (logand x #x1ffffffff) 32))

(defun %u64w-octets (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

(deftest u64-word-assembly.boundaries
  (list (%u64w-read (%u64w-octets 1 2 3 4 5 6 7 8) 0)
        (%u64w-read (%u64w-octets 0 0 0 0 0 0 0 0) 0)
        (%u64w-read (%u64w-octets 255 255 255 255 255 255 255 255) 0)
        (%u64w-read (%u64w-octets 0 0 0 0 0 0 0 128) 0)
        (%u64w-read (%u64w-octets 255 255 255 255 255 255 255 127) 0)
        (%u64w-read (%u64w-octets 0 0 0 0 0 0 0 64) 0)
        (%u64w-read (%u64w-octets 9 1 2 3 4 5 6 7 240) 1))
  (#x0807060504030201 0 #xffffffffffffffff #x8000000000000000 #x7fffffffffffffff
   #x4000000000000000 #xf007060504030201))

(deftest u64-word-assembly.random-xor
  (let ((rs (make-random-state nil)) (bad nil))
    (dotimes (n 200 bad)
      (let* ((len (* 8 (1+ (random 6 rs))))
             (a (make-array len :element-type '(unsigned-byte 8)))
             (b (make-array len :element-type '(unsigned-byte 8)))
             (c (make-array len :element-type '(unsigned-byte 8))))
        (dotimes (i len) (setf (aref a i) (random 256 rs) (aref b i) (random 256 rs)))
        (%u64w-xor len a b c)
        (dotimes (i len)
          (unless (= (aref c i) (logxor (aref a i) (aref b i))) (push (list n i) bad)))
        (dotimes (k (floor len 8))
          (unless (= (%u64w-read a (* 8 k)) (%u64w-ref a (* 8 k))) (push (list n :read k) bad))))))
  nil)

(deftest u64-word-assembly.shifts
  (list (%u64w-halves #xffffffff #xffffffff)
        (%u64w-halves #x80000000 0)
        (%u64w-halves 0 #xffffffff)
        (%u64w-too-wide #x1ffffffff)
        (%u64w-too-wide #xffffffff)
        (%u64w-sum-wide (%u64w-octets 255 1 0 0 0 0 0 0)))
  (#xffffffffffffffff #x8000000000000000 #xffffffff #x1ffffffff00000000
   #xffffffff00000000 #xff0000000000001fe))

;;; The word and the XOR of two words stay in the lane: no generic LOGIOR, ASH
;;; or LOGXOR in the code.
(setf dotcl:*save-sil* t)
(defun %u64w-sil (fn) (%flat-text (dotcl:function-sil fn)))
(defun %u64w-xor-sil (len a b c)
  (declare (type (simple-array (unsigned-byte 8) (*)) a b c) (type %u64w-index len)
           (optimize speed (safety 0)))
  (dotimes (k (floor len 8))
    (let ((o (* 8 k)))
      (setf (%u64w-ub64 c o) (logxor (%u64w-ub64 a o) (%u64w-ub64 b o)))))
  c)
(defun %u64w-read-sil (v o)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (type %u64w-index o))
  (%u64w-ub64 v o))
(deftest-emitting-only u64-word-assembly.stays-in-lane
  (loop for fn in (list #'%u64w-xor-sil #'%u64w-read-sil)
        collect (let ((s (%u64w-sil fn)))
                  (list (search "Logior2" s) (search "Logxor2" s) (search "Runtime.Ash" s))))
  ((nil nil nil) (nil nil nil)))
(setf dotcl:*save-sil* nil)
