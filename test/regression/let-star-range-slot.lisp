;;; Regression tests for the raw Int64 slot of a LET* binding whose init has a
;;; proven integer range. The SETF expansion of a place whose (SETF F) is an
;;; INLINE function binds each place argument to a LET* temp, and a temp such as
;;; (* i 4) used to be a boxed local: the product was boxed, then unboxed again
;;; by the substituted body (ironclad's (SETF UB32REF/LE)). The temp is now a
;;; raw slot when its init's range is proven and it is passed on to a parameter
;;; the INLINE definition declares an integer type, or read by native integer
;;; arithmetic.

(setf dotcl:*save-sil* t)

(defun %lsr-sil-has-p (fn text)
  (and (search text (%flat-text (dotcl:function-sil fn))) t))

(declaim (inline %lsr-u16 (setf %lsr-u16) (setf %lsr-u32)))
(defun %lsr-u16 (v off)
  (declare (type (simple-array (unsigned-byte 8) (*)) v)
           (type (mod 4611686018427387901) off))
  (logior (aref v off) (ash (aref v (1+ off)) 8)))
(defun (setf %lsr-u16) (val v off)
  (declare (type (unsigned-byte 16) val)
           (type (simple-array (unsigned-byte 8) (*)) v)
           (type (mod 4611686018427387901) off))
  (setf (aref v off) (ldb (byte 8 0) val)
        (aref v (1+ off)) (ldb (byte 8 8) val))
  val)
(defun (setf %lsr-u32) (val v off)
  (declare (type (unsigned-byte 32) val)
           (type (simple-array (unsigned-byte 8) (*)) v)
           (type (mod 4611686018427387901) off))
  (setf (%lsr-u16 v off) (logand val #xffff)
        (%lsr-u16 v (+ off 2)) (ldb (byte 16 16) val))
  val)

;; ironclad's SALSA output loop: 16 words stored little-endian.
(defun %lsr-out16 (buf x)
  (declare (type (simple-array (unsigned-byte 8) (64)) buf)
           (type (simple-array (unsigned-byte 32) (16)) x)
           (optimize speed))
  (dotimes (i 16)
    (setf (%lsr-u32 buf (* i 4)) (aref x i)))
  buf)

(deftest-compiled-only lsr-setf-temp-not-boxed
  (list (%lsr-sil-has-p #'%lsr-out16 "MultiplyFixnum")
        (%lsr-sil-has-p #'%lsr-out16 "AddFixnum")
        (%lsr-sil-has-p #'%lsr-out16 "UNBOX-FIXNUM"))
  (nil nil nil))

(deftest lsr-out16-value
  (let ((buf (make-array 64 :element-type '(unsigned-byte 8)))
        (x (make-array 16 :element-type '(unsigned-byte 32))))
    (dotimes (i 16) (setf (aref x i) (+ #x01020304 (* i #x10101010))))
    (%lsr-out16 buf x)
    (list (coerce (subseq buf 0 8) 'list) (coerce (subseq buf 60 64) 'list)
          (loop for i below 16 always (= (%lsr-u16 buf (* i 4)) (logand (aref x i) #xffff)))))
  ((4 3 2 1 20 19 18 17) (244 243 242 241) t))

;;; ---- the same slot outside SETF ----

(declaim (inline %lsr-at))
(defun %lsr-at (v i)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (fixnum i))
  (aref v i))

;; A LET* temp passed to an INLINE function's FIXNUM parameter.
(defun %lsr-pass (v i)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (type (integer 0 10) i))
  (let* ((k (* i 3)) (m (+ k 1)))
    (+ (%lsr-at v k) (%lsr-at v m))))

(deftest-compiled-only lsr-pass-not-boxed
  (list (%lsr-sil-has-p #'%lsr-pass "MultiplyFixnum")
        (%lsr-sil-has-p #'%lsr-pass "Fixnum.Make"))
  (nil nil))

(deftest lsr-pass-value
  (let ((v (make-array 40 :element-type '(unsigned-byte 8))))
    (dotimes (j 40) (setf (aref v j) j))
    (list (%lsr-pass v 0) (%lsr-pass v 4) (%lsr-pass v 10)))
  (1 25 61))

;; The raw slot read in a generic position (a list, a non-inline call) boxes the
;; value again, which has to be the same integer.
(defun %lsr-generic (i)
  (declare (type (integer 0 1000) i))
  (let* ((k (* i 7)) (m (+ k 1)))
    (list k m (+ m 0) (identity k))))

(deftest lsr-generic-read
  (%lsr-generic 900)
  (6300 6301 6301 6300))

;; An init that is itself a raw variable is copied raw.
(defun %lsr-copy (v i)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (fixnum i))
  (let* ((j i) (k (+ j 1)))
    (list (%lsr-at v j) (%lsr-at v k))))

(deftest lsr-copy-value
  (let ((v (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(10 11 12 13))))
    (%lsr-copy v 2))
  (12 13))

;;; ---- bindings that must stay boxed ----

;; Assigned in the body: may leave the init's range.
(defun %lsr-mutated (i)
  (declare (type (integer 0 10) i))
  (let* ((k (* i 4)))
    (when (> k 4) (setq k (expt 2 70)))
    (list k (%lsr-inc k))))
(defun %lsr-inc (x) (+ x 1))

(deftest lsr-mutated
  (list (%lsr-mutated 1) (%lsr-mutated 5))
  ((4 5) (1180591620717411303424 1180591620717411303425)))

;; Captured and assigned by a closure.
(defun %lsr-captured (i)
  (declare (type (integer 0 10) i))
  (let* ((k (* i 4)) (f (lambda () (setq k (expt 2 70)))))
    (funcall f)
    (+ k 1)))

(deftest lsr-captured
  (%lsr-captured 3)
  1180591620717411303425)

;; A later init that names a sibling shadowing a declared outer variable sees
;; the sibling, not the outer declaration.
(defun %lsr-shadow (big)
  (let ((n 5))
    (declare (type (integer 0 10) n))
    (let* ((n big) (m (+ n 1)))
      (list n m (%lsr-inc m)))))

(deftest lsr-shadow
  (%lsr-shadow (expt 2 70))
  (1180591620717411303424 1180591620717411303425 1180591620717411303426))

;; Evaluation order of the SETF place arguments and the value is unchanged.
(defvar *lsr-log* nil)
(defun %lsr-note (x) (push x *lsr-log*) x)
(defun %lsr-order (buf i)
  (declare (type (simple-array (unsigned-byte 8) (*)) buf) (type (integer 0 4) i))
  (setf (%lsr-u32 (%lsr-note buf) (* (%lsr-note i) 4)) (%lsr-note #x0a0b0c0d)))

(deftest lsr-order
  (let ((buf (make-array 16 :element-type '(unsigned-byte 8))))
    (setq *lsr-log* nil)
    (list (%lsr-order buf 2) (coerce (subseq buf 8 12) 'list) (cdr (reverse *lsr-log*))))
  (#x0a0b0c0d (13 12 11 10) (2 #x0a0b0c0d)))
