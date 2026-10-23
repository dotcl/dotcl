;;; An FLET function declared INLINE gets a literal number argument as that
;;; constant in its substituted body, its value is taken by its bits in the
;;; 64-bit lane, and a LET variable bound to an (unsigned-byte 64) value past
;;; fixnum range gets the bits slot without a declaration. ironclad's SHA-512
;;; round has this shape (its RHO, with literal rotation counts).

(setf dotcl:*save-sil* t)

(declaim (inline %fsi-mod64+ %fsi-rol64 %fsi-ror64))
(defun %fsi-mod64+ (a b)
  (declare (type (unsigned-byte 64) a b))
  (ldb (byte 64 0) (+ a b)))
(defun %fsi-rol64 (a s)
  (declare (type (unsigned-byte 64) a) (type (integer 0 64) s))
  (logior (ldb (byte 64 0) (ash a s)) (ash a (- s 64))))
(defun %fsi-ror64 (a s)
  (declare (type (unsigned-byte 64) a) (type (integer 0 64) s))
  (%fsi-rol64 a (- 64 s)))

(defun %fsi-round (v)
  (declare (type (simple-array (unsigned-byte 64) (4)) v))
  (let ((a (aref v 0)) (b (aref v 1)))
    (flet ((rho (x r1 r2 r3)
             (logxor (%fsi-ror64 x r1) (%fsi-ror64 x r2) (%fsi-ror64 x r3))))
      (declare (inline rho))
      (let ((x (%fsi-mod64+ (rho a 14 18 41) b)))
        (setf (aref v 2) (%fsi-mod64+ x (rho b 28 34 39))
              (aref v 3) (%fsi-mod64+ x 0))))
    v))

(defun %fsi-reference (a b)
  (flet ((ror (x s) (ldb (byte 64 0) (logior (ash x (- s)) (ash x (- 64 s)))))
         (m+ (p q) (ldb (byte 64 0) (+ p q))))
    (let ((x (m+ (logxor (ror a 14) (ror a 18) (ror a 41)) b)))
      (list (m+ x (logxor (ror b 28) (ror b 34) (ror b 39))) x))))

(defun %fsi-run (a b)
  (let ((v (make-array 4 :element-type '(unsigned-byte 64) :initial-contents (list a b 0 0))))
    (%fsi-round v)
    (list (aref v 2) (aref v 3))))

(deftest flet-inline-u64-const.values
  (loop for (a b) in '((0 0) (1 2) (#xFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFF)
                       (#x6A09E667F3BCC908 #xBB67AE8584CAA73B)
                       (#x8000000000000000 #x7FFFFFFFFFFFFFFF))
        always (equal (%fsi-run a b) (%fsi-reference a b)))
  t)

;; A parameter the body assigns stays a variable; one passed a non-literal too.
(defun %fsi-assigned (n)
  (flet ((f (k step) (setq k (+ k step)) (* k 2)))
    (list (f n 3) (f 10 n))))

(deftest flet-inline-u64-const.assigned-param
  (%fsi-assigned 4)
  (14 28))

;; NOTINLINE keeps the call, and #'F still names a function.
(defun %fsi-notinline (n)
  (flet ((f (k) (+ k 1)))
    (declare (notinline f))
    (list (f n) (mapcar #'f '(1 2)))))

(deftest flet-inline-u64-const.notinline
  (%fsi-notinline 5)
  (6 (2 3)))

(defun %fsi-sil (fn) (%flat-text (dotcl:function-sil fn)))

;; No closure call for RHO, and nothing boxed in the round: no BoxU64 before
;; the stores, no Low64 taking a boxed word apart.
(deftest-emitting-only flet-inline-u64-const.no-call-no-box
  (let ((s (%fsi-sil #'%fsi-round)))
    (list (search "LispFunction.Invoke" s)
          (search "Runtime.Low64" s)
          (search "Fixnum.Make" s)))
  (nil nil nil))
