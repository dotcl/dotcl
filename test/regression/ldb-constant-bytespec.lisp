;;; (ldb (byte SIZE POS) X) with literal SIZE and POS is compiled as a shift and
;;; a mask instead of a call to LDB. These check that the open-coded form gives
;;; the same value as the function for every kind of integer, and that it still
;;; rejects a non-integer.

(defun %ldbc-inline (x)
  (list (ldb (byte 8 0) x)
        (ldb (byte 8 8) x)
        (ldb (byte 32 0) x)
        (ldb (byte 32 16) x)
        (ldb (byte 0 5) x)
        (ldb (byte 64 0) x)
        (ldb (byte 100 3) x)))

(defun %ldbc-generic (x)
  (let ((f #'ldb))
    (list (funcall f (byte 8 0) x)
          (funcall f (byte 8 8) x)
          (funcall f (byte 32 0) x)
          (funcall f (byte 32 16) x)
          (funcall f (byte 0 5) x)
          (funcall f (byte 64 0) x)
          (funcall f (byte 100 3) x))))

(deftest ldb-constant-bytespec.matches-function
  (loop for x in (list 0 1 -1 255 256 #x12345678 #xDEADBEEF -12345
                       most-positive-fixnum most-negative-fixnum
                       (expt 2 70) (- (expt 3 50)) #x1FFFFFFFFFFFFFFFF)
        unless (equal (%ldbc-inline x) (%ldbc-generic x))
          collect x)
  nil)

;; The shape ironclad's MOD32+ uses: declared (unsigned-byte 32) operands.
(defun %ldbc-mod32+ (a b)
  (declare (type (unsigned-byte 32) a b))
  (ldb (byte 32 0) (+ a b)))

(deftest ldb-constant-bytespec.mod32+
  (list (%ldbc-mod32+ #xFFFFFFFF 1)
        (%ldbc-mod32+ #xFFFFFFFF #xFFFFFFFF)
        (%ldbc-mod32+ 3 4))
  (0 #xFFFFFFFE 7))

(deftest ldb-constant-bytespec.not-an-integer
  (handler-case (progn (ldb (byte 8 0) (the t (identity 1.5))) :no-error)
    (type-error () :type-error))
  :type-error)
