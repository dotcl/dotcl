;;; A constant right shift of an (unsigned-byte 64) value is the CIL shift itself.
;;; Before, it was a call of a helper that the JIT stopped inlining in a very large
;;; method (a table-driven round function has hundreds of these shifts). The values
;;; must be the same as the generic arithmetic gives: high bit set, the shifts 1,
;;; 8, 56 and 63, and a byte taken from the shifted word.

(defun u64cs-ref (v n)
  (ldb (byte 8 n) v))

(declaim (ftype (function ((unsigned-byte 64)) (unsigned-byte 64)) u64cs-shr1 u64cs-shr63))
(defun u64cs-shr1 (v) (declare (type (unsigned-byte 64) v) (optimize (speed 3) (safety 0)))
  (ash v -1))
(defun u64cs-shr63 (v) (declare (type (unsigned-byte 64) v) (optimize (speed 3) (safety 0)))
  (ash v -63))

(defun u64cs-bytes (a)
  "The eight bytes of the word A[0], most significant first, through constant shifts and a table."
  (declare (type (simple-array (unsigned-byte 64) (2)) a) (optimize (speed 3) (safety 0)))
  (list (logand (ash (aref a 0) -56) #xff) (logand (ash (aref a 0) -48) #xff)
        (logand (ash (aref a 0) -40) #xff) (logand (ash (aref a 0) -32) #xff)
        (logand (ash (aref a 0) -24) #xff) (logand (ash (aref a 0) -16) #xff)
        (logand (ash (aref a 0) -8) #xff) (logand (aref a 0) #xff)))

(deftest u64-constant-shift.single-shifts
  (let ((w #xF123456789ABCDEF))
    (list (u64cs-shr1 w) (u64cs-shr63 w) (u64cs-shr63 #x7FFFFFFFFFFFFFFF) (u64cs-shr1 0)
          (u64cs-shr1 #xFFFFFFFFFFFFFFFF)))
  (#x7891A2B3C4D5E6F7 #x1 0 0 #x7FFFFFFFFFFFFFFF))

(deftest u64-constant-shift.bytes-of-a-word
  (let ((a (make-array 2 :element-type '(unsigned-byte 64)
                         :initial-contents '(#xF123456789ABCDEF #x0102030405060708))))
    (list (u64cs-bytes a)
          (loop for n from 56 downto 0 by 8 collect (u64cs-ref (aref a 0) n))))
  ((#xF1 #x23 #x45 #x67 #x89 #xAB #xCD #xEF) (#xF1 #x23 #x45 #x67 #x89 #xAB #xCD #xEF)))
