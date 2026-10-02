;;; ASH on declared integers, the shapes 32-bit rotates and hashes are written in:
;;; a left shift whose result the range proof keeps in int64, a left shift under
;;; LDB / LOGAND (only the low bits are wanted), and a shift by a variable count
;;; with a declared range. These take a raw int64 path; the values must be the
;;; ones generic arithmetic gives. Expected values are SBCL's.

(defun %mas-rol (a k)
  (declare (type (unsigned-byte 32) a) (type (integer 0 31) k))
  (logior (ldb (byte 32 0) (ash a k)) (ash a (- k 32))))
(defun %mas-rol-all (a)
  (loop for k from 0 below 32 collect (%mas-rol a k)))
(defun %mas-shr (x k)
  (declare (type (signed-byte 40) x) (type (integer -63 0) k))
  (ash x k))
(defun %mas-low-wide (x k)
  (declare (type (unsigned-byte 32) x) (type (integer 0 63) k))
  (ldb (byte 32 0) (ash x k)))
(defun %mas-exact-left (a)
  (declare (type (unsigned-byte 32) a))
  (ash a 7))
(defun %mas-sum-left (a b)
  (declare (type (unsigned-byte 32) a b))
  (ldb (byte 32 0) (ash (ldb (byte 32 0) (+ a b)) 13)))
(defun %mas-neg-left (x)
  (declare (type (signed-byte 32) x))
  (logand (ash x 20) #xFFFFFFFFFF))
(defun %mas-const-wrap ()
  (let ((z 5)) (declare (type (integer 0 7) z))
    (list (ldb (byte 8 0) (ash z 62)) (ldb (byte 8 56) (ash z 62)) (logand (ash 5 62) #xFF))))
(defun %mas-array (x k)
  (declare (type (simple-array (unsigned-byte 32) (4)) x) (type (integer 0 31) k))
  (dotimes (i 3)
    (setf (aref x (1+ i)) (logior (ldb (byte 32 0) (ash (aref x i) k)) (ash (aref x i) (- k 32)))))
  (coerce x 'list))

(defun %mas-rol-reference (a)
  (loop for k from 0 below 32
        collect (+ (mod (* a (expt 2 k)) (expt 2 32))
                   (floor a (expt 2 (- 32 k))))))

(deftest modular-ash.rotate-variable-count
  (list (equal (%mas-rol-all #x80000001) (%mas-rol-reference #x80000001))
        (equal (%mas-rol-all #xDEADBEEF) (%mas-rol-reference #xDEADBEEF))
        (nth 7 (%mas-rol-all #xDEADBEEF)))
  (t t 1457485807))

(deftest modular-ash.right-shift-variable-count
  (list (%mas-shr -5 -1) (%mas-shr -5 0) (%mas-shr -5 -63)
        (%mas-shr (1- (expt 2 39)) -38) (%mas-shr (- (expt 2 39)) -39)
        (%mas-shr 12345 -3))
  (-3 -5 -1 1 -1 1543))

(deftest modular-ash.low-bits-of-wide-shift
  (list (%mas-low-wide #xFFFFFFFF 0) (%mas-low-wide #xFFFFFFFF 31)
        (%mas-low-wide #xFFFFFFFF 32) (%mas-low-wide #x12345678 40)
        (%mas-low-wide #xFFFFFFFF 63))
  (4294967295 2147483648 0 0 0))

(deftest modular-ash.exact-left-shift
  (list (%mas-exact-left #xFFFFFFFF) (%mas-exact-left 1)
        (%mas-sum-left #xFFFFFFFF #xFFFFFFFF) (%mas-sum-left #x80000000 #x80000000))
  (549755813760 128 4294950912 0))

(deftest modular-ash.negative-operand-under-mask
  (list (%mas-neg-left -1) (%mas-neg-left -12345) (%mas-neg-left 7))
  (1099510579200 1086566957056 7340032))

(deftest modular-ash.constant-past-int64-under-mask
  (%mas-const-wrap)
  (0 64 0))

(deftest modular-ash.rotate-in-typed-array
  (%mas-array (make-array 4 :element-type '(unsigned-byte 32)
                            :initial-contents '(#x9E3779B9 0 0 0))
              5)
  (2654435769 3337566003 3722897016 3168587547))
