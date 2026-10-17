;;; ASH by a variable count whose declared range has both signs, on an operand
;;; whose range keeps every result in int64, compiles to int64 shifts.

(setf dotcl:*save-sil* t)

(defun %asc-has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t))

(declaim (inline %asc-mod32ash))
(defun %asc-mod32ash (num count)
  (declare (type (unsigned-byte 32) num) (type (integer -31 31) count))
  (logand #xffffffff (ash num count)))

(defun %asc-sigma (x)
  (declare (type (unsigned-byte 32) x))
  (logxor (%asc-mod32ash x -3) (%asc-mod32ash x 7) (%asc-mod32ash x 31)))

(defun %asc-ash (x n)
  (declare (type (signed-byte 32) x) (type (integer -63 31) n))
  (ash x n))

(defun %asc-wide (x n)
  (declare (type (signed-byte 40) x) (type (integer -63 63) n))
  (ash x n))

(deftest ash-signed-count.values
  (list (%asc-sigma #xdeadbeef) (%asc-sigma 0) (%asc-sigma #xffffffff)
        (loop for n in '(-63 -40 -31 -1 0 1 5 31)
              collect (list (%asc-ash -123456789 n) (%asc-ash 2147483647 n)
                            (%asc-ash -2147483648 n)))
        (list (%asc-wide (1- (ash 1 39)) 63) (%asc-wide (- (ash 1 39)) 63)
              (%asc-wide -5 -63) (%asc-wide 5 -2)))
  (3440033885 0 1610612863
   ((-1 0 -1) (-1 0 -1) (-1 0 -1) (-61728395 1073741823 -1073741824)
    (-123456789 2147483647 -2147483648) (-246913578 4294967294 -4294967296)
    (-3950617248 68719476704 -68719476736)
    (-265121435612086272 4611686016279904256 -4611686018427387904))
   (5070602400903694233949958045696 -5070602400912917605986812821504 -1 1)))

(deftest-emitting-only ash-signed-count.native
  (list (%asc-has "Runtime.Ash)" #'%asc-sigma) (%asc-has "Runtime.AshLong" #'%asc-ash)
        (%asc-has "Runtime.Ash)" #'%asc-wide))
  (nil t t))
