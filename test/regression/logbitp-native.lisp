;;; LOGBITP with a literal index and LOGTEST with a literal mask, on fixnums, compile to native
;;; arithmetic; the answers are those of the generic functions (including for negative numbers).

(defun %lbn-generic-logbitp (k x) (declare (notinline logbitp)) (logbitp k x))
(defun %lbn-generic-logtest (m x) (declare (notinline logtest)) (logtest m x))

(defun %lbn-bit7 (x) (declare (fixnum x)) (logbitp 7 x))
(defun %lbn-bit0 (x) (declare (fixnum x)) (logbitp 0 x))
(defun %lbn-bit61 (x) (declare (fixnum x)) (logbitp 61 x))
(defun %lbn-mask (x) (declare (fixnum x)) (logtest #x1234 x))
(defun %lbn-mask-first (x) (declare (fixnum x)) (logtest x #x80))
(defun %lbn-value (x) (declare (fixnum x)) (list (logbitp 3 x) (logtest 8 x)))
(defun %lbn-in-test (x) (declare (fixnum x)) (if (logbitp 7 x) :set :clear))

(defvar cl-user::*lbn-samples*
  (list 0 1 -1 127 128 129 255 256 -128 -129 #x1234 #x1235 (- #x1234)
        (ash 1 61) (1- (ash 1 61)) (- (ash 1 61)) (1- (ash 1 62)) (- (ash 1 62))
        most-positive-fixnum most-negative-fixnum))

(deftest logbitp-native.matches-generic
  (loop for x in cl-user::*lbn-samples*
        always (and (eq (%lbn-bit7 x) (%lbn-generic-logbitp 7 x))
                    (eq (%lbn-bit0 x) (%lbn-generic-logbitp 0 x))
                    (eq (%lbn-bit61 x) (%lbn-generic-logbitp 61 x))
                    (eq (%lbn-mask x) (%lbn-generic-logtest #x1234 x))
                    (eq (%lbn-mask-first x) (%lbn-generic-logtest x #x80))
                    (equal (%lbn-value x) (list (%lbn-generic-logbitp 3 x) (%lbn-generic-logtest 8 x)))
                    (eq (%lbn-in-test x) (if (%lbn-generic-logbitp 7 x) :set :clear))))
  t)

(deftest logbitp-native.values-are-t-or-nil
  (list (%lbn-bit7 128) (%lbn-bit7 127) (%lbn-mask #x1234) (%lbn-mask 1))
  (t nil t nil))

(deftest logbitp-native.non-fixnum-and-other-indexes
  (list (logbitp 7 (ash 1 70)) (logbitp 70 (ash 1 70)) (logbitp 100 -1) (logbitp 62 most-positive-fixnum)
        (let ((k 7)) (logbitp k 128)) (logtest 0 5) (logtest #x80 (ash 1 70)))
  (nil t t t t nil nil))

(defun %lbn-loop (v)
  (declare (type (simple-array (unsigned-byte 8) (*)) v))
  (let ((k 0))
    (declare (fixnum k))
    (dotimes (i (length v)) (when (logbitp 7 (aref v i)) (incf k)))
    k))

(deftest-compiled-only logbitp-native.no-call-per-byte
  (let ((v (make-array 200000 :element-type '(unsigned-byte 8))))
    (dotimes (i 200000) (setf (aref v i) (mod (* i 37) 256)))
    (%lbn-loop v)
    (list (= (%lbn-loop v) (count-if (lambda (b) (>= b 128)) v))
          (< (min-allocated-bytes (lambda () (%lbn-loop v))) 100000)))
  (t t))
