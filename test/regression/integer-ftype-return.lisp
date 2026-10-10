;;; A call to a function whose FTYPE declares an integer result that fits int64
;;; is an integer operand for the arithmetic around it, not only when that type
;;; is FIXNUM. (UNSIGNED-BYTE 32) is how 32-bit modular helpers are declared
;;; (ironclad's MOD32+ and ROL32), and (UNSIGNED-BYTE 8) how a byte extractor
;;; is; a LOGXOR or an index around such a call used to take the generic path,
;;; boxing on both sides. A declared type wider than int64 is not trusted to
;;; fit a raw int64.

(setf dotcl:*save-sil* t)

(defun %ifr-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %ifr-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(declaim (ftype (function ((unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 32))
                %ifr-add32))
(defun %ifr-add32 (a b)
  (declare (type (unsigned-byte 32) a b))
  (logand (+ a b) #xffffffff))

(declaim (ftype (function (unsigned-byte) (unsigned-byte 8)) %ifr-byte1))
(defun %ifr-byte1 (x)
  (ldb (byte 8 8) x))

(declaim (ftype (function (t) (unsigned-byte 64)) %ifr-wide))
(defun %ifr-wide (x)
  (declare (ignore x))
  (1- (expt 2 64)))

(defun %ifr-xor (a b c)
  (declare (type (unsigned-byte 32) a b c))
  (logxor c (%ifr-add32 a b)))

(defun %ifr-index (v x)
  (declare (type (simple-array (unsigned-byte 32) (*)) v))
  (aref v (+ 256 (%ifr-byte1 x))))

(defun %ifr-wide-xor (x)
  (logxor 1 (%ifr-wide x)))

(defparameter *ifr-v*
  (let ((v (make-array 512 :element-type '(unsigned-byte 32) :initial-element 0)))
    (dotimes (i 512 v) (setf (aref v i) (* i 3)))))

(deftest integer-ftype-return.values
  (list (%ifr-xor #xffffffff 2 #x0f0f0f0f)
        (%ifr-xor 0 0 0)
        (%ifr-index *ifr-v* #x1234)
        (%ifr-wide-xor nil))
  (#x0f0f0f0e 0 #.(* 3 (+ 256 #x12)) #.(- (expt 2 64) 2)))

;; The LOGXOR is done on raw int64s: no generic Logxor2.
(deftest-emitting-only integer-ftype-return.logxor-is-native
  (%ifr-count "Runtime.Logxor2" (%ifr-sil #'%ifr-xor))
  0)

;; The index is added natively, so the element is read with an int64 index.
(deftest-emitting-only integer-ftype-return.index-is-native
  (let ((d (%ifr-sil #'%ifr-index)))
    (list (%ifr-count "Runtime.Add\"" d)
          (%ifr-count "Runtime.Aref\"" d)))
  (0 0))

;; (UNSIGNED-BYTE 64) does not fit an int64: the generic path stays.
(deftest-emitting-only integer-ftype-return.wide-stays-generic
  (%ifr-count "Runtime.Logxor2" (%ifr-sil #'%ifr-wide-xor))
  1)
