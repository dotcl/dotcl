;;; (ASH X 0), one-argument LOGIOR / LOGXOR / LOGAND and a LOGAND with a 2^n-1 mask
;;; around a value already inside the mask are the value itself, and compile as the
;;; raw value when X is statically a fixnum (BABEL's UB-GET is written this way).
;;; Values and errors are what the general operations give.

(defun %tio-octet (v i)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (fixnum i))
  (logand 255 (logior (ash (aref v i) 0))))

(defun %tio-octet-sum (v i)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (fixnum i))
  (let ((o (logand 255 (logior (ash (aref v i) 0)))))
    (declare (type (unsigned-byte 8) o))
    (+ o (logand 255 (logior (ash (aref v (1+ i)) 0))))))

(defun %tio-ash0 (x) (declare (fixnum x)) (ash x 0))
(defun %tio-ash0-any (x) (ash x 0))
(defun %tio-logior1 (x) (declare (fixnum x)) (logior x))
(defun %tio-logand-mask (x) (declare (type (integer 0 15) x)) (logand 15 x))
(defun %tio-logand-mask-narrow (x) (declare (type (integer 0 300) x)) (logand 255 x))
(defun %tio-between (v i end)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (fixnum i end))
  (and (< i end) (< #x7f (logand 255 (aref v i)) #xc0)))
(defun %tio-chain (a b c) (declare (fixnum a b c)) (list (< a b c) (<= a b c) (> a b c) (< 5 b 10) (< a 5 c)))

(deftest trivial-integer-ops-typed.values
  (let ((v (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(0 127 200 255))))
    (list (mapcar (lambda (i) (%tio-octet v i)) '(0 1 2 3))
          (%tio-octet-sum v 1)
          (%tio-ash0 -5) (%tio-ash0 most-positive-fixnum)
          (%tio-ash0-any (expt 10 30))
          (%tio-logior1 -7)
          (list (%tio-logand-mask 9) (%tio-logand-mask-narrow 300) (%tio-logand-mask-narrow 255))
          (list (%tio-between v 1 4) (%tio-between v 2 4) (%tio-between v 0 4) (%tio-between v 2 2) (%tio-between (make-array 1 :element-type '(unsigned-byte 8) :initial-element 130) 0 1))
          (%tio-chain 1 2 3) (%tio-chain 3 2 1) (%tio-chain 1 7 20)))
  ((0 127 200 255) 327 -5 9223372036854775807 1000000000000000000000000000000 -7
   (9 44 255) (nil nil nil nil t)
   (t t nil nil nil) (nil nil t nil nil) (t t nil t t)))

(deftest trivial-integer-ops-typed.errors
  (list (handler-case (%tio-ash0-any "x") (type-error () :type-error))
        (handler-case (%tio-ash0-any 1.5) (type-error () :type-error)))
  (:type-error :type-error))
