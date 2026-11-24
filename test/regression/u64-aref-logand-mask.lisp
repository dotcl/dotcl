;;; (logand MASK (aref A I)) on an (unsigned-byte 64) array with a small non-negative MASK is
;;; computed on the element's bits, with no boxed integer; the value is the same for elements
;;; at or above 2^63.

(defun %ulm-sum-low8 (n a)
  (declare (fixnum n) (type (simple-array (unsigned-byte 64) (4)) a))
  (let ((s 0)) (declare (fixnum s))
    (dotimes (i n) (incf s (logand 255 (aref a (logand i 3)))))
    s))

(defun %ulm-sum-low63 (a)
  (declare (type (simple-array (unsigned-byte 64) (4)) a))
  (let ((s 0)) (declare (fixnum s))
    (dotimes (i 4) (setq s (logxor s (logand (aref a i) #x7FFFFFFFFFFFFFFF))))
    s))

(declaim (type (simple-array (unsigned-byte 64) (4)) *ulm-g*))
(defvar *ulm-g* (make-array 4 :element-type '(unsigned-byte 64)
                              :initial-contents (list #xFFFFFFFFFFFFFFFF #x8000000000000001 7 #x123456789ABCDEF0)))

(defun %ulm-global (n)
  (declare (fixnum n))
  (let ((s 0)) (declare (fixnum s))
    (dotimes (i n) (incf s (logand #xFFFF (aref *ulm-g* (logand i 3)))))
    s))

(deftest u64-aref-logand-mask.low-byte
  (let ((a (make-array 4 :element-type '(unsigned-byte 64)
                         :initial-contents (list #xFFFFFFFFFFFFFFFF #x8000000000000001 7 #x123456789ABCDEF0))))
    (list (%ulm-sum-low8 4 a) (%ulm-sum-low8 8 a)))
  (#.(+ 255 1 7 #xF0) #.(* 2 (+ 255 1 7 #xF0))))

(deftest u64-aref-logand-mask.low-63-bits
  (let ((a (make-array 4 :element-type '(unsigned-byte 64)
                         :initial-contents (list #xFFFFFFFFFFFFFFFF #x8000000000000001 7 #x123456789ABCDEF0))))
    (%ulm-sum-low63 a))
  #.(logxor #x7FFFFFFFFFFFFFFF 1 7 #x123456789ABCDEF0))

(deftest u64-aref-logand-mask.global-array
  (list (%ulm-global 4) (%ulm-global 8))
  (#.(+ #xFFFF 1 7 #xDEF0) #.(* 2 (+ #xFFFF 1 7 #xDEF0))))
