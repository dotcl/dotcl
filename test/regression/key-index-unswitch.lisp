;;; An untyped &key or &optional parameter used in a subscript's arithmetic
;;; inside a loop, (AREF B (+ (* 128 START) I)), gets a copy of the body in
;;; which it is a (SIGNED-BYTE 32), taken when the argument is one. The other
;;; copy keeps the generic arithmetic for any other value.

(defun %kiu-xor (b1 b2 &key (start1 0) (start2 0))
  (declare (type (simple-array (unsigned-byte 64) (*)) b1 b2))
  (dotimes (i 4)
    (setf (aref b1 (+ (* 128 start1) i))
          (logxor (aref b1 (+ (* 128 start1) i))
                  (aref b2 (+ (* 128 start2) i)))))
  (values))

(defun %kiu-u64 (n)
  (let ((v (make-array n :element-type '(unsigned-byte 64))))
    (dotimes (i n v) (setf (aref v i) i))))

(deftest key-index-unswitch.values
  (let ((a (%kiu-u64 384)) (b (%kiu-u64 384)))
    (%kiu-xor a b :start2 2)
    (%kiu-xor a b :start1 1)
    (%kiu-xor a b :start1 2 :start2 1)
    (mapcar (lambda (k) (coerce (subseq a k (+ k 5)) 'list)) '(0 128 256)))
  ((256 256 256 256 4) (128 128 128 128 132) (384 384 384 384 260)))

(defun %kiu-sum (v &optional (s 0))
  (let ((acc 0))
    (dotimes (i 3 acc)
      (incf acc (aref v (+ (- s 5000000000) i))))))

(deftest key-index-unswitch.outside-the-range
  (let ((v (vector 10 20 30 40 50)))
    (list (%kiu-sum v 5000000001)
          (%kiu-sum v 5000000002)
          (handler-case (%kiu-sum v 1) (error () :error))))
  (90 120 :error))

(defun %kiu-assigned (v &key (s 0))
  (setq s (- s 4000000000))
  (let ((acc 0))
    (dotimes (i 2 acc)
      (incf acc (aref v (+ (* 1 s) i))))))

(deftest key-index-unswitch.assigned-parameter
  (%kiu-assigned (vector 1 2 3 4) :s 4000000002)
  7)

(defun %kiu-bytes () (nth 4 (dotcl:gc-stats)))

(deftest-compiled-only key-index-unswitch.no-boxing
  (let ((a (make-array 140000 :element-type '(unsigned-byte 64)))
        (b (make-array 140000 :element-type '(unsigned-byte 64))))
    (%kiu-xor a b :start1 1000 :start2 1001)
    (let ((b0 (%kiu-bytes)))
      (dotimes (k 1000) (%kiu-xor a b :start1 1000 :start2 1001))
      (< (/ (- (%kiu-bytes) b0) 1000) 200)))
  t)
