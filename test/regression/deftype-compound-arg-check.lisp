;;; A declared argument or slot type that is a DEFTYPE with arguments, as in
;;; (deftype octets (&optional n) ...) and (declare (type (octets 64) b)), is
;;; checked from a test built once; the expander is not called on every check.

(deftype %dca-octets (&optional (n '*)) `(simple-array (unsigned-byte 8) (,n)))
(deftype %dca-block () '(%dca-octets 64))
(deftype %dca-range (lo hi) `(integer ,lo ,hi))

(defun %dca-first (b) (declare (type %dca-block b)) (aref b 0))
(defun %dca-direct (b) (declare (type (%dca-octets 16) b)) (aref b 0))
(defun %dca-small (x) (declare (type (%dca-range 0 9) x)) x)

(defstruct %dca-s (buf (make-array 4 :element-type '(unsigned-byte 8)) :type (%dca-octets 4)))

(defun %dca-type-error-p (thunk)
  (handler-case (progn (funcall thunk) nil) (type-error () t)))

(deftest deftype-compound-arg-check.accepts-and-rejects
  (list (%dca-first (make-array 64 :element-type '(unsigned-byte 8) :initial-element 7))
        (%dca-type-error-p (lambda () (%dca-first (make-array 63 :element-type '(unsigned-byte 8)))))
        (%dca-type-error-p (lambda () (%dca-first (make-array 64))))
        (%dca-direct (make-array 16 :element-type '(unsigned-byte 8) :initial-element 3))
        (%dca-type-error-p (lambda () (%dca-direct (make-array 64 :element-type '(unsigned-byte 8)))))
        (%dca-small 9)
        (%dca-type-error-p (lambda () (%dca-small 10)))
        (%dca-type-error-p (lambda () (setf (%dca-s-buf (make-%dca-s)) (make-array 5 :element-type '(unsigned-byte 8)))))
        (%dca-type-error-p (lambda () (setf (%dca-s-buf (make-%dca-s)) (make-array 4 :element-type '(unsigned-byte 8))))))
  (7 t t 3 t 9 t t nil))

(defun %dca-loop (n)
  (declare (fixnum n))
  (let ((b (make-array 64 :element-type '(unsigned-byte 8))) (r 0))
    (dotimes (i n r) (setq r (%dca-first b)))))

(deftest-compiled-only deftype-compound-arg-check.no-consing
  (< (bytes-per-op #'%dca-loop) 8)
  t)
