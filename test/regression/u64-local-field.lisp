;;; (LDB (BYTE S P) X) of a local holding (UNSIGNED-BYTE 64) bits, with a field
;;; below 2^63, is a fixnum computed from the raw bits: a LET bound to it holds
;;; an int64, and no box is made for it.

(defstruct (ulf (:constructor make-ulf (state)))
  (state 0 :type (unsigned-byte 64)))

(declaim (inline %ulf-mix))
(defun %ulf-mix (data)
  (declare (type (unsigned-byte 37) data))
  (ldb (byte 32 0) (logxor data (ash data -18))))

(defun %ulf-fields (u)
  (let* ((state (ulf-state u))
         (a (ldb (byte 37 27) state))
         (b (ldb (byte 5 59) state))
         (c (ldb (byte 63 0) state))
         (d (logand state #xff)))
    (list (+ (%ulf-mix a) b) c d (ldb (byte 1 63) state))))

(defun %ulf-ref (x)
  (list (+ (ldb (byte 32 0) (logxor (ldb (byte 37 27) x) (ash (ldb (byte 37 27) x) -18)))
           (ldb (byte 5 59) x))
        (ldb (byte 63 0) x) (logand x #xff) (ldb (byte 1 63) x)))

(defun %ulf-loop (u n)
  (let ((s 0))
    (declare (fixnum s))
    (dotimes (i n s)
      (let* ((state (ulf-state u))
             (a (ldb (byte 37 27) state)))
        (setf s (logand (+ s (%ulf-mix a)) #xffff))))))

(deftest u64-local-field.values
  (loop for x in (list 0 1 #x7fffffffffffffff #x8000000000000000 #xffffffffffffffff
                       #xfedcba9876543210 #x0123456789abcdef)
        unless (equal (%ulf-fields (make-ulf x)) (%ulf-ref x))
          collect x)
  nil)

(deftest-emitting-only u64-local-field.no-allocation
  (let ((u (make-ulf #xfedcba9876543210)))
    (%ulf-loop u 100)
    (< (min-allocated-bytes (lambda () (%ulf-loop u 100000))) 10000))
  t)
