;;; A LET variable bound to (MAKE-ARRAY N :ELEMENT-TYPE '<numeric type>) with a
;;; literal N, whose value never leaves the function (only AREF, (SETF AREF),
;;; LENGTH and REPLACE with another numeric array use it), is held as the bare
;;; .NET array: no vector object is made. Returning it, passing it to a
;;; function, capturing it in a closure or assigning it keeps the vector.

(setf dotcl:*save-sil* t)

(defun %rla-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %rla-raw (fn)
  (- (%rla-count "(NEWARR " fn)
     (%rla-count "(NEWARR Object)" fn)
     (%rla-count "(NEWARR LispObject)" fn)))

(defun %rla-sum (n)
  (declare (fixnum n))
  (let ((x (make-array 8 :element-type '(unsigned-byte 32))))
    (dotimes (i 8) (setf (aref x i) (+ i n)))
    (let ((s 0))
      (dotimes (i (length x) s) (setq s (+ s (aref x i)))))))

(defun %rla-copy (src)
  (declare (type (simple-array (unsigned-byte 32) (*)) src))
  (let ((x (make-array 4 :element-type '(unsigned-byte 32)))
        (out (make-array 4 :element-type '(unsigned-byte 32) :initial-element 9)))
    (replace x src)
    (setf (aref x 0) (logxor (aref x 0) 1))
    (replace out x)
    out))

(defun %rla-double (a)
  (declare (double-float a))
  (let ((x (make-array 3 :element-type 'double-float)))
    (setf (aref x 1) a)
    (+ (aref x 0) (aref x 1) (aref x 2))))

;; A fresh, zeroed array on every iteration.
(defun %rla-in-loop ()
  (let ((r '()))
    (dotimes (k 3 (nreverse r))
      (let ((x (make-array 2 :element-type '(unsigned-byte 8))))
        (push (aref x 1) r)
        (setf (aref x 1) 200)))))

(defun %rla-ref (i) (let ((x (make-array 4 :element-type 'fixnum))) (aref x i)))
(defun %rla-store-u8 (v)
  (declare (fixnum v))
  (let ((x (make-array 2 :element-type '(unsigned-byte 8))))
    (setf (aref x 0) v)
    (aref x 0)))

;; These keep the vector.
(defun %rla-returned ()
  (let ((x (make-array 4 :element-type '(unsigned-byte 32))))
    (setf (aref x 0) 1)
    x))
(declaim (notinline %rla-keep))
(defun %rla-keep (x) (aref x 0))
(defun %rla-passed ()
  (let ((x (make-array 4 :element-type '(unsigned-byte 32))))
    (setf (aref x 0) 2)
    (%rla-keep x)))
(defun %rla-closure ()
  (let ((x (make-array 4 :element-type '(unsigned-byte 32))))
    (setf (aref x 0) 3)
    (funcall (lambda () (aref x 0)))))
(defun %rla-setq (y)
  (let ((x (make-array 4 :element-type '(unsigned-byte 32))))
    (setf (aref x 0) 4)
    (when y (setq x y))
    (aref x 0)))

(deftest raw-local-array.values
  (list (%rla-sum 10)
        (coerce (%rla-copy (make-array 6 :element-type '(unsigned-byte 32)
                                         :initial-contents '(10 20 30 40 50 60)))
                'list)
        (%rla-double 2.5d0)
        (%rla-in-loop)
        (%rla-ref 3)
        (%rla-store-u8 255)
        (aref (%rla-returned) 0)
        (%rla-passed)
        (%rla-closure)
        (%rla-setq nil)
        (%rla-setq (make-array 1 :element-type '(unsigned-byte 32) :initial-element 7)))
  (108 (11 20 30 40) 2.5d0 (0 0 0) 0 255 1 2 3 4 7))

(deftest raw-local-array.errors
  (list (handler-case (%rla-ref 4) (error () :error))
        (handler-case (%rla-ref -1) (error () :error))
        (handler-case (%rla-store-u8 256) (type-error () :type-error))
        (handler-case (%rla-store-u8 -1) (type-error () :type-error)))
  (:error :error :type-error :type-error))

(deftest-emitting-only raw-local-array.sil
  (list (mapcar (lambda (f) (list (%rla-count "MakeArrayOfType" f) (%rla-raw f)))
                (list #'%rla-sum #'%rla-double #'%rla-in-loop #'%rla-ref #'%rla-store-u8))
        (list (%rla-raw #'%rla-copy) (%rla-count "ReplaceKeys" #'%rla-copy))
        (mapcar (lambda (f) (list (%rla-count "MakeArrayOfType" f) (%rla-raw f)))
                (list #'%rla-returned #'%rla-passed #'%rla-closure #'%rla-setq)))
  (((0 1) (0 1) (0 1) (0 1) (0 1))
   (1 0)
   ((1 0) (1 0) (1 0) (1 0))))

(deftest-emitting-only raw-local-array.no-vector-allocated
  (progn
    (dotimes (i 1000) (%rla-sum i))
    (< (min-allocated-bytes (lambda () (dotimes (i 10000) (%rla-sum 1))))
       (* 10000 100)))
  t)
