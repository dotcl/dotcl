;;; Regression tests for the raw-long-index aref/(setf aref) path: when every
;;; index expression is statically fixnum-typed, the compiler lowers indices
;;; as raw int64 and calls the Runtime.*L variants. The fast path only covers
;;; plain (non-displaced, non-bit) LispVectors; everything else must re-box
;;; and defer to the generic path with identical behavior — these tests pin
;;; that equivalence, plus bounds behavior and evaluation order.

;;; ---- basic reads/writes with fixnum-typed indices ----

(defun %nai-fill-and-sum (n)
  (declare (fixnum n))
  (let ((v (make-array n :initial-element 0))
        (s 0))
    (dotimes (i n)
      (setf (aref v i) (* i 2)))
    (dotimes (i n)
      (setq s (+ s (aref v i))))
    s))

(deftest nai-1d-fill-sum
  (%nai-fill-and-sum 100)
  9900)

(defun %nai-2d (n)
  (declare (fixnum n))
  (let ((a (make-array (list n n) :initial-element 1)))
    (dotimes (i n)
      (dotimes (j n)
        (setf (aref a i j) (+ (aref a i j) (* i j)))))
    (aref a 3 4)))

(deftest nai-2d
  (%nai-2d 6)
  13)

(defun %nai-3d (n)
  (declare (fixnum n))
  (let ((a (make-array (list n n n) :initial-element 0)))
    (dotimes (i n)
      (dotimes (j n)
        (dotimes (k n)
          (setf (aref a i j k) (+ (* 100 i) (* 10 j) k)))))
    (aref a 2 3 1)))

(deftest nai-3d
  (%nai-3d 5)
  231)

;; svref with a typed index. (Compare as a list — EQUAL doesn't descend vectors.)
(deftest nai-svref
  (let ((v (vector 10 20 30)))
    (dotimes (i 3)
      (setf (svref v i) (+ (svref v i) 1)))
    (coerce v 'list))
  (11 21 31))

;; Literal indices are fixnum-typed too.
(deftest nai-literal-index
  (let ((v (make-array 5 :initial-element 0)))
    (setf (aref v 2) 'x)
    (list (aref v 2) (aref v 0)))
  (x 0))

;; setf returns the stored value.
(deftest nai-setf-value
  (let ((v (make-array 3)))
    (declare (ignorable v))
    (let ((i 1))
      (declare (fixnum i))
      (setf (aref v i) 'stored)))
  stored)

;;; ---- fallback equivalence: exotic arrays with typed indices ----

;; Displaced array: fast path must reject and defer — reads/writes go
;; through to the underlying array.
(deftest nai-displaced
  (let* ((base (make-array 10 :initial-element 0))
         (d (make-array 5 :displaced-to base :displaced-index-offset 2)))
    (dotimes (i 5)
      (setf (aref d i) (+ i 100)))
    (list (aref base 2) (aref base 6) (aref d 4)))
  (100 104 104))

;; Bit vector (bit-packed storage).
(deftest nai-bit-vector
  (let ((b (make-array 8 :element-type 'bit :initial-element 0)))
    (dotimes (i 8)
      (when (evenp i) (setf (aref b i) 1)))
    b)
  #*10101010)

;; String via aref.
(deftest nai-string
  (let ((s (copy-seq "hello")))
    (dotimes (i 5)
      (setf (aref s i) (char-upcase (aref s i))))
    s)
  "HELLO")

;; Fill-pointer vector: aref addresses the full storage.
(deftest nai-fill-pointer
  (let ((v (make-array 5 :fill-pointer 2 :initial-element 'a)))
    (let ((i 4))
      (declare (fixnum i))
      (setf (aref v i) 'z)
      (aref v i)))
  z)

;;; ---- bounds errors survive the native path ----

(deftest nai-read-out-of-range
  (let ((v (make-array 3 :initial-element 0)))
    (handler-case
        (progn (let ((i 5)) (declare (fixnum i)) (aref v i)) :no-error)
      (error () :err)))
  :err)

(deftest nai-write-out-of-range
  (let ((v (make-array 3 :initial-element 0)))
    (handler-case
        (progn (let ((i -1)) (declare (fixnum i)) (setf (aref v i) 9)) :no-error)
      (error () :err)))
  :err)

;;; ---- evaluation order: array, indices, then value, left to right ----

(deftest nai-eval-order
  (let ((order '())
        (v (make-array 4 :initial-element 0)))
    (flet ((note (tag val) (push tag order) val))
      (setf (aref (note :arr v) (the fixnum (note :idx 1)))
            (note :val 7)))
    (list (nreverse order) (aref v 1)))
  ((:arr :idx :val) 7))
