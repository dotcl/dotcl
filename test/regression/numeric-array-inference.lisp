;;; Regression tests for numeric-array LOCAL INFERENCE: a let binding whose
;;; init is (make-array ... :element-type 'BOUNDED-INTEGER) is proven
;;; numeric-backed, so aref on it with fixnum-typed indices reads/writes the
;;; element as a raw int64 (Runtime.ArefNum*L) and contributes the storage
;;; range to the arithmetic range prover. Contract: results identical to the
;;; boxed path, loud range errors, graceful fallback when the array's backing
;;; changes at runtime (adjust-array :displaced-to), and NO inference for
;;; mutated locals.

(setf dotcl:*save-sil* t)

;; The cl-bench 2d-arrays inner-loop shape, &optional included.
(defun %nai2-bench-2d (&optional (size 8) (runs 2))
  (declare (fixnum size))
  (let ((ones (make-array (list size size) :element-type '(integer 0 1000) :initial-element 1))
        (twos (make-array (list size size) :element-type '(integer 0 1000) :initial-element 2))
        (threes (make-array (list size size) :element-type '(integer 0 2000))))
    (dotimes (runs runs)
      (dotimes (i size)
        (dotimes (j size)
          (setf (aref threes i j)
                (+ (aref ones i j) (aref twos i j))))))
    (aref threes 3 3)))

(deftest nai2-bench-2d-result
  (%nai2-bench-2d)
  3)

;; The whole inner loop must be on the raw-long entries: no boxed Aref2D,
;; no generic Runtime.Add.
(deftest nai2-bench-2d-sil
  (let ((sil (princ-to-string (dotcl:function-sil #'%nai2-bench-2d))))
    (list (notnot (search "ArefNum2DL" sil))
          (notnot (search "ArefSetNum2DL" sil))
          (notnot (search "Runtime.Add" sil))))
  (t t nil))

;; 1D and 3D shapes.
(defun %nai2-1d (n)
  (declare (fixnum n))
  (let ((v (make-array n :element-type '(unsigned-byte 8) :initial-element 3))
        (s 0))
    (dotimes (i n)
      (setf (aref v i) (+ (aref v i) 1))
      (setq s (+ s (aref v i))))
    s))

(deftest nai2-1d
  (%nai2-1d 10)
  40)

(defun %nai2-3d (n)
  (declare (fixnum n))
  (let ((a (make-array (list n n n) :element-type 'fixnum)))
    (dotimes (i n)
      (dotimes (j n)
        (dotimes (k n)
          (setf (aref a i j k) (+ (* i 100) (* j 10) k)))))
    (aref a 2 1 3)))

(deftest nai2-3d
  (%nai2-3d 4)
  213)

;; Range violation through the inferred raw-long store still signals.
(deftest nai2-range-error
  (let ((v (make-array 4 :element-type '(integer 0 2000) :initial-element 0)))
    (handler-case
        (progn (dotimes (i 4) (setf (aref v i) 70000)) :no-error)
      (error () :err)))
  :err)

;; adjust-array to displaced mid-flight: the raw-long entries must fall back
;; to the boxed path and stay correct.
(defun %nai2-displace-flip (flip)
  (let ((base (make-array 8 :element-type '(unsigned-byte 16) :initial-element 5))
        (v (make-array 4 :element-type '(unsigned-byte 16) :initial-element 1 :adjustable t))
        (s 0))
    (when flip
      (adjust-array v 4 :displaced-to base :displaced-index-offset 2))
    (dotimes (i 4)
      (setq s (+ s (aref v i))))
    s))

(deftest nai2-displaced-fallback
  (list (%nai2-displace-flip nil) (%nai2-displace-flip t))
  (4 20))

;; A mutated (setq'd) array local is NOT inferred — and stays correct.
(defun %nai2-mutated (n)
  (declare (fixnum n))
  (let ((v (make-array n :element-type '(unsigned-byte 8) :initial-element 1)))
    (setq v (make-array n :initial-element 10))
    (let ((s 0))
      (dotimes (i n) (setq s (+ s (aref v i))))
      s)))

(deftest nai2-mutated-not-inferred
  (%nai2-mutated 5)
  50)

(setf dotcl:*save-sil* nil)
