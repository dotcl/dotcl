;;; A constant-count ASH (and the LDB / MASK-FIELD it is used to open-code) on a
;;; declared integer took a raw int64 path with two holes:
;;;  - CIL SHR takes the count mod 64, so (ash x -64) returned x.
;;;  - (* x y) / (+ x y) of (signed-byte 64) operands counted as fixnum-typed and
;;;    was computed with a wrapping int64 multiply / add first.
;;; (ldb (byte 64 64) (* x y)) -- the portable multiply-high -- returned the low
;;; half of the product. Expected values are SBCL's.

(defun %afr-s1 (x) (declare (type (signed-byte 64) x)) (ash x -64))
(defun %afr-s2 (x) (declare (type (signed-byte 64) x)) (ash x -100))
(defun %afr-s3 (x y) (declare (type (signed-byte 64) x y)) (ash (* x y) -1))
(defun %afr-t1 (x y) (declare (type (signed-byte 64) x y)) (ash (+ x y) -1))
(defun %afr-m1 (x y) (declare (type (signed-byte 64) x y)) (ldb (byte 64 64) (* x y)))
(defun %afr-m2 (x y) (declare (type (signed-byte 64) x y)) (ldb (byte 8 60) (+ x y)))
(defun %afr-small (x y) (declare (type (signed-byte 32) x y)) (ash (* x y) -3))
(defun %afr-the (x y) (declare (type (signed-byte 64) x y)) (ash (the fixnum (+ x y)) -1))

(deftest ash-fast-path-range.count-64-and-above
  (list (%afr-s1 -5) (%afr-s1 5) (%afr-s2 -1) (%afr-s2 most-positive-fixnum))
  (-1 0 -1 0))

(deftest ash-fast-path-range.no-wrap-before-shift
  (list (%afr-s3 (expt 2 40) (expt 2 40))
        (%afr-t1 (expt 2 62) (expt 2 62))
        (%afr-m1 1234567890123456789 4611686018427387904)
        (%afr-m2 (expt 2 62) (expt 2 62)))
  (604462909807314587353088 4611686018427387904 308641972530864197 8))

(deftest ash-fast-path-range.proven-range-still-exact
  (list (%afr-small -3 5) (%afr-small (1- (expt 2 31)) (1- (expt 2 31)))
        (%afr-the 6 8))
  (-2 576460751766552576 7))

;;; The proven cases keep the raw shift: no generic ASH call in the code.
(setf dotcl:*save-sil* t)
(defun %afr-sil (fn) (princ-to-string (dotcl:function-sil fn)))
(defun %afr-small-sil (x y) (declare (type (signed-byte 32) x y)) (ash (* x y) -3))
(defun %afr-leaf-sil (x) (declare (type (signed-byte 64) x)) (ldb (byte 32 32) x))
(deftest-emitting-only ash-fast-path-range.proven-stays-native
  (list (and (search "(SHR)" (%afr-sil #'%afr-small-sil)) t)
        (and (search "(SHR)" (%afr-sil #'%afr-leaf-sil)) t))
  (t t))
(setf dotcl:*save-sil* nil)
