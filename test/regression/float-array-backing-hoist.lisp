;;; A local declared (SIMPLE-ARRAY DOUBLE-FLOAT (*)) or (SIMPLE-ARRAY
;;; SINGLE-FLOAT (*)) has its double[] / float[] fetched once where it is bound,
;;; and the AREFs in its body become bare element loads and stores, as the
;;; integer kinds already did. The float kinds called the per-element helper on
;;; every access, which re-derived the buffer each time: a double sum ran at
;;; about three times the cost of the same sum over fixnums.
;;;
;;; What the tests pin: the shape (the fetch, no per-element helper in the fast
;;; copy), the values (single elements widen exactly, stores narrow, the SETF
;;; value), that a non-simple array under the declaration is still a TYPE-ERROR
;;; above (safety 0) and still reads correctly at (safety 0), and that a vector
;;; of another element type is still read through the per-element helper.

(setf dotcl:*save-sil* t)

(defun %fab-sil (fn) (%flat-text (dotcl:function-sil fn)))
(defun %fab-has (needle fn) (and (search needle (%fab-sil fn)) t))

(defun %fab-dsum (v)
  (declare (type (simple-array double-float (*)) v))
  (let ((acc 0d0))
    (declare (double-float acc))
    (dotimes (i (length v) acc)
      (setq acc (+ acc (aref v i))))))

(defun %fab-fsum (v)
  (declare (type (simple-array single-float (*)) v))
  (let ((acc 0.0))
    (declare (single-float acc))
    (dotimes (i (length v) acc)
      (setq acc (+ acc (aref v i))))))

(defun %fab-dscale (v k)
  (declare (type (simple-array double-float (*)) v) (double-float k))
  (dotimes (i (length v) v)
    (setf (aref v i) (* (aref v i) k))))

(defun %fab-fscale (v k)
  (declare (type (simple-array single-float (*)) v) (single-float k))
  (dotimes (i (length v) v)
    (setf (aref v i) (* (aref v i) k))))

(defun %fab-set-value (v x)
  (declare (type (simple-array double-float (*)) v) (double-float x))
  (list (setf (aref v 0) x) (aref v 0)))

(defun %fab-dsum-unsafe (v)
  (declare (type (simple-array double-float (*)) v) (optimize (safety 0)))
  (let ((acc 0d0))
    (declare (double-float acc))
    (dotimes (i (length v) acc)
      (setq acc (+ acc (aref v i))))))

(defun %fab-dv (&rest xs)
  (make-array (length xs) :element-type 'double-float :initial-contents xs))

(defun %fab-fv (&rest xs)
  (make-array (length xs) :element-type 'single-float :initial-contents xs))

(deftest-emitting-only float-array-backing-hoist.shape
  (list (%fab-has "BackingF64" #'%fab-dsum)
        (%fab-has "BackingF32" #'%fab-fsum)
        (%fab-has "BackingF64" #'%fab-dscale)
        (%fab-has "BackingF64Checked" #'%fab-dsum)
        (%fab-has "BackingF64Checked" #'%fab-dsum-unsafe))
  (t t t t nil))

(deftest-emitting-only float-array-backing-hoist.no-allocation
  (let ((v (make-array 1000 :element-type 'double-float :initial-element 1.5d0))
        (f (make-array 1000 :element-type 'single-float :initial-element 0.5)))
    (%fab-dsum v) (%fab-fsum f) (%fab-dscale v 1d0) (%fab-fscale f 1.0)
    (< (min-allocated-bytes
        (lambda ()
          (dotimes (r 100)
            (%fab-dsum v) (%fab-fsum f) (%fab-dscale v 1d0) (%fab-fscale f 1.0))))
       20000))
  t)

(deftest float-array-backing-hoist.values
  (list (%fab-dsum (%fab-dv 1d0 2.5d0 -0.5d0))
        (%fab-fsum (%fab-fv 1.0 2.5 0.25))
        (coerce (%fab-dscale (%fab-dv 1d0 2d0 3d0) 0.5d0) 'list)
        (coerce (%fab-fscale (%fab-fv 1.0 2.0 3.0) 0.5) 'list)
        (%fab-set-value (%fab-dv 0d0) 7.25d0)
        (%fab-dsum (%fab-dv))
        (let ((x (%fab-fscale (%fab-fv 1.1) 1.0)))
          (list (aref x 0) (type-of (aref x 0))))
        (handler-case (progn (%fab-dsum-unsafe (%fab-dv 1d0 2d0)))
          (error () :error)))
  (3d0 3.75 (0.5d0 1d0 1.5d0) (0.5 1.0 1.5) (7.25d0 7.25d0) 0d0
   (1.1 single-float) 3d0))

(deftest float-array-backing-hoist.non-simple-is-a-type-error
  (let ((adj (make-array 3 :element-type 'double-float :adjustable t
                           :initial-contents '(1d0 2d0 3d0))))
    (list (handler-case (progn (%fab-dsum adj) :no-error)
            (type-error () :type-error))
          (%fab-dsum-unsafe adj)))
  (:type-error 6d0))

;; A vector of another element type under the declaration is a TYPE-ERROR.
(deftest float-array-backing-hoist.other-element-type-reads-through-the-helper
  (list (handler-case (%fab-dsum (%fab-fv 1.5)) (type-error () :type-error))
        (handler-case (%fab-dsum (vector 1d0 2d0)) (type-error () :type-error))
        (handler-case (progn (%fab-dsum '(1d0)) :no-error)
          (type-error () :type-error)))
  (:type-error :type-error :type-error))

(deftest float-array-backing-hoist.subscript-out-of-range
  (handler-case (progn (%fab-set-value (%fab-dv) 1d0) :no-error)
    (error () :error))
  :error)
