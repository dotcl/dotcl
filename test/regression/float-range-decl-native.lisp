;;; A variable declared (SINGLE-FLOAT LO HI) / (DOUBLE-FLOAT LO HI) is computed
;;; with the raw float operations, as one declared SINGLE-FLOAT / DOUBLE-FLOAT is.

(setf dotcl:*save-sil* t)

(defun %frd-s (x w)
  (declare (type (single-float 0f0 1f0) x) (type (unsigned-byte 32) w))
  (- (* x w) 0.5f0))
(defun %frd-d (x)
  (declare (type (double-float -1d0 1d0) x))
  (+ (* x 2d0) 1d0))
(defun %frd-let (a)
  (let ((y (* a 0.5f0)))
    (declare (type (single-float 0f0) y))
    (+ y y)))

(deftest float-range-decl-native.values
  (list (%frd-s 0.5f0 10) (%frd-s 0f0 3) (%frd-d -0.5d0) (%frd-d 1d0) (%frd-let 3f0)
        (handler-case (%frd-s 2f0 1) (type-error () :type-error)))
  (4.5 -0.5 0d0 3d0 3.0 :type-error))

(deftest-compiled-only float-range-decl-native.let-check
  (handler-case (%frd-let 1d0) (type-error () :type-error))
  :type-error)

(deftest-compiled-only float-range-decl-native.raw
  (let ((s (%flat-text (dotcl:function-sil #'%frd-s)))
        (d (%flat-text (dotcl:function-sil #'%frd-d))))
    (list (search "Runtime.Multiply" s) (search "Runtime.Subtract" s)
          (search "Runtime.Multiply" d) (search "Runtime.Add" d)))
  (nil nil nil nil))

(setf dotcl:*save-sil* nil)
