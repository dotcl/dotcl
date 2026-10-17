;;; A parameter declared (SINGLE-FLOAT LO HI) / (DOUBLE-FLOAT LO HI) is checked
;;; without the general type test.

(setf dotcl:*save-sil* t)

(defun %frc-s (x) (declare (type (single-float 0f0 1f0) x)) x)
(defun %frc-s* (x) (declare (type (single-float 0f0 *) x)) x)
(defun %frc-d (x) (declare (type (double-float -1d0 1d0) x)) x)

(defun %frc-try (f x)
  (handler-case (funcall f x)
    (type-error () :type-error)))

(deftest float-range-arg-check.single
  (mapcar (lambda (x) (%frc-try #'%frc-s x)) (list 0f0 0.5f0 1f0 1.5f0 -0.5f0 0.5d0 1))
  (0.0 0.5 1.0
   :type-error
   :type-error
   :type-error
   :type-error))

(deftest float-range-arg-check.open-upper
  (list (%frc-s* 0f0) (%frc-s* most-positive-single-float)
        (%frc-try #'%frc-s* -1f0))
  (0.0 #.most-positive-single-float :type-error))

(deftest float-range-arg-check.double
  (mapcar (lambda (x) (%frc-try #'%frc-d x)) (list -1d0 0.25d0 1d0 2d0 0.5f0))
  (-1d0 0.25d0 1d0
   :type-error
   :type-error))

(deftest-compiled-only float-range-arg-check.direct
  (let ((s (%flat-text (dotcl:function-sil #'%frc-s)))
        (d (%flat-text (dotcl:function-sil #'%frc-d))))
    (list (and (search "CheckArgSingleRange" s) t)
          (and (search "CheckArgDoubleRange" d) t)
          (search "CheckArgType" s)))
  (t t nil))

(setf dotcl:*save-sil* nil)
