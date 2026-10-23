;;; A (simple-array single-float ...) parameter read only through a PROG / LET
;;; variable bound to it (the Gabriel fft's (prog ((ar areal)) ...)) gets its
;;; element buffer hoisted, and the alias reads it directly.

(setf dotcl:*save-sil* t)

(defun %fal-scale (areal n)
  (declare (type (simple-array single-float (*)) areal) (fixnum n))
  (prog ((ar areal))
     (do ((i 0 (1+ i))) ((>= i n)) (setf (aref ar i) (* 2.0 (aref ar i))))
     (return ar)))

(deftest float-array-let-alias-hoist.values
  (let ((v (make-array 4 :element-type 'single-float :initial-contents '(1.0 2.0 3.0 4.0))))
    (coerce (%fal-scale v 4) 'list))
  (2.0 4.0 6.0 8.0))

(deftest float-array-let-alias-hoist.wrong-element-type
  (let ((v (make-array 3 :element-type 'double-float :initial-contents '(1d0 2d0 3d0))))
    (handler-case (%fal-scale v 3)
      (type-error () :type-error)))
  :type-error)

(deftest-emitting-only float-array-let-alias-hoist.typed
  (let ((d (%flat-text (dotcl:function-sil #'%fal-scale))))
    (and (search "LDELEM-R4" d) (search "STELEM-R4" d) t))
  t)
