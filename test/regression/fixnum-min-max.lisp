;;; MIN / MAX of exact fixnums compare the raw int64 values; anything else keeps
;;; the generic entry.

(setf dotcl:*save-sil* t)

(defun %fmm-clamp (x lo hi) (declare (fixnum x lo hi)) (max (min x hi) lo))
(defun %fmm-nary (x y) (declare (fixnum x y)) (list (min x y 3) (max x y -3)))
(defun %fmm-mixed (x y) (declare (fixnum x) (single-float y)) (list (min x 3) (max x 1.5f0) (min y 2)))
(defun %fmm-range (x)
  (declare (type (integer -5 5) x))
  (let ((m (max x 0))) (* m (min x 100))))

(deftest fixnum-min-max.values
  (list (%fmm-clamp 5 0 3) (%fmm-clamp -2 0 3) (%fmm-clamp 2 0 3)
        (%fmm-clamp most-positive-fixnum 0 most-positive-fixnum)
        (%fmm-clamp most-negative-fixnum most-negative-fixnum 0)
        (%fmm-nary 7 -9) (%fmm-mixed 1 2f0) (%fmm-mixed 5 1f0) (%fmm-range 3) (%fmm-range -3))
  (3 0 2 #.most-positive-fixnum #.most-negative-fixnum (-9 7) (1 1.5 2.0) (3 5 1.0) 9 0))

(deftest-compiled-only fixnum-min-max.raw
  (let ((c (%flat-text (dotcl:function-sil #'%fmm-clamp)))
        (n (%flat-text (dotcl:function-sil #'%fmm-nary)))
        (r (%flat-text (dotcl:function-sil #'%fmm-range))))
    (loop for s in (list c n r)
          collect (or (search "Runtime.Min" s) (search "Runtime.Max" s))))
  (nil nil nil))

(setf dotcl:*save-sil* nil)
