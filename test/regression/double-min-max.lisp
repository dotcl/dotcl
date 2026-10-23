;;; MIN and MAX of two DOUBLE-FLOAT values are computed on raw doubles, with the
;;; same choice of operand as the generic MIN / MAX (on a tie the first for MAX
;;; and for MIN, so (max -0d0 0d0) is -0d0).

(setf dotcl:*save-sil* t)

(defun %dmm-min (a b) (declare (double-float a b)) (min a b))
(defun %dmm-max (a b) (declare (double-float a b)) (max a b))
(defun %dmm-clip (lo hi x)
  (declare (double-float lo hi x))
  (let ((m (max lo (min hi x)))) (+ m 1d0)))
(defun %dmm-let (a b)
  (declare (double-float a b))
  (let ((u (min (* a 2d0) b))) (* u u)))

(defun %dmm-generic (op a b) (funcall op a b))

(deftest double-min-max.values
  (let ((pairs (list (cons 1d0 2d0) (cons 2d0 1d0) (cons -3.5d0 -3.5d0)
                     (cons -0d0 0d0) (cons 0d0 -0d0) (cons 1d300 -1d300))))
    (list (every (lambda (p) (eql (%dmm-min (car p) (cdr p)) (%dmm-generic #'min (car p) (cdr p)))) pairs)
          (every (lambda (p) (eql (%dmm-max (car p) (cdr p)) (%dmm-generic #'max (car p) (cdr p)))) pairs)
          (%dmm-max -0d0 0d0) (%dmm-max 0d0 -0d0) (%dmm-min -0d0 0d0) (%dmm-min 0d0 -0d0)
          (%dmm-clip 0d0 1d0 5d0) (%dmm-clip 0d0 1d0 -5d0) (%dmm-clip 0d0 1d0 0.25d0)
          (%dmm-let 1d0 5d0) (%dmm-let 3d0 5d0)))
  (t t -0d0 0d0 -0d0 0d0 2d0 1d0 1.25d0 4d0 25d0))

(deftest double-min-max.type-error
  (list (handler-case (%dmm-min 1 2d0) (type-error () :type-error))
        (handler-case (%dmm-max 1d0 nil) (type-error () :type-error)))
  (:type-error :type-error))

(deftest-compiled-only double-min-max.raw
  (flet ((has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t)))
    (list (has "Runtime.Max)" #'%dmm-max) (has "Runtime.Min)" #'%dmm-min)
          (has "Runtime.Max)" #'%dmm-clip) (has "Runtime.Min)" #'%dmm-let)
          (has "MaxDouble" #'%dmm-clip)))
  (nil nil nil nil t))

(setf dotcl:*save-sil* nil)
