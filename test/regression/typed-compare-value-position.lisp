;;; A two-argument < > <= >= = of two fixnum-typed (or two float-typed)
;;; operands, used as a value rather than as a test, compares the raw values.
;;; It used to box both operands again and call the generic entry, which made
;;; (lambda (a b) (declare (fixnum a b)) (< a b)) as a SORT predicate slower
;;; than #'<. The value is T or NIL, as a single value.

(setf dotcl:*save-sil* t)

(defun %tcv-lt (a b) (declare (fixnum a b)) (< a b))
(defun %tcv-gt (a b) (declare (fixnum a b)) (> a b))
(defun %tcv-le (a b) (declare (fixnum a b)) (<= a b))
(defun %tcv-ge (a b) (declare (fixnum a b)) (>= a b))
(defun %tcv-eq (a b) (declare (fixnum a b)) (= a b))
(defun %tcv-dlt (a b) (declare (double-float a b)) (< a b))
(defun %tcv-mixed-lt (a b) (declare (single-float a) (double-float b)) (< a b))
(defun %tcv-mixed-ge (a b) (declare (single-float a) (double-float b)) (>= a b))
(defun %tcv-mixed-eq (a b) (declare (double-float a) (single-float b)) (= a b))

(deftest-emitting-only typed-compare-value-position.no-generic-entry
  (loop for f in '(%tcv-lt %tcv-gt %tcv-le %tcv-ge %tcv-eq %tcv-dlt %tcv-mixed-lt)
        for s = (%flat-text (dotcl:function-sil (symbol-function f)))
        collect (or (search "Runtime.LessThan" s) (search "Runtime.GreaterThan" s)
                    (search "Runtime.LessEqual" s) (search "Runtime.GreaterEqual" s)
                    (search "Runtime.NumEqual" s) (search "Fixnum.Make" s)))
  (nil nil nil nil nil nil nil))

(deftest typed-compare-value-position.fixnum
  (let ((pairs (list (list 1 2) (list 2 1) (list 3 3)
                     (list most-negative-fixnum most-positive-fixnum)
                     (list most-positive-fixnum most-negative-fixnum)
                     (list -5 -5) (list 0 -1))))
    (loop for (a b) in pairs
          collect (list (%tcv-lt a b) (%tcv-gt a b) (%tcv-le a b)
                        (%tcv-ge a b) (%tcv-eq a b))))
  ((t nil t nil nil) (nil t nil t nil) (nil nil t t t)
   (t nil t nil nil) (nil t nil t nil) (nil nil t t t) (nil t nil t nil)))

(deftest typed-compare-value-position.float
  (list (%tcv-dlt 1d0 2d0) (%tcv-dlt 2d0 1d0) (%tcv-dlt -0d0 0d0)
        ;; 0.1f0 widened exactly is a little above 0.1d0.
        (%tcv-mixed-lt 0.1f0 0.1d0) (%tcv-mixed-ge 0.1f0 0.1d0)
        (%tcv-mixed-lt 1.5f0 1.5d0) (%tcv-mixed-ge 1.5f0 1.5d0)
        (%tcv-mixed-eq 1.5d0 1.5f0) (%tcv-mixed-eq 0.1d0 0.1f0))
  (t nil nil nil t nil t t nil))

(deftest typed-compare-value-position.single-value
  (list (multiple-value-list (%tcv-lt 1 2))
        (multiple-value-list (%tcv-lt 2 1))
        (multiple-value-list (%tcv-dlt 1d0 2d0))
        (multiple-value-list (funcall (lambda (a b) (declare (fixnum a b)) (>= a b)) 4 4)))
  ((t) (nil) (t) (t)))

(deftest typed-compare-value-position.sort-predicate
  (let ((l (loop for i below 200 collect (mod (* i 7919) 211))))
    (equal (sort (copy-list l) (lambda (a b) (declare (fixnum a b)) (< a b)))
           (sort (copy-list l) #'<)))
  t)
