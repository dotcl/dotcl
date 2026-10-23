;;; A raw double / single operand pushed before another operand whose code
;;; opens a protected region (HANDLER-CASE, IGNORE-ERRORS, UNWIND-PROTECT):
;;; CIL enters a try block only with an empty evaluation stack.

(defmacro %fot-d (x) `(the double-float (or (ignore-errors (coerce ,x 'double-float)) 0d0)))
(defmacro %fot-s (x) `(the single-float (or (ignore-errors (coerce ,x 'single-float)) 0f0)))

(defun %fot-d-add (x y) (declare (double-float y)) (+ y (%fot-d x)))
(defun %fot-d-nest (x y) (declare (double-float y)) (- 1d0 (/ y (%fot-d x))))
(defun %fot-d-cmp (x y) (declare (double-float y)) (< y (%fot-d x)))
(defun %fot-d-min (x y) (declare (double-float y)) (min y (%fot-d x)))
(defun %fot-d-mixed (x i) (declare (fixnum i)) (* i (%fot-d x)))
(defun %fot-d-store (x y a)
  (declare (double-float y) (type (simple-array double-float (*)) a))
  (setf (aref a 0) (+ y (%fot-d x)))
  (aref a 0))
(defun %fot-d-uwp (x y)
  (declare (double-float y))
  (+ y (the double-float (unwind-protect (float x 1d0) nil))))
(defun %fot-d-let (x y)
  (declare (double-float y))
  (let ((u (let ((a (%fot-d x)) (b y)) (+ a b))))
    (- 1d0 (/ u (let ((w (%fot-d (+ x 1)))) w)))))
(defun %fot-s-add (x y) (declare (single-float y)) (+ y (%fot-s x)))
(defun %fot-s-nest (x y) (declare (single-float y)) (- 1f0 (/ y (%fot-s x))))
(defun %fot-s-cmp (x y) (declare (single-float y)) (>= y (%fot-s x)))

(deftest float-operand-try.double
  (let ((a (make-array 1 :element-type 'double-float)))
    (list (%fot-d-add 3 2d0) (%fot-d-add "x" 2d0) (%fot-d-nest 4 2d0) (%fot-d-cmp 3 2d0)
          (%fot-d-min 3 2d0) (%fot-d-mixed 3 2) (%fot-d-store 3 2d0 a) (%fot-d-uwp 3 2d0)
          (%fot-d-let 3 1d0)))
  (5d0 2d0 0.5d0 t 2d0 6d0 5d0 5d0 0d0))

(deftest float-operand-try.single
  (list (%fot-s-add 3 2f0) (%fot-s-nest 4 2f0) (%fot-s-cmp 3 2f0) (%fot-s-cmp "x" 2f0))
  (5f0 0.5f0 nil t))
