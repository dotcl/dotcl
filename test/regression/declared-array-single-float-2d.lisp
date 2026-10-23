;;; AREF on a parameter declared (simple-array single-float (* *)) fetches the
;;; float32 backing once and reads the element unboxed, like the other element
;;; types of the rank-2 path. No other test reached the single-float helper of
;;; that path, so a broken name there would only have shown up in a program.

(setf dotcl:*save-sil* t)

(defun %dsf-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %dsf-2d (arr i j)
  (declare (type (simple-array single-float (* *)) arr) (fixnum i j)
           (optimize (speed 3) (safety 0) (debug 0)))
  (aref arr i j))

(defun %dsf-sum (arr)
  (declare (type (simple-array single-float (* *)) arr)
           (optimize (speed 3) (safety 1) (debug 0)))
  (let ((s 0f0))
    (declare (single-float s))
    (dotimes (i (array-dimension arr 0) s)
      (dotimes (j (array-dimension arr 1))
        (incf s (aref arr i j))))))

(defparameter *dsf-a*
  (make-array '(2 3) :element-type 'single-float
                     :initial-contents '((1.5f0 2.0f0 3.0f0) (4.0f0 5.25f0 6.0f0))))

(deftest declared-array-single-float-2d.values
  (list (%dsf-2d *dsf-a* 0 0) (%dsf-2d *dsf-a* 1 1) (%dsf-sum *dsf-a*))
  (1.5f0 5.25f0 21.75f0))

(deftest-emitting-only declared-array-single-float-2d.uses-the-float32-backing
  (%dsf-count "Runtime.BackingNDF32" (%flat-text (dotcl:function-sil #'%dsf-sum)))
  1)
