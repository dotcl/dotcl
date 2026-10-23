;;; An IF whose two arms are both DOUBLE-FLOAT (or both SINGLE-FLOAT) is itself
;;; a float of that format: it is computed raw where a raw value is wanted (a
;;; LET, an argument of float arithmetic, a self tail call into a float
;;; parameter), with no box for the arm's value.

(setf dotcl:*save-sil* t)

(defun %fir-d (a b)
  (declare (double-float a b))
  (let ((m (if (> a b) (* a 2d0) b)))
    (+ m 1d0)))

(defun %fir-s (n)
  (declare (fixnum n))
  (labels ((rec (i x acc)
             (declare (fixnum i) (single-float x acc))
             (if (>= i n)
                 acc
                 (rec (1+ i) (if (> x 1.0f0) 0.0f0 (+ x 0.25f0)) (+ acc x)))))
    (rec 0 0.0f0 0.0f0)))

(defun %fir-nested (a)
  (declare (double-float a))
  (* 2d0 (if (< a 0d0) (- a) (if (> a 10d0) 10d0 a))))

(deftest float-if-raw.values
  (list (%fir-d 3d0 1d0) (%fir-d 1d0 3d0) (%fir-s 10)
        (%fir-nested -2d0) (%fir-nested 20d0) (%fir-nested 4d0))
  (7d0 4d0 5.25f0 4d0 20d0 8d0))

(deftest-compiled-only float-if-raw.no-box
  (flet ((has (needle fn) (and (search needle (%flat-text (dotcl:function-sil fn))) t)))
    (list (has "CheckArgSingle" #'%fir-s) (has "NEWOBJ SingleFloat" #'%fir-s)
          (has "Runtime.Multiply" #'%fir-nested) (has "Runtime.Add" #'%fir-d)))
  (nil nil nil nil))

(setf dotcl:*save-sil* nil)
