;;; Single-float values kept raw: undeclared LET / LET* variables bound to a
;;; single-float computation, integer * single-float, (FLOAT i 1f0), and the
;;; value of a substituted local function.

(setf dotcl:*save-sil* t)

(defun %sfl-cubic (x p0 p1 p2 p3)
  (declare (type single-float x p0 p1 p2 p3))
  (let* ((step1 (* x (+ (* 3f0 (- p1 p2)) p3 (- p0))))
         (step2 (* x (+ (* 2f0 p0) (* -5f0 p1) (* 4f0 p2) (- p3) step1)))
         (step3 (* x (+ p2 (- p0) step2))))
    (+ p1 (* 0.5f0 step3))))

(deftest single-float-let-inference.let*-chain
  (%sfl-cubic 0.25f0 1f0 2f0 4f0 8f0)
  2.3828125)

(defun %sfl-sum (a n)
  (declare (type (simple-array single-float (*)) a) (fixnum n))
  (flet ((cubic (x p0 p1 p2 p3)
           (declare (type single-float x p0 p1 p2 p3))
           (let* ((step1 (* x (+ (* 3f0 (- p1 p2)) p3 (- p0))))
                  (step2 (* x (+ (* 2f0 p0) (* -5f0 p1) (* 4f0 p2) (- p3) step1))))
             (+ p1 (* 0.5f0 step2)))))
    (declare (inline cubic))
    (let ((s 0f0))
      (declare (single-float s))
      (dotimes (i n)
        (let ((v (cubic 0.5f0 (aref a i) (aref a (+ i 1)) (aref a (+ i 2)) (aref a (+ i 3)))))
          (setf s (+ s v))))
      s)))

(deftest single-float-let-inference.inline-call-value
  (%sfl-sum (make-array 6 :element-type 'single-float
                          :initial-contents '(1f0 2f0 4f0 8f0 16f0 32f0))
            3)
  14.875)

(defun %sfl-mixed (x w px)
  (declare (type (single-float 0f0 1f0) x) (fixnum w) (type (unsigned-byte 8) px))
  (let ((sx (- (* x w) 0.5f0))
        (p (float (the (unsigned-byte 8) px) 1f0)))
    (list sx p (+ sx p))))

(deftest single-float-let-inference.mixed-and-float
  (%sfl-mixed 0.5f0 7 200)
  (3.0 200.0 203.0))

;;; An assigned variable is not given a single-float slot.
(deftest single-float-let-inference.assigned
  (let ((a (* (identity 1.5f0) 2f0)))
    (setq a (list a))
    a)
  (3.0))

;;; An inner binding of a name declared FIXNUM outside is not that fixnum.
(defun %sfl-shadow (n)
  (declare (fixnum n))
  (flet ((g (d)
           (let* ((n d) (r (* n 2f0))) r)))
    (declare (inline g))
    (list (* n 2f0) (g (identity 0.5d0)))))

(deftest single-float-let-inference.shadowed-fixnum
  (mapcar #'type-of (%sfl-shadow 3))
  (single-float double-float))

(defun %sfl-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (1+ q))) (return k))))))

;;; Only the value returned is boxed.
(deftest-compiled-only single-float-let-inference.no-boxes
  (let ((s (%flat-text (dotcl:function-sil #'%sfl-sum))))
    (+ (%sfl-count "NEWOBJ \"SingleFloat\"" s) (%sfl-count "SingleFloat.Box" s)))
  1)

(setf dotcl:*save-sil* nil)
