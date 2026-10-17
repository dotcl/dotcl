;;; A local function's parameter, or a LET inside it, that has the name of an
;;; outer variable is a different variable: references to it do not make the
;;; outer one captured or assigned.

(setf dotcl:*save-sil* t)

(defun %cas-param (a)
  (declare (single-float a))
  (let ((x (* a 2f0)))
    (flet ((f (x) (setq x (* x 3)) x))
      (list (+ x 1f0) (f 2)))))

(defun %cas-let (a)
  (declare (single-float a))
  (let ((x (* a 2f0)))
    (flet ((f (z) (let ((x z)) (setq x (1+ x)) x)))
      (list (+ x 1f0) (f 2)))))

(defun %cas-lambda (a)
  (declare (single-float a))
  (let ((x (* a 2f0)))
    (list (+ x 1f0) (funcall (lambda (x) (incf x) x) 5))))

(defun %cas-captured (a)
  (let ((x a))
    (flet ((f () (incf x)))
      (f) (f)
      (list x (let ((x 10)) (incf x) x) x))))

(defun %cas-captured-nested (a)
  (let ((x a))
    (flet ((f (y) (let ((z y)) (lambda () (incf x z)))))
      (let ((g (f 3)))
        (funcall g) (funcall g)
        x))))

(deftest capture-analysis-shadowing.values
  (list (%cas-param 1.5f0) (%cas-let 1.5f0) (%cas-lambda 1.5f0)
        (%cas-captured 1) (%cas-captured-nested 1))
  ((4.0 6) (4.0 3) (4.0 6) (3 11 3) 7))

(deftest-compiled-only capture-analysis-shadowing.raw
  (mapcar (lambda (f) (search "Runtime.Add" (%flat-text (dotcl:function-sil f))))
          (list #'%cas-param #'%cas-let #'%cas-lambda))
  (nil nil nil))

(setf dotcl:*save-sil* nil)
