;;; A closure does not capture a local function every call of which it
;;; substitutes, and the function object is then not made. When the closure
;;; still needs the object (#'F, a call that is not substituted), it captures it.

(setf dotcl:*save-sil* t)

(defun %cio-free (a l)
  (declare (single-float a))
  (flet ((sq (x) (declare (single-float x)) (* x a)))
    (declare (inline sq))
    (mapcar (lambda (e) (sq e)) l)))

(defun %cio-only (l)
  (flet ((sq (x) (declare (single-float x)) (* x x)))
    (declare (inline sq))
    (flet ((acc (e) (sq e)))
      (mapcar #'acc l))))

(defun %cio-object (l)
  (flet ((sq (x) (* x x)))
    (declare (inline sq))
    (mapcar (lambda (e) (list (sq e) (funcall #'sq (1+ e)))) l)))

(defun %cio-shadowed (a l)
  (flet ((sq (x) (* x a)))
    (declare (inline sq))
    (mapcar (lambda (a) (sq a)) l)))

(defun %cio-labels (l)
  (labels ((f (x) (* x 2))
           (g (x) (+ (f x) 1)))
    (declare (inline f g))
    (mapcar (lambda (e) (g e)) l)))

(deftest closure-inline-only-capture.values
  (list (%cio-free 2f0 '(1f0 2f0)) (%cio-only '(1f0 3f0)) (%cio-object '(2 3))
        (%cio-shadowed 10 '(1 2)) (%cio-labels '(1 2)))
  ((2.0 4.0) (1.0 9.0) ((4 9) (9 16)) (10 20) (3 5)))

(deftest-compiled-only closure-inline-only-capture.no-object
  (mapcar (lambda (f) (search "MAKE-FUNCTION" (%flat-text (dotcl:function-sil f))))
          (list #'%cio-free #'%cio-only #'%cio-labels))
  (nil nil nil))

(setf dotcl:*save-sil* nil)
