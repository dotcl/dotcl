;;; An unassigned LET / LET* variable bound to a double-float or single-float
;;; value is that type: the arithmetic on it stays raw. The value may come
;;; through (VALUES X), (PROGN X), (THE (VALUES TYPE &OPTIONAL) X),
;;; (LOCALLY (DECLARE (OPTIMIZE ..)) X) and the value of an inner LET, which is
;;; the shape Coalton generates for every arithmetic operation.

(setf dotcl:*save-sil* t)

(defun %lfp-has (needle fn)
  (and (search needle (%flat-text (dotcl:function-sil fn))) t))

(defun %lfp-generic-p (fn)
  (or (%lfp-has "Runtime.Multiply" fn) (%lfp-has "Runtime.Add" fn)
      (%lfp-has "Runtime.Subtract" fn) (%lfp-has "Runtime.UnwrapMv" fn)))

(defun %lfp-d1 (zr)
  (declare (double-float zr))
  (let ((a zr) (b zr)) (* a b)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %lfp-op (op x y type)
    `(locally (declare (optimize (safety 0)))
       (the (values ,type &optional)
         (let ((a ,x) (b ,y))
           (declare (ignorable a b))
           (values (progn (,op a b))))))))

(defun %lfp-d2 (zr zi cr)
  (declare (double-float zr zi cr))
  #.`(let ((p ,(%lfp-op '* 'zr 'zr 'double-float)))
       (declare (ignorable p))
       (let ((q ,(%lfp-op '* 'zi 'zi 'double-float)))
         ,(%lfp-op '+ (%lfp-op '- 'p 'q 'double-float) 'cr 'double-float))))

(defun %lfp-d3 (zr)
  (declare (double-float zr))
  (let* ((a (let ((x zr) (y 2d0)) (* x y)))
         (b (- a 1d0)))
    (+ a b)))

(defun %lfp-s1 (x)
  (declare (single-float x))
  #.`(let ((p ,(%lfp-op '* 'x 'x 'single-float)))
       ,(%lfp-op '+ 'p 1f0 'single-float)))

(deftest let-float-type-propagation.values
  (list (%lfp-d1 3d0) (%lfp-d2 1.5d0 0.5d0 0.25d0) (%lfp-d3 2d0) (%lfp-s1 3f0))
  (9d0 2.25d0 7d0 10f0))

(deftest-compiled-only let-float-type-propagation.raw
  (mapcar #'%lfp-generic-p (list #'%lfp-d1 #'%lfp-d2 #'%lfp-d3 #'%lfp-s1))
  (nil nil nil nil))

;;; An assigned variable keeps whatever it is given.
(defun %lfp-assigned (d)
  (declare (double-float d))
  (let ((a d))
    (setq a (list a))
    a))

;;; A captured one is read by the closure as the object it was bound to.
(defun %lfp-captured (d)
  (declare (double-float d))
  (let ((a (* d 2d0)))
    (list (* a a) (funcall (lambda () a)))))

;;; Mixed formats are left to the generic contagion.
(defun %lfp-mixed (s d)
  (declare (single-float s) (double-float d))
  (let ((a s) (b d)) (* a b)))

(deftest let-float-type-propagation.edges
  (list (%lfp-assigned 1d0) (%lfp-captured 1.5d0) (%lfp-mixed 2f0 3d0))
  ((1d0) (9d0 3d0) 6d0))

;;; A LOCALLY that declares something other than OPTIMIZE is not looked through:
;;; here it makes Y refer to the special binding.
(defun %lfp-special ()
  (let ((y 1d0))
    (declare (special y))
    (let ((y 2d0))
      (declare (double-float y))
      (let ((a (locally (declare (special y)) y)))
        (+ a 0d0)))))

(deftest let-float-type-propagation.locally-special
  (%lfp-special)
  1d0)

(setf dotcl:*save-sil* nil)
