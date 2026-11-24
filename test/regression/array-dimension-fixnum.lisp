;;; (array-dimension A N) is a fixnum (a C# int axis size), so a sum of it in a fixnum variable
;;; is computed in 64 bits with no boxed intermediate; the errors of the call are unchanged.

(defun %adf-sum (n a)
  (declare (fixnum n) (type (simple-array (unsigned-byte 32) (8 256)) a))
  (let ((s 0)) (declare (fixnum s))
    (dotimes (i n) (incf s (array-dimension a 1)))
    s))

(defun %adf-axis (a k)
  (declare (type (simple-array t (3 4)) a) (fixnum k))
  (let ((s 0)) (declare (fixnum s))
    (incf s (array-dimension a k))
    s))

(deftest array-dimension-fixnum.values
  (let ((a (make-array '(8 256) :element-type '(unsigned-byte 32)))
        (b (make-array '(3 4))))
    (list (%adf-sum 1000 a) (%adf-axis b 0) (%adf-axis b 1)))
  (256000 3 4))

(deftest array-dimension-fixnum.errors
  (let ((b (make-array '(3 4))))
    (flet ((o (f) (handler-case (funcall f) (program-error () :program-error) (type-error () :type-error))))
      (list (o (lambda () (%adf-axis b 2))) (o (lambda () (%adf-axis b -1)))
            (o (lambda () (let ((s 0)) (declare (fixnum s)) (incf s (array-dimension 5 0)) s))))))
  (:program-error :program-error :type-error))
