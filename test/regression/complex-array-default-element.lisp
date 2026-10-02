;;; An array specialized to (complex single-float) or (complex double-float)
;;; starts out holding complex zeros when no :initial-element is given, the way
;;; a double-float array starts out holding 0.0d0. It held NIL, which is not of
;;; the element type. magicl allocates LAPACK work arrays with plain MAKE-ARRAY
;;; and copies every element into foreign memory, so REALPART got NIL.

(deftest complex-array-default-element.double
  (let ((a (make-array 3 :element-type '(complex double-float))))
    (list (every (lambda (z) (typep z '(complex double-float))) a)
          (every #'zerop a)))
  (t t))

(deftest complex-array-default-element.single
  (let ((a (make-array '(2 2) :element-type '(complex single-float))))
    (list (typep (aref a 1 1) '(complex single-float))
          (zerop (aref a 0 1))))
  (t t))

(deftest complex-array-default-element.adjust
  (let ((a (adjust-array (make-array 1 :element-type '(complex double-float)
                                       :initial-element #c(1d0 2d0) :adjustable t)
                         3)))
    (list (aref a 0) (typep (aref a 2) '(complex double-float))))
  (#c(1d0 2d0) t))

(deftest complex-array-default-element.initial-element
  (aref (make-array 2 :element-type '(complex double-float) :initial-element #c(3d0 4d0)) 1)
  #c(3d0 4d0))
