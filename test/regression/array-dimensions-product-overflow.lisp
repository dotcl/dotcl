;;; The element count of a multi-dimensional array is the product of its dimensions: one
;;; that does not fit the total size limit is an error, not a count wrapped into range.

(deftest array-dimensions-product-overflow.make-array
  (flet ((rejected (dims) (handler-case (progn (make-array dims) nil) (error () t))))
    (list (rejected (list 65536 65536))
          (rejected (list 70000 70000 2))
          (rejected (list 65536 65536 65536))
          (rejected (list 2147483647 2))
          (rejected (list 4294967296 4294967296))
          (array-total-size (make-array (list 3 4)))
          (array-dimensions (make-array (list 2 3 4)))
          (array-total-size (make-array (list 0 65536 65536)))))
  (t t t t t 12 (2 3 4) 0))

(deftest array-dimensions-product-overflow.adjust-array
  (let ((a (make-array '(2 3) :adjustable t :initial-element 0)))
    (list (handler-case (progn (adjust-array a '(65536 65536)) nil) (error () t))
          (handler-case (progn (adjust-array a '(70000 70000)) nil) (error () t))
          (array-dimensions (adjust-array a '(3 4)))
          (array-total-size a)))
  (t t (3 4) 12))
