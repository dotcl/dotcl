;;; Growing an adjustable multi-dimensional array without :initial-element fills the new
;;; elements with the element type's default, not NIL (a BIT array signalled TYPE-ERROR).

(deftest adjust-array-multidim-default.bit-grow
  (let* ((a (make-array '(2 2) :element-type 'bit :adjustable t :initial-element 1))
         (b (adjust-array a '(3 4))))
    (list (array-dimensions b) (aref b 0 0) (aref b 1 1) (eq a b)))
  ((3 4) 1 1 t))

(deftest adjust-array-multidim-default.bit-grow-rank3
  (let* ((a (make-array '(2 2 2) :element-type 'bit :adjustable t :initial-element 1))
         (b (adjust-array a '(2 2 3))))
    (list (array-dimensions b) (aref b 1 1 1) (aref b 0 0 0)))
  ((2 2 3) 1 1))

(deftest adjust-array-multidim-default.other-element-types
  (flet ((grown (et init)
           (let ((b (adjust-array (make-array '(2 2) :element-type et :adjustable t
                                              :initial-element init)
                                  '(3 3))))
             (list (aref b 0 0) (aref b 1 1)))))
    (list (grown 'character #\a) (grown '(unsigned-byte 8) 7) (grown 'double-float 1d0)))
  ((#\a #\a) (7 7) (1d0 1d0)))

(deftest adjust-array-multidim-default.explicit-initial-element-and-shrink
  (list (let ((b (adjust-array (make-array '(2 2) :element-type 'bit :adjustable t
                                           :initial-element 1)
                               '(3 3) :initial-element 0)))
          (list (aref b 0 0) (aref b 2 2)))
        (let ((b (adjust-array (make-array '(3 3) :element-type 'bit :adjustable t
                                           :initial-element 1)
                               '(2 2))))
          (list (array-dimensions b) (aref b 1 1))))
  ((1 0) ((2 2) 1)))
