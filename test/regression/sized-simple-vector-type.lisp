;;; (SIMPLE-VECTOR n) and (SIMPLE-BIT-VECTOR n) checked on a typed slot store: the right length
;;; and element type pass, anything else is a TYPE-ERROR.

(defstruct ssvt (v (vector 1 2 3 4) :type (simple-vector 4)) (b (make-array 8 :element-type 'bit) :type (simple-bit-vector 8)))

(deftest sized-simple-vector-type.accepts
  (let ((s (make-ssvt)))
    (setf (ssvt-v s) (vector 9 8 7 6))
    (setf (ssvt-b s) (make-array 8 :element-type 'bit :initial-element 1))
    (list (coerce (ssvt-v s) 'list) (coerce (ssvt-b s) 'list)))
  ((9 8 7 6) (1 1 1 1 1 1 1 1)))

(deftest sized-simple-vector-type.rejects
  (let ((s (make-ssvt)))
    (flet ((bad (slot val)
             (handler-case (progn (if (eq slot :v) (setf (ssvt-v s) val) (setf (ssvt-b s) val)) :stored)
               (type-error () :type-error))))
      (list (bad :v (vector 1 2 3))
            (bad :v (make-array 4 :adjustable t))
            (bad :v (make-array 4 :element-type 'bit))
            (bad :v nil)
            (bad :b (make-array 7 :element-type 'bit))
            (bad :b (vector 0 0 0 0 0 0 0 0))
            (bad :v (vector 1 2 3 4)))))
  (:type-error :type-error :type-error :type-error :type-error :type-error :stored))
