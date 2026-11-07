;;; ARRAY-ROW-MAJOR-INDEX checks each subscript against its own axis.

(defun %armi-outcome (thunk)
  (handler-case (funcall thunk)
    (error () :error)))

(deftest array-row-major-index.in-range
  (let ((a (make-array '(3 4) :initial-element 0)))
    (list (array-row-major-index a 0 0)
          (array-row-major-index a 1 2)
          (array-row-major-index a 2 3)))
  (0 6 11))

(deftest array-row-major-index.subscript-past-axis
  (let ((a (make-array '(3 4) :initial-element 0)))
    (list (%armi-outcome (lambda () (array-row-major-index a 1 4)))
          (%armi-outcome (lambda () (array-row-major-index a 3 0)))
          (%armi-outcome (lambda () (array-row-major-index a -1 0)))))
  (:error :error :error))

(deftest array-row-major-index.vector-with-fill-pointer
  (let ((v (make-array 5 :fill-pointer 2 :initial-element 0)))
    (list (array-row-major-index v 4)
          (%armi-outcome (lambda () (array-row-major-index v 5)))))
  (4 :error))
