;;; BIT-AND and the other bit-array operations signal an error when the arrays have the same total size but
;;; different dimensions, for the arguments and for the result array.

(defun %bad (d &optional (init 1)) (make-array d :element-type 'bit :initial-element init))
(defmacro %bao-err (form) `(handler-case (progn ,form :no-error) (error () :error)))

(deftest bit-array-ops-dimensions.mismatch
  (list (%bao-err (bit-and (%bad '(2 2)) (%bad '(4))))
        (%bao-err (bit-and (%bad '(2 3)) (%bad '(3 2))))
        (%bao-err (bit-ior (%bad '(2 2) 0) (%bad '(4) 1)))
        (%bao-err (bit-and (%bad '(2 2)) (%bad '(2 2)) (%bad '(1 4))))
        (%bao-err (bit-and (%bad '(2 2)) (%bad '(2 2)) (%bad '(4))))
        (%bao-err (bit-not (%bad '(2 2)) (%bad '(4)))))
  (:error :error :error :error :error :error))

(defun %flat (a) (loop for i below (array-total-size a) collect (row-major-aref a i)))

(deftest bit-array-ops-dimensions.match
  (mapcar #'%flat
          (list (bit-and (%bad '(2 2)) (%bad '(2 2) 0))
                (bit-xor #*101 #*111)
                (let ((r (%bad '(2 2) 0))) (bit-ior (%bad '(2 2) 1) (%bad '(2 2) 0) r) r)
                (bit-not (%bad '(2 3) 0))))
  ((0 0 0 0) (0 1 0) (1 1 1 1) (1 1 1 1 1 1)))
