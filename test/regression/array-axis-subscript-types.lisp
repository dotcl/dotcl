;;; The axis number of ARRAY-DIMENSION and the subscripts of ARRAY-IN-BOUNDS-P are integers.
;;; A symbol, a float or NIL there is a TYPE-ERROR, not a dimension or NIL.

(defmacro %aast-error-p (form)
  `(handler-case (progn ,form nil) (type-error () t)))

(deftest array-axis-subscript-types.array-dimension-axis
  (list (%aast-error-p (array-dimension #(1) 'aast-bad))
        (%aast-error-p (array-dimension #(1) 1.0))
        (%aast-error-p (array-dimension #(1) nil))
        (%aast-error-p (array-dimension (make-array '(2 3)) 'aast-bad)))
  (t t t t))

(deftest array-axis-subscript-types.array-dimension-valid
  (list (array-dimension #(1 2 3) 0) (array-dimension (make-array '(2 3)) 1)
        (array-dimension "abcd" 0) (array-dimension (make-array 0) 0)
        (handler-case (array-dimension #(1) 5) (error () :error))
        (handler-case (array-dimension #(1) -1) (error () :error)))
  (3 3 4 0 :error :error))

(deftest array-axis-subscript-types.in-bounds-subscripts
  (list (%aast-error-p (array-in-bounds-p #(1 2) 'aast-bad))
        (%aast-error-p (array-in-bounds-p (make-array '(2 2)) 0 'aast-bad))
        (%aast-error-p (array-in-bounds-p #(1 2) 1.0)))
  (t t t))

(deftest array-axis-subscript-types.in-bounds-valid
  (list (array-in-bounds-p #(1 2) 1) (array-in-bounds-p #(1 2) 2) (array-in-bounds-p #(1 2) -1)
        (array-in-bounds-p #(1 2) (expt 10 30)) (array-in-bounds-p (make-array '(2 2)) 1 1)
        (array-in-bounds-p (make-array '(2 2)) 1 2))
  (t nil nil nil t nil))
