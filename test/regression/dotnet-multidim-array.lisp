;;; Multi-dimensional .NET array queries, MAKE-ARRAY dimensions, and a ref parameter.

(defun %dma-condition (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (error (e) e)))

(defun %dma-mentions (text needle)
  (and (search needle (princ-to-string text) :test #'char-equal) t))

(deftest dotnet-multidim-array.queries
  (let ((a (dotnet:make-array "System.Int32" 2 3)))
    (list (array-rank a) (array-dimensions a) (array-dimension a 1)
          (array-total-size a) (length a) (aref a 1 2)
          (row-major-aref a 5) (array-in-bounds-p a 1 2)
          (array-in-bounds-p a 2 0) (array-row-major-index a 1 2)))
  (2 (2 3) 3 6 6 0 0 t nil 5))

(deftest dotnet-multidim-array.row-major-write
  (let ((a (dotnet:make-array "System.Int32" '(2 3))))
    (setf (row-major-aref a 4) 42)
    (list (row-major-aref a 4) (aref a 1 1) (row-major-aref a 0)))
  (42 42 0))

(deftest dotnet-multidim-array.jagged-is-rank-1
  (let ((a (dotnet:new-array "System.Int32[]"
                             (dotnet:new-array "System.Int32" 1 2)
                             (dotnet:new-array "System.Int32" 3 4 5))))
    (list (array-rank a) (array-dimensions a) (array-total-size a)
          (array-in-bounds-p a 1) (aref (aref a 1) 2)))
  (1 (2) 2 t 5))

(deftest dotnet-multidim-array.aref-wrong-rank
  (let ((e (%dma-condition (lambda () (aref (dotnet:make-array "System.Int32" '(2 3)) 5)))))
    (list (typep e 'error)
          (%dma-mentions e "rank-2")
          (%dma-mentions e "one-dimensional")))
  (t t nil))

(deftest dotnet-multidim-array.setf-aref-wrong-rank
  (let ((e (%dma-condition
            (lambda () (setf (aref (dotnet:make-array "System.Int32" '(2 3)) 5) 1)))))
    (list (typep e 'error) (%dma-mentions e "rank-2")))
  (t t))

(deftest dotnet-multidim-array.aref-out-of-range
  (let ((e (%dma-condition
            (lambda () (aref (dotnet:make-array "System.Int32" '(2 3)) 1 3)))))
    (list (typep e 'type-error)
          (%dma-mentions e "axis 1")
          (%dma-mentions e "size 3")))
  (t t t))

(deftest dotnet-multidim-array.setf-aref-out-of-range
  (let ((e (%dma-condition
            (lambda () (setf (aref (dotnet:make-array "System.Int32" '(2 3)) 2 0) 1)))))
    (list (typep e 'type-error)
          (%dma-mentions e "axis 0")
          (%dma-mentions e "size 2")))
  (t t t))

(deftest dotnet-multidim-array.row-major-out-of-range
  (let ((e (%dma-condition
            (lambda () (row-major-aref (dotnet:make-array "System.Int32" '(2 3)) 6)))))
    (list (typep e 'type-error) (%dma-mentions e "size 6")))
  (t t))

(deftest dotnet-multidim-array.setf-row-major-out-of-range
  (let ((e (%dma-condition
            (lambda ()
              (setf (row-major-aref (dotnet:make-array "System.Int32" '(2 3)) 6) 1)))))
    (list (typep e 'type-error) (%dma-mentions e "size 6")))
  (t t))

(deftest dotnet-multidim-array.array-in-bounds-wrong-count
  (let ((e (%dma-condition
            (lambda () (array-in-bounds-p (dotnet:make-array "System.Int32" '(2 3)) 0)))))
    (list (typep e 'error) (%dma-mentions e "rank 2")))
  (t t))

(deftest dotnet-multidim-array.make-array-list-dims
  (let ((a (dotnet:make-array "System.Int32" '(2 3))))
    (list (array-dimensions a) (array-total-size a)))
  ((2 3) 6))

(deftest dotnet-multidim-array.make-array-several-dims-still-work
  (let ((a (dotnet:make-array "System.Int32" 2 3 :initial-element 4)))
    (list (array-dimensions a) (aref a 0 0) (aref a 1 2)))
  ((2 3) 4 4))

(deftest dotnet-multidim-array.make-array-initial-element
  (let ((a (dotnet:make-array "System.Int32" 3 :initial-element 7)))
    (list (aref a 0) (aref a 1) (aref a 2)))
  (7 7 7))

(deftest dotnet-multidim-array.make-array-initial-contents
  (let ((a (dotnet:make-array "System.Int32" 3 :initial-contents '(1 2 3))))
    (list (aref a 0) (aref a 1) (aref a 2)))
  (1 2 3))

(deftest dotnet-multidim-array.make-array-nested-initial-contents
  (let ((a (dotnet:make-array "System.Int32" '(2 3)
                              :initial-contents '((1 2 3) (4 5 6)))))
    (list (aref a 0 0) (aref a 1 2) (row-major-aref a 3)))
  (1 6 4))

(deftest dotnet-multidim-array.make-array-wrong-shape
  (let ((e (%dma-condition
            (lambda ()
              (dotnet:make-array "System.Int32" '(2 3)
                                 :initial-contents '(1 2 3))))))
    (list (typep e 'error) (%dma-mentions e "INITIAL-CONTENTS")))
  (t t))

(deftest dotnet-multidim-array.make-array-both-initials
  (let ((e (%dma-condition
            (lambda ()
              (dotnet:make-array "System.Int32" 3
                                 :initial-element 0
                                 :initial-contents '(1 2 3))))))
    (list (typep e 'error)
          (%dma-mentions e "INITIAL-ELEMENT")
          (%dma-mentions e "INITIAL-CONTENTS")))
  (t t t))

(deftest dotnet-multidim-array.static-generic-ref
  (let ((a (dotnet:new-array "System.Int32" 1 2 3)))
    (list (dotnet:static-generic "System.Array" "Resize" '("System.Int32") a 5)
          (length a)))
  (nil 3))

(deftest dotnet-multidim-array.call-out-generic-ref
  (let ((a (dotnet:new-array "System.Int32" 1 2 3)))
    (multiple-value-bind (returned resized)
        (dotnet:call-out-generic "System.Array" "Resize" '("System.Int32") a 5)
      (list returned (length resized) (aref resized 0) (aref resized 2)
            (aref resized 4))))
  (t 5 1 3 0))
