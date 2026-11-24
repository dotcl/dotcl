;;; System.Array search and predicate methods on a Lisp vector: the element type
;;; of the vector fixes the generic method's T, and a Lisp function becomes the
;;; Predicate<T>, Action<T> or Comparison<T> the method declares.

(deftest dotnet-array-methods.index-of-int32
  (let ((a (make-array 3 :element-type '(signed-byte 32) :initial-contents '(3 1 2))))
    (list (dotnet:static "System.Array" "IndexOf" a 2)
          (dotnet:static "System.Array" "IndexOf" a 3)
          (dotnet:static "System.Array" "LastIndexOf" a 2)))
  (2 0 2))

(deftest dotnet-array-methods.index-of-unsigned-byte-8
  (let ((a (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(3 1 2))))
    (list (dotnet:static "System.Array" "IndexOf" a 2)
          (dotnet:static "System.Array" "LastIndexOf" a 2)
          (dotnet:static "System.Array" "Exists" a (lambda (x) (= x 2)))))
  (2 2 t))

(deftest dotnet-array-methods.index-of-signed-byte-64
  (let ((a (make-array 3 :element-type '(signed-byte 64) :initial-contents '(3 1 2))))
    (list (dotnet:static "System.Array" "IndexOf" a 2)
          (dotnet:static "System.Array" "LastIndexOf" a 2)
          (dotnet:static "System.Array" "Exists" a (lambda (x) (= x 2)))))
  (2 2 t))

(deftest dotnet-array-methods.index-of-double-float
  (let ((a (make-array 3 :element-type 'double-float :initial-contents '(3d0 1d0 2d0))))
    (list (dotnet:static "System.Array" "IndexOf" a 2d0)
          (dotnet:static "System.Array" "LastIndexOf" a 2d0)
          (dotnet:static "System.Array" "Exists" a (lambda (x) (= x 2d0)))))
  (2 2 t))

(deftest dotnet-array-methods.index-of-single-float
  (let ((a (make-array 3 :element-type 'single-float :initial-contents '(3.0 1.0 2.0))))
    (list (dotnet:static "System.Array" "IndexOf" a 2.0)
          (dotnet:static "System.Array" "LastIndexOf" a 2.0)
          (dotnet:static "System.Array" "Exists" a (lambda (x) (= x 2.0)))))
  (2 2 t))

(deftest dotnet-array-methods.index-of-character
  (let ((a (make-array 3 :element-type 'character :initial-contents '(#\c #\a #\b))))
    (list (dotnet:static "System.Array" "IndexOf" a #\b)
          (dotnet:static "System.Array" "LastIndexOf" a #\b)
          (dotnet:static "System.Array" "Exists" a (lambda (x) (char= x #\b)))))
  (2 2 t))

(deftest dotnet-array-methods.index-of-general-vector
  (let ((a (make-array 3 :element-type t :initial-contents '(3 1 2))))
    (list (dotnet:static "System.Array" "IndexOf" a 2)
          (dotnet:static "System.Array" "LastIndexOf" a 2)
          (dotnet:static "System.Array" "Exists" a (lambda (x) (= x 2)))))
  (2 2 t))

(deftest dotnet-array-methods.find
  (let ((a (make-array 3 :element-type '(signed-byte 32) :initial-contents '(3 1 2))))
    (dotnet:static "System.Array" "Find" a (lambda (x) (= x 2))))
  2)

(deftest dotnet-array-methods.find-index
  (let ((a (make-array 3 :element-type '(signed-byte 32) :initial-contents '(3 1 2))))
    (dotnet:static "System.Array" "FindIndex" a (lambda (x) (= x 2))))
  2)

(deftest dotnet-array-methods.true-for-all
  (let ((a (make-array 3 :element-type '(signed-byte 32) :initial-contents '(3 1 2))))
    (list (dotnet:static "System.Array" "TrueForAll" a (lambda (x) (= x 2)))
          (dotnet:static "System.Array" "TrueForAll" a (lambda (x) (< x 4)))))
  (nil t))

(deftest dotnet-array-methods.for-each
  (let ((a (make-array 3 :element-type '(signed-byte 32) :initial-contents '(3 1 2)))
        (sum 0))
    (dotnet:static "System.Array" "ForEach" a (lambda (x) (setq sum (+ sum x))))
    sum)
  6)

(deftest dotnet-array-methods.sort-with-comparison
  (let ((a (make-array 3 :element-type '(signed-byte 32) :initial-contents '(3 1 2))))
    (dotnet:static "System.Array" "Sort" a (lambda (x y) (- y x)))
    (coerce a 'list))
  (3 2 1))

(deftest dotnet-array-methods.sort-general-vector
  (let ((a (vector 3 1 2)))
    (dotnet:static "System.Array" "Sort" a (lambda (x y) (- y x)))
    (coerce a 'list))
  (3 2 1))

(deftest dotnet-array-methods.binary-search
  (let ((a (make-array 3 :element-type '(signed-byte 32) :initial-contents '(1 2 3))))
    (dotnet:static "System.Array" "BinarySearch" a 2))
  1)
