;;; The sequence functions take a .NET array or collection, reading its elements.

(defun dasf-arr () (dotnet:new-array "System.Int32" 3 1 2 3))

(deftest dotnet-array-sequence-functions.search
  (list (find 2 (dasf-arr)) (find-if #'oddp (dasf-arr)) (position 3 (dasf-arr))
        (position-if #'evenp (dasf-arr)) (count 3 (dasf-arr)) (count-if #'oddp (dasf-arr)))
  (2 3 0 2 2 3))

(deftest dotnet-array-sequence-functions.remove
  (list (coerce (remove 3 (dasf-arr)) 'list)
        (coerce (remove-if #'oddp (dasf-arr)) 'list)
        (coerce (remove-duplicates (dasf-arr)) 'list)
        (coerce (substitute 9 3 (dasf-arr)) 'list)
        (coerce (delete 1 (dasf-arr)) 'list))
  ((1 2) (2) (1 2 3) (9 1 2 9) (3 2 3)))

(deftest dotnet-array-sequence-functions.copy
  (list (coerce (subseq (dasf-arr) 1) 'list)
        (coerce (copy-seq (dasf-arr)) 'list)
        (coerce (reverse (dasf-arr)) 'list)
        (coerce (sort (dasf-arr) #'<) 'list))
  ((1 2 3) (3 1 2 3) (3 2 1 3) (1 2 3 3)))

(deftest dotnet-array-sequence-functions.fold
  (list (reduce #'+ (dasf-arr)) (reduce #'+ (dasf-arr) :initial-value 10)
        (every #'numberp (dasf-arr)) (some #'evenp (dasf-arr)) (every #'= (dasf-arr) (dasf-arr))
        (notany #'stringp (dasf-arr)))
  (9 19 t t t t))

(deftest dotnet-array-sequence-functions.combine
  (list (concatenate 'list (dasf-arr) '(0))
        (search (dotnet:new-array "System.Int32" 1 2) (dasf-arr))
        (mismatch (dasf-arr) '(3 1 5))
        (replace (list 0 0) (dasf-arr)))
  ((3 1 2 3 0) 1 2 (3 1)))

(deftest dotnet-array-sequence-functions.lists-and-vectors-unchanged
  (list (find 2 '(1 2 3)) (position 2 #(1 2 3)) (reduce #'+ "abc" :key #'char-code)
        (remove 2 '(1 2 3)))
  (2 1 294 (1 3)))
