;;; length, elt, coerce and map read a .NET array or collection as a sequence.

(deftest dotnet-array-sequence.length
  (list (length (dotnet:new-array "System.String" "x" "y"))
        (length (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                                      (list "System.Int32")))))
  (2 0))

(deftest dotnet-array-sequence.elt
  (elt (dotnet:new-array "System.String" "x" "y") 1)
  "y")

(deftest dotnet-array-sequence.coerce
  (list (coerce (dotnet:new-array "System.Int32" 1 2 3) 'list)
        (equalp (coerce (dotnet:new-array "System.Int32" 1 2 3) 'vector) #(1 2 3)))
  ((1 2 3) t))

(deftest dotnet-array-sequence.map-matches
  (map 'list (lambda (m) (dotnet:invoke m "Value"))
       (dotnet:static "System.Text.RegularExpressions.Regex" "Matches" "a1b22" "\\d+"))
  ("1" "22"))

(deftest dotnet-array-sequence.identity-kept
  (let ((b (dotnet:make-array "System.Byte" 3)))
    (list (length b) (progn (setf (aref b 0) 7) (aref b 0))))
  (3 7))
