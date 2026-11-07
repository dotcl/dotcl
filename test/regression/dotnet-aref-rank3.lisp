;;; AREF and (SETF AREF) work on .NET arrays of rank 3 (rank 1, 2 and 4 already did).

(deftest dotnet-aref-rank3.rank3
  (let ((a (dotnet:make-array "System.Int32" 2 2 2)))
    (setf (aref a 1 1 1) 5)
    (setf (aref a 0 1 0) 7)
    (list (aref a 1 1 1) (aref a 0 1 0) (aref a 0 0 0)
          (handler-case (aref a 2 0 0) (error () :error))))
  (5 7 0 :error))

(deftest dotnet-aref-rank3.other-ranks
  (let ((a1 (dotnet:make-array "System.Int32" 4)) (a2 (dotnet:make-array "System.Int32" 2 3)))
    (setf (aref a1 3) 9 (aref a2 1 2) 7)
    (list (aref a1 3) (aref a2 1 2)))
  (9 7))
