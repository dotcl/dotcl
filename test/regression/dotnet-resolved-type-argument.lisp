;;; Where a type name is taken, an already resolved System.Type (from DOTNET:RESOLVE-TYPE) is
;;; accepted too: DOTNET:STATIC used to fail with "type not found: #<DOTNET System.RuntimeType ...>".

(deftest dotnet-resolved-type-argument.static-and-new
  (let ((m (dotnet:resolve-type "System.Math"))
        (sb (dotnet:resolve-type "System.Text.StringBuilder"))
        (i (dotnet:resolve-type "System.Int32")))
    (list (dotnet:static m "Sqrt" 4d0)
          (dotnet:static m "Max" 3 8)
          (dotnet:static "System.Math" "Sqrt" 9d0)
          (dotnet:invoke (dotnet:new sb "ab") "ToString")
          (dotnet:invoke (dotnet:new-array i 3) "GetLength" 0)
          (dotnet:is-instance-of 5 i)))
  (2d0 8 3d0 "ab" 1 t))

(deftest dotnet-resolved-type-argument.unknown-name-still-errors
  (handler-case (dotnet:static "No.Such.TypeAtAll" "X") (error () :error))
  :error)
