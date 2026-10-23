;;; (dotnet:null) must arrive as a .NET null also where an argument is
;;; marshalled without a declared parameter type (the binder path: an Object
;;; parameter reached at run time, a COM IDispatch member).

(deftest dotnet-null-into-object-parameter
  (dotnet:static "System.Object" "ReferenceEquals" (dotnet:null) nil)
  t)

(deftest dotnet-null-into-object-array
  (let ((a (dotnet:make-array "System.Object" 1)))
    (dotnet:invoke a "SetValue" "x" 0)
    (dotnet:invoke a "SetValue" (dotnet:null) 0)
    (list (dotnet:invoke a "GetValue" 0)
          (dotnet:static "System.Object" "ReferenceEquals" (dotnet:invoke a "GetValue" 0) nil)))
  (nil t))

(deftest dotnet-null-into-arraylist
  (let ((l (dotnet:new "System.Collections.ArrayList")))
    (dotnet:invoke l "Add" (dotnet:null))
    (list (dotnet:invoke l "Count")
          (dotnet:invoke l "Contains" (dotnet:null))))
  (1 t))
