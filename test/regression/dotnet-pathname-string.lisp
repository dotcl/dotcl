;;; A pathname is accepted where a .NET method takes a string, as its namestring.

(deftest dotnet-pathname-string.path
  (dotnet:static "System.IO.Path" "GetFileName" #P"/tmp/a.txt")
  "a.txt")

(deftest dotnet-pathname-string.ctor
  (dotnet:invoke (dotnet:new "System.IO.FileInfo" #P"/tmp/a.txt") "Name")
  "a.txt")

(deftest dotnet-pathname-string.wild-refused
  (handler-case (dotnet:static "System.IO.Path" "GetFileName" #P"/tmp/*.txt")
    (type-error (c) (and (search "wild pathname" (princ-to-string c)) :refused)))
  :refused)

(deftest dotnet-pathname-string.not-for-numbers
  (handler-case (dotnet:static "System.Math" "Abs" #P"a")
    (error () :error))
  :error)
