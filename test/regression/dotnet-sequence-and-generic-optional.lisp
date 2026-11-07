;;; A Lisp list or vector passed where a method has an IEnumerable<T> overload and a
;;; catch-all (params object[]) one: String.Join / String.Concat / StringBuilder.AppendJoin
;;; used to take the list as ONE object and print it ("(\"a\" \"b\")").
;;; And dotnet:static-generic / dotnet:invoke-generic leave a trailing C# optional
;;; parameter out, like dotnet:static does (JsonSerializer.Serialize<T>(value, options = null)).

(dotnet:load-assembly "System.Text.Json")

(deftest dsg-join-list
  (list (dotnet:static "System.String" "Join" "-" '("a" "b" "c"))
        (dotnet:static "System.String" "Join" "," (vector "x" "y"))
        (dotnet:static "System.String" "Concat" '("p" "q"))
        (dotnet:static "System.String" "Join" "," '(1 2 3)))
  ("a-b-c" "x,y" "pq" "1,2,3"))

(deftest dsg-append-join-list
  (let ((sb (dotnet:new "System.Text.StringBuilder")))
    (dotnet:invoke sb "AppendJoin" ", " '("a" "b"))
    (dotnet:invoke sb "ToString"))
  "a, b")

(deftest dsg-join-params-unchanged
  (list (dotnet:static "System.String" "Join" ";" "x" "y" "z")
        (dotnet:static "System.String" "Join" "," (dotnet:new-array "System.String" "a" "b"))
        (dotnet:static "System.String" "Join" "," (dotnet:new-array "System.Int32" 1 2)))
  ("x;y;z" "a,b" "1,2"))

(deftest dsg-static-generic-optional-omitted
  (list (dotnet:static-generic "System.Text.Json.JsonSerializer" "Serialize" '("System.Int32[]")
                               (dotnet:new-array "System.Int32" 1 2 3))
        (dotnet:static-generic "System.Text.Json.JsonSerializer" "Serialize" '("System.String") "hi"))
  ("[1,2,3]" "\"hi\""))

(deftest dsg-static-generic-optional-supplied
  (dotnet:static-generic "System.Text.Json.JsonSerializer" "Serialize" '("System.Int32[]")
                         (dotnet:new-array "System.Int32" 4 5) (dotnet:null))
  "[4,5]")

(deftest dsg-static-generic-deserialize-optional
  (dotnet:invoke
   (dotnet:static-generic "System.Text.Json.JsonSerializer" "Deserialize" '("System.Int32[]") "[7,8]")
   "Length")
  2)
