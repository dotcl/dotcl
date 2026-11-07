;;; String.Join with a value-type array (int[]) joins the elements ("1,2,3"); it used to take the
;;; array as one object and answer "System.Int32[]". string[] and other calls are unchanged.

(deftest dotnet-join-value-array.join
  (list (dotnet:static "System.String" "Join" "," (dotnet:new-array "System.Int32" 1 2 3))
        (dotnet:static "System.String" "Join" "-" (dotnet:new-array "System.Double" 1.5d0 2.5d0))
        (dotnet:static "System.String" "Join" "," (dotnet:new-array "System.String" "a" "b"))
        (dotnet:static "System.String" "Join" ";" "x" "y" "z")
        (dotnet:static "System.String" "Concat" "ab" "cd"))
  ("1,2,3" "1.5-2.5" "a,b" "x;y;z" "abcd"))
