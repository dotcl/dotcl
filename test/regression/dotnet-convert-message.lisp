;;; A failed argument conversion to .NET names the Lisp value and its type.

(deftest dotnet-convert-message.value
  (let ((m (handler-case (dotnet:invoke "abc" "Substring" "x")
             (error (c) (princ-to-string c)))))
    (and (search "\"x\"" m) (search "Int32" m) t))
  t)

(deftest dotnet-convert-message.lisp-type
  (let ((m (handler-case (dotnet:static "System.Math" "Sqrt" "a")
             (error (c) (princ-to-string c)))))
    (and (not (search "LispString" m)) (search "Double" m) t))
  t)
