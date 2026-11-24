;;; A method that returns a Span cannot be called through reflection; the message says why.

(deftest dotnet-nested-invocation.span-return-message
  (let ((m (handler-case (dotnet:invoke "hello" "AsSpan")
             (error (c) (princ-to-string c)))))
    (and (search "ref struct" m)
         (not (search "Specified method is not supported" m))
         t))
  t)
