;;; (error <.NET exception object>) signals that exception.

(deftest error-dotnet-exception.throw
  (handler-case (error (dotnet:new "System.InvalidOperationException" "bad"))
    (error (c)
      (list (dotnet:exception-typep c "System.InvalidOperationException")
            (and (search "bad" (princ-to-string c)) t))))
  (t t))

(deftest error-dotnet-exception.rethrow
  (handler-case
      (handler-case (dotnet:static "System.Int32" "Parse" "x")
        (error (c) (error (dotnet:exception-object c))))
    (error (c2) (dotnet:exception-typep c2 "System.FormatException")))
  t)

(deftest error-dotnet-exception.handler-bind
  (block nil
    (dotnet:handler-bind (("System.ArgumentException" (e) (declare (ignore e)) (return-from nil :seen)))
      (error (dotnet:new "System.ArgumentException" "a"))))
  :seen)

(deftest error-dotnet-exception.non-condition-still-rejected
  (handler-case (error (dotnet:new "System.Object"))
    (type-error () :type-error))
  :type-error)
