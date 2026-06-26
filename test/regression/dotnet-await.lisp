;;; Regression tests for dotnet:await — blocking await of .NET awaitables (step A)

;;; non-generic Task completes -> NIL (void)
(deftest d1301-await-task-void
  (dotnet:await (dotnet:static "System.Threading.Tasks.Task" "Delay" 5))
  nil)

;;; Task<int> -> marshalled int
(deftest d1301-await-task-int
  (dotnet:await (dotnet:static-generic "System.Threading.Tasks.Task" "FromResult"
                                       '("System.Int32") 42))
  42)

;;; Task<string> -> marshalled string
(deftest d1301-await-task-string
  (dotnet:await (dotnet:static-generic "System.Threading.Tasks.Task" "FromResult"
                                       '("System.String") "hi"))
  "hi")

;;; ValueTask<int> (struct awaitable) -> int
(deftest d1301-await-valuetask-int
  (dotnet:await (dotnet:new "System.Threading.Tasks.ValueTask`1[System.Int32]" 7))
  7)

;;; faulted Task rethrows the INNER exception (not AggregateException),
;;; so handler-case sees the real condition message.
(deftest d1301-await-faulted-inner
  (handler-case
      (dotnet:await
       (dotnet:static-generic "System.Threading.Tasks.Task" "FromException"
                              '("System.Int32")
                              (dotnet:new "System.InvalidOperationException" "boom")))
    (error (e) (let ((s (format nil "~A" e)))
                 (and (search "boom" s) t))))
  t)

;;; non-awaitable argument signals an error
(deftest d1301-await-non-awaitable
  (handler-case (progn (dotnet:await 5) :no-error)
    (error () :error))
  :error)
