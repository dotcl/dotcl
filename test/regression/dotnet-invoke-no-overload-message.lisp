;;; An argument no public overload accepts reaches the interface-member fallback, which used to report
;;; only "implements Add separately for ICollection`1 and IList": the real reason is that no public Add
;;; takes the argument. The message still names the dotnet:cast remedy and now says what was given.

(deftest-emitting-only dotnet-invoke-no-overload-message.names-the-arguments
  (let* ((l (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List" (list "System.Int32"))))
         (msg (handler-case (progn (dotnet:invoke l "Add" (expt 2 40)) nil)
                (error (c) (princ-to-string c)))))
    (list (and msg (search "name the one you mean with dotnet:cast" msg) t)
          (and msg (search "no public Add overload" msg) t)
          (and msg (search "Int64" msg) t)))
  (t t t))

(deftest-emitting-only dotnet-invoke-no-overload-message.valid-add-works
  (let ((l (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List" (list "System.Int32")))))
    (dotnet:invoke l "Add" 7)
    (dotnet:invoke l "Add" -1)
    (list (dotnet:invoke l "Count") (dotnet:invoke l "Item" 1)))
  (2 -1))
