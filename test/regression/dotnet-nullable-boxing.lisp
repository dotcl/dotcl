;;; A Nullable<T> is boxed by the CLR as its underlying T; the empty value is NIL.

(defun %dnb-message (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error (e) (princ-to-string e))))

(defun %dnb-int-nullable ()
  (dotnet:make-generic-type "System.Nullable" (list "System.Int32")))

(deftest dotnet-nullable-boxing.new-empty-is-nil
  (list (dotnet:new (%dnb-int-nullable))
        (dotnet:new (dotnet:make-generic-type "System.Nullable" (list "System.DateTime"))))
  (nil nil))

(deftest dotnet-nullable-boxing.new-value-is-the-underlying-value
  (let* ((nt (%dnb-int-nullable))
         (n (dotnet:new nt 5)))
    (list (dotnet:static "System.Convert" "ToInt32" n)
          (dotnet:invoke n "ToString")
          (dotnet:invoke (dotnet:static "System.Nullable" "GetUnderlyingType" nt)
                         "get_FullName")))
  (5 "5" "System.Int32"))

(deftest dotnet-nullable-boxing.member-error-explains-boxing
  (let* ((n (dotnet:new (%dnb-int-nullable) 5))
         (hasvalue (%dnb-message (lambda () (dotnet:-> n "HasValue"))))
         (value (%dnb-message (lambda () (dotnet:-> n "Value")))))
    (list (and hasvalue (search "Nullable<T> is boxed" hasvalue)
               (search "NIL" hasvalue) t)
          (and value (search "Nullable<T> is boxed" value)
               (search "NIL" value) t)))
  (t t))

(deftest dotnet-nullable-boxing.list-element-round-trips-value-and-empty
  (let* ((nt (%dnb-int-nullable))
         (lt (dotnet:make-generic-type "System.Collections.Generic.List" (list nt)))
         (l (dotnet:new lt)))
    (dotnet:invoke l "Add" 5)
    (dotnet:invoke l "Add" nil)
    (list (dotnet:invoke l "get_Count")
          (dotnet:invoke l "get_Item" 0)
          (dotnet:invoke l "get_Item" 1)))
  (2 5 nil))

(deftest dotnet-nullable-boxing.max-over-nullable-list
  (let* ((nt (%dnb-int-nullable))
         (lt (dotnet:make-generic-type "System.Collections.Generic.List" (list nt)))
         (l (dotnet:new lt)))
    (dotnet:invoke l "Add" 3)
    (dotnet:invoke l "Add" nil)
    (dotnet:invoke l "Add" 7)
    (dotnet:static-generic "System.Linq.Enumerable" "Max" (list nt) l))
  7)

(deftest dotnet-nullable-boxing.compare-with-value-and-nil
  (list (dotnet:static-generic "System.Nullable" "Compare" '("System.Int32") 5 nil)
        (dotnet:static-generic "System.Nullable" "Compare" '("System.Int32") nil 5)
        (dotnet:static-generic "System.Nullable" "Compare" '("System.Int32") 5 5))
  (1 -1 0))

(deftest dotnet-nullable-boxing.equals-generic-static
  (list (dotnet:static-generic "System.Nullable" "Equals" '("System.Int32") 5 5)
        (dotnet:static-generic "System.Nullable" "Equals" '("System.Int32") 5 nil))
  (t nil))
