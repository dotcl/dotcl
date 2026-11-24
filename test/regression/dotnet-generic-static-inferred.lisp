;;; A generic method infers type arguments from a delegate or plain values when one overload fits.

(defun dgsi-error (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error (c) (princ-to-string c))))

(deftest dotnet-generic-static-inferred.task-run-func
  (dotnet:await
   (dotnet:static "System.Threading.Tasks.Task" "Run"
                  (dotnet:make-delegate "System.Func`1[System.Int32]" (lambda () 42))))
  42)

(deftest dotnet-generic-static-inferred.list-convert-all-delegate
  (let ((l (dotnet:new "System.Collections.Generic.List`1[System.Int32]"))
        (conv (dotnet:make-delegate "System.Converter`2[System.Int32,System.String]"
                                    (lambda (a) (format nil "~D" a)))))
    (dotnet:invoke l "Add" 1)
    (dotnet:invoke l "Add" 2)
    (let ((r (dotnet:invoke l "ConvertAll" conv)))
      (list (dotnet:invoke r "get_Count")
            (dotnet:invoke r "get_Item" 0)
            (dotnet:invoke r "get_Item" 1))))
  (2 "1" "2"))

(deftest dotnet-generic-static-inferred.tuple-create-int-string
  (let ((x (dotnet:static "System.Tuple" "Create" 1 "a")))
    (list (dotnet:invoke x "get_Item1")
          (dotnet:invoke x "get_Item2")))
  (1 "a"))

(deftest dotnet-generic-static-inferred.task-from-result-int
  (dotnet:await (dotnet:static "System.Threading.Tasks.Task" "FromResult" 7))
  7)

(deftest dotnet-generic-static-inferred.task-from-result-string
  (dotnet:await (dotnet:static "System.Threading.Tasks.Task" "FromResult" "hi"))
  "hi")

(deftest dotnet-generic-static-inferred.nil-still-asks
  (let ((m (dgsi-error (lambda () (dotnet:static "System.Threading.Tasks.Task" "FromResult" nil)))))
    (list (and m (search "dotnet:static-generic" m) t)
          (and m (search "FromResult" m) t)))
  (t t))

(deftest dotnet-generic-static-inferred.list-still-asks
  (let ((m (dgsi-error (lambda () (dotnet:static "System.Threading.Tasks.Task" "FromResult" '(1))))))
    (list (and m (search "dotnet:static-generic" m) t)
          (and m (search "Lisp list" m) t)))
  (t t))

(deftest dotnet-generic-static-inferred.integer-outside-int32-still-asks
  (let ((m (dgsi-error (lambda () (dotnet:static "System.Threading.Tasks.Task" "FromResult"
                                                 3000000000)))))
    (and m (search "dotnet:static-generic" m) t))
  t)

(deftest dotnet-generic-static-inferred.instance-ambiguous-names-invoke-generic
  (let* ((l (dotnet:new "System.Collections.Generic.List`1[System.Int32]"))
         (m (dgsi-error (lambda () (dotnet:invoke l "ConvertAll" 3)))))
    (and m (search "dotnet:invoke-generic" m) t))
  t)

(deftest dotnet-generic-static-inferred.explicit-static-generic-unchanged
  (dotnet:await
   (dotnet:static-generic "System.Threading.Tasks.Task" "FromResult" '("System.Int32") 5))
  5)

(deftest dotnet-generic-static-inferred.ordinary-instance-call-unchanged
  (let ((l (dotnet:new "System.Collections.Generic.List`1[System.Int32]")))
    (dotnet:invoke l "Add" 4)
    (dotnet:invoke l "get_Item" 0))
  4)
