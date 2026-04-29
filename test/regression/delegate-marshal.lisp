;;; Regression tests for D891 — LispFunction → Delegate marshal (#188)
;;; dotnet:make-delegate + auto-marshal in dotnet:new

;;; make-delegate returns a non-nil .NET object
(deftest d891-make-delegate-returns-obj
  (not (null (dotnet:make-delegate "System.Action" (lambda () nil))))
  t)

;;; Comparison<int> delegate created from lambda sorts a List<int>
(deftest d891-make-delegate-sort
  (let* ((lst (dotnet:new "System.Collections.Generic.List`1[System.Int32]"))
         (cmp (dotnet:make-delegate
                 "System.Comparison`1[System.Int32]"
                 (lambda (a b)
                   (cond ((< a b) -1) ((> a b) 1) (t 0))))))
    (dotnet:invoke lst "Add" 3)
    (dotnet:invoke lst "Add" 1)
    (dotnet:invoke lst "Add" 2)
    (dotnet:invoke lst "Sort" cmp)
    (list (dotnet:invoke lst "get_Item" 0)
          (dotnet:invoke lst "get_Item" 1)
          (dotnet:invoke lst "get_Item" 2)))
  (1 2 3))

;;; Action<int>: void delegate with one int arg is invokable
(deftest d891-make-delegate-action-void
  (let* ((result 0)
         (action (dotnet:make-delegate
                    "System.Action`1[System.Int32]"
                    (lambda (x) (setf result x)))))
    (dotnet:invoke action "Invoke" 42)
    result)
  42)

;;; Func<int,int>: return value is marshaled back to Lisp
(deftest d891-make-delegate-func-return
  (let* ((fn (dotnet:make-delegate
                "System.Func`2[System.Int32,System.Int32]"
                (lambda (x) (* x 3)))))
    (dotnet:invoke fn "Invoke" 7))
  21)

;;; auto-marshal in dotnet:new: Thread ctor takes ThreadStart (void() delegate)
(deftest d891-auto-marshal-new-thread
  (not (null (dotnet:new "System.Threading.Thread"
                         (lambda () nil))))
  t)

;;; make-delegate errors on non-delegate type
(deftest d891-make-delegate-non-delegate-error
  (signals-error
    (dotnet:make-delegate "System.Int32" (lambda () nil))
    error)
  t)
