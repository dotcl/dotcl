;;; Regression tests for dotnet:static-generic: generic static methods

;;; Enumerable.Where<int> — filter list with lambda predicate
(deftest d895-linq-where-basic
  (let* ((lst (dotnet:new "System.Collections.Generic.List`1[System.Int32]"))
         (pred (dotnet:make-delegate "System.Func`2[System.Int32,System.Boolean]"
                                     (lambda (x) (> x 3)))))
    (dotnet:invoke lst "Add" 1)
    (dotnet:invoke lst "Add" 3)
    (dotnet:invoke lst "Add" 5)
    (dotnet:invoke lst "Add" 7)
    (let* ((filtered (dotnet:static-generic "System.Linq.Enumerable" "Where"
                                             '("System.Int32") lst pred))
           (result (dotnet:static-generic "System.Linq.Enumerable" "ToList"
                                           '("System.Int32") filtered)))
      (list (dotnet:invoke result "get_Count")
            (dotnet:invoke result "get_Item" 0)
            (dotnet:invoke result "get_Item" 1))))
  (2 5 7))

;;; Enumerable.Select<int,int> — transform
(deftest d895-linq-select
  (let* ((lst (dotnet:new "System.Collections.Generic.List`1[System.Int32]"))
         (sel (dotnet:make-delegate "System.Func`2[System.Int32,System.Int32]"
                                    (lambda (x) (* x 2)))))
    (dotnet:invoke lst "Add" 1)
    (dotnet:invoke lst "Add" 2)
    (dotnet:invoke lst "Add" 3)
    (let* ((mapped (dotnet:static-generic "System.Linq.Enumerable" "Select"
                                           '("System.Int32" "System.Int32") lst sel))
           (result (dotnet:static-generic "System.Linq.Enumerable" "ToList"
                                           '("System.Int32") mapped)))
      (list (dotnet:invoke result "get_Item" 0)
            (dotnet:invoke result "get_Item" 1)
            (dotnet:invoke result "get_Item" 2))))
  (2 4 6))

;;; Enumerable.Count<int>
(deftest d895-linq-count
  (let* ((lst (dotnet:new "System.Collections.Generic.List`1[System.Int32]")))
    (dotnet:invoke lst "Add" 10)
    (dotnet:invoke lst "Add" 20)
    (dotnet:invoke lst "Add" 30)
    (dotnet:static-generic "System.Linq.Enumerable" "Count" '("System.Int32") lst))
  3)

;;; Error: wrong type arg count
(deftest d895-wrong-type-arg-count-error
  (signals-error
    (dotnet:static-generic "System.Linq.Enumerable" "Where" '() nil nil)
    error)
  t)

;;; dotnet:invoke-generic: generic INSTANCE methods (dotcl/dotcl#23).
;;; List<int>.ConvertAll<string>(Converter<int,string>) via a Lisp delegate.
(deftest d1122-invoke-generic-convertall
  (let ((lst (dotnet:new "System.Collections.Generic.List`1[System.Int32]")))
    (dotnet:invoke lst "Add" 5)
    (dotnet:invoke lst "Add" 10)
    (let* ((conv (dotnet:make-delegate "System.Converter`2[System.Int32,System.String]"
                                       (lambda (x) (format nil "n~a" x))))
           (res (dotnet:invoke-generic lst "ConvertAll" '("System.String") conv)))
      (list (dotnet:invoke res "get_Item" 0)
            (dotnet:invoke res "get_Item" 1))))
  ("n5" "n10"))

;;; Error: no matching generic instance method.
(deftest d1122-invoke-generic-no-method-error
  (signals-error
    (dotnet:invoke-generic (dotnet:new "System.Object") "NoSuch" '("System.Int32") 1)
    error)
  t)

(deftest d1122-invoke-generic-has-doc
  (and (stringp (documentation 'dotnet:invoke-generic 'function)) t)
  t)
