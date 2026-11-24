;;; A .NET method that itself fails inside reflection (IQueryable execution)
;;; reports the real failure, not "Exception has been thrown by the target of
;;; an invocation".

(deftest dotnet-nested-invocation.queryable-first-empty
  (let ((m (handler-case
               (dotnet:static "System.Linq.Queryable" "First"
                              (dotnet:static "System.Linq.Queryable" "AsQueryable"
                                             (dotnet:new-array "System.Int32")))
             (error (c) (princ-to-string c)))))
    (and (search "no elements" m)
         (not (search "target of an invocation" m))
         (search "Queryable.First" m)
         t))
  t)
