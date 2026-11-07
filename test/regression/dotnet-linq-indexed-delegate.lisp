;;; A two-argument Lisp lambda given to LINQ's Where / Select reaches the indexed overload
;;; (Func<T,int,bool>): the iterator classes behind Enumerable.Range carry a Where(Func<T,bool>)
;;; of their own that used to be called with it and failed at call time.

(defun %dlid-list (obj)
  (let ((arr (dotnet:invoke obj "ToArray")) (out '()))
    (dotimes (i (dotnet:invoke arr "Length") (nreverse out))
      (push (dotnet:invoke arr "GetValue" i) out))))

(deftest dotnet-linq-indexed-delegate.where-indexed
  (let ((range (dotnet:static "System.Linq.Enumerable" "Range" 1 5)))
    (list (%dlid-list (dotnet:-> range ("Where" (lambda (x i) (declare (ignore x)) (oddp i)))))
          (%dlid-list (dotnet:-> range ("Where" (lambda (x) (oddp x)))))
          (%dlid-list (dotnet:-> range ("Select" (lambda (x i) (+ x i)))))))
  ((2 4) (1 3 5) (1 3 5 7 9)))
