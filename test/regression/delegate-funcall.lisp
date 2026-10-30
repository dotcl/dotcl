;;; A .NET delegate is a function designator: funcall, apply and the mapping
;;; functions call it, marshalling arguments to its parameter types.

(deftest delegate-funcall-func
  (let ((d (dotnet:make-delegate "System.Func`2[System.Int32,System.Int32]"
                                 (lambda (x) (* x 2)))))
    (list (funcall d 21) (apply d '(4)) (mapcar d '(1 2 3))))
  (42 8 (2 4 6)))

(deftest delegate-funcall-void-returns-nil
  (let* ((seen nil)
         (d (dotnet:make-delegate "System.Action`1[System.String]"
                                  (lambda (s) (setf seen s)))))
    (list (funcall d "hi") seen))
  (nil "hi"))

(deftest delegate-funcall-arity-error
  (let ((d (dotnet:make-delegate "System.Func`2[System.Int32,System.Int32]" #'1+)))
    (handler-case (progn (funcall d 1 2) :no-error)
      (error () :error)))
  :error)
