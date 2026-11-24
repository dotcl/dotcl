;;; A delegate made from a Lisp function has a Method with exactly the delegate's own
;;; parameters, so APIs that read delegate.Method (the minimal-API binder, DI
;;; factories) do not meet a hidden Closure argument.

(defun %dms-method-params (d)
  (mapcar (lambda (p) (dotnet:invoke p "Name"))
          (coerce-params (dotnet:invoke (dotnet:invoke d "Method") "GetParameters"))))

(defun coerce-params (arr)
  (loop for i below (dotnet:invoke arr "Length") collect (aref arr i)))

(deftest-emitting-only delegate-method-shape.func-no-hidden-parameter
  (let ((d (dotnet:make-delegate "System.Func`1[System.String]" (lambda () "x"))))
    (list (%dms-method-params d)
          (dotnet:invoke d "Invoke")))
  (nil "x"))

(deftest-emitting-only delegate-method-shape.parameters-keep-the-delegate-names
  (let ((d (dotnet:make-delegate "System.Func`3[System.Int32,System.String,System.String]"
                                 (lambda (n s) (format nil "~a~a" s n)))))
    (list (%dms-method-params d)
          (dotnet:invoke d "Invoke" 7 "k")))
  (("arg1" "arg2") "k7"))

(deftest-emitting-only delegate-method-shape.void-and-value-types
  (let* ((seen nil)
         (a (dotnet:make-delegate "System.Action`1[System.Int32]" (lambda (n) (setq seen n))))
         (f (dotnet:make-delegate "System.Func`2[System.Int32,System.Int32]" (lambda (n) (* n 2)))))
    (dotnet:invoke a "Invoke" 21)
    (list seen (dotnet:invoke f "Invoke" 21)))
  (21 42))

;; Needs the ASP.NET Core shared framework; where it is not installed the test passes
;; without checking anything.
(deftest-emitting-only delegate-method-shape.minimal-api-binder-accepts-it
  (let ((loaded (dolist (a '("Microsoft.AspNetCore.Http.Abstractions" "Microsoft.AspNetCore.Http"
                             "Microsoft.AspNetCore.Http.Extensions" "Microsoft.AspNetCore.Routing"
                             "Microsoft.AspNetCore")
                           t)
                  (handler-case (dotnet:load-assembly a)
                    (error () (return nil))))))
    (if loaded
        (let ((d (dotnet:make-delegate "System.Func`1[System.String]" (lambda () "hi"))))
          ;; RequestDelegateFactory reads d.Method; a hidden Closure parameter made it throw.
          (handler-case
              (progn (dotnet:static "Microsoft.AspNetCore.Http.RequestDelegateFactory" "Create" d) t)
            (error (e)
              (let ((m (princ-to-string e)))
                (if (search "type not found" m) t m)))))
        t))
  t)
