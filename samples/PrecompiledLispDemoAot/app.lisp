;;;; app.lisp — the application's Lisp code, precompiled to a stable-named .NET
;;;; IL assembly (appfasl.dll, internal assembly name "appfasl") at build time.
;;;; At publish time the NativeAOT compiler bakes that assembly into the native
;;;; image; at run time the host invokes its CompiledModule.ModuleInit directly
;;;; (no Assembly.LoadFrom, no code generation). This source is never read or
;;;; eval'd by the shipped binary. host-log is a host C# function exposed via
;;;; DotclHost.Register.

(defun fib (n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

(defun sum-squares (n)
  (let ((acc 0))
    (dotimes (i n) (incf acc (* i i)))
    acc))

(defun greet (name)
  ;; Lisp -> C#: call back into a function the host registered.
  (host-log (format nil "hello ~a, from precompiled Lisp" name))
  (length name))
