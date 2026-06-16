;;;; app.lisp — the application's Lisp code, precompiled to app.fasl at build
;;;; time. At run time the host loads app.fasl (no code generation) and calls
;;;; these functions; this file's source is never read or eval'd by the shipped
;;;; app. host-log is a host C# function exposed via DotclHost.Register.

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
