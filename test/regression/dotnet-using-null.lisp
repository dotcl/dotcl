;;; dotnet:using skips a NIL resource, and a body error wins over Dispose.

(require "dotnet-class")

(defun %dun-incomplete-task ()
  (dotnet:invoke (dotnet:new "System.Threading.Tasks.TaskCompletionSource`1[System.Int32]")
                 "get_Task"))

(defun %dun-closed-p (stream)
  (handler-case (progn (dotnet:invoke stream "get_Length") nil)
    (error () t)))

(deftest dotnet-using-null.nil-resource
  (dotnet:using ((s nil)) 42)
  42)

(deftest dotnet-using-null.null-from-call
  (dotnet:using ((s (dotnet:static "System.Type" "GetType" "DotclUsing.NoSuchType"))) 5)
  5)

(deftest dotnet-using-null.body-values-kept
  (multiple-value-list (dotnet:using ((s nil)) (values 1 2)))
  (1 2))

(deftest dotnet-using-null.nested-one-nil
  (let ((a (dotnet:new "System.IO.MemoryStream"))
        (b (dotnet:new "System.IO.MemoryStream")))
    (dotnet:using ((x a) (y nil) (z b)) 7)
    (list (%dun-closed-p a) (%dun-closed-p b)))
  (t t))

(deftest dotnet-using-null.body-error-wins-over-dispose
  (handler-case
      (dotnet:using ((task (%dun-incomplete-task))) (error "body failed"))
    (error (e) (princ-to-string e)))
  "body failed")

(deftest dotnet-using-null.body-error-with-good-dispose
  (handler-case
      (dotnet:using ((stream (dotnet:new "System.IO.MemoryStream"))) (error "body failed"))
    (error (e) (princ-to-string e)))
  "body failed")

(deftest dotnet-using-null.dispose-error-propagates
  (handler-case
      (progn (dotnet:using ((task (%dun-incomplete-task))) 1) :no-error)
    (error (e) (and (search "completion state" (princ-to-string e)) t)))
  t)

(deftest dotnet-using-null.nonlocal-exit-disposes
  (let ((stream (dotnet:new "System.IO.MemoryStream")))
    (catch 'exit (dotnet:using ((s stream)) (throw 'exit 5)))
    (%dun-closed-p stream))
  t)
