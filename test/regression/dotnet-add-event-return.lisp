;;; DOTNET:ADD-EVENT returns the handler that it subscribed, so a caller can
;;; pass that value straight to DOTNET:REMOVE-EVENT. It used to return NIL.
;;; BackgroundWorker.ReportProgress raises ProgressChanged synchronously, so
;;; the detach is observed rather than assumed.

(defun %event-worker ()
  (let ((worker (dotnet:new "System.ComponentModel.BackgroundWorker")))
    (dotnet:invoke worker "set_WorkerReportsProgress" t)
    worker))

(deftest dotnet-add-event.returns-handler
  (let* ((worker (%event-worker))
         (handler (lambda (sender args) (declare (ignore sender args)) nil))
         (returned (dotnet:add-event worker "ProgressChanged" handler)))
    (eq returned handler))
  t)

(deftest dotnet-add-event.returned-handler-removes
  (let* ((worker (%event-worker))
         (calls 0)
         (handler (lambda (sender args)
                    (declare (ignore sender args))
                    (incf calls)))
         (returned (dotnet:add-event worker "ProgressChanged" handler)))
    (dotnet:invoke worker "ReportProgress" 50)
    (dotnet:remove-event worker "ProgressChanged" returned)
    (dotnet:invoke worker "ReportProgress" 50)
    calls)
  1)
