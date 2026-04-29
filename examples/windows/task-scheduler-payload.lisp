;;;; task-scheduler-payload.lisp
;;;; Launched by Task Scheduler at logon (DotclLogonHello) or on USB plug
;;;; (DotclUsbHello). Speaks a greeting via SAPI and writes a marker line
;;;; so you can verify it actually ran even if audio is muted.

(let ((voice (dotnet:new "SAPI.SpVoice"))
      (log   (format nil "~a/dotcl-task-scheduler-demo.log"
                     (or (dotcl:getenv "TEMP") "C:/Windows/Temp"))))
  (with-open-file (s log :direction :output
                         :if-exists :append :if-does-not-exist :create)
    (format s "~a fired by task scheduler~%"
            (dotnet:static "System.DateTime" "Now")))
  ;; SVSFlagsAsync = 1 — return immediately (process can exit before audio finishes
  ;; if 0 is passed and the process is killed), but we want to wait so use 0.
  (dotnet:invoke voice "Speak" "おはようございます、dotclからのお知らせです" 0))
