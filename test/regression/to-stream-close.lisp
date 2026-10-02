;;; CLOSE on a stream made by DOTNET:TO-STREAM closes the .NET stream under it.
;;;
;;; Bug: the Lisp stream was closed but the .NET stream stayed open. dotcl-socket's
;;; socket-accept / socket-connect hand back only the Lisp stream, so a program
;;; had no way to end a connection: the peer never saw end of file, and an
;;; HTTP/1.0 response (delimited by the close) never finished.

(defun %tsc-pair (kind)
  (let* ((addr (dotnet:static "System.Net.IPAddress" "Parse" "127.0.0.1"))
         (listener (dotnet:new "System.Net.Sockets.TcpListener" addr 0)))
    (dotnet:invoke listener "Start")
    (let* ((port (dotnet:invoke (dotnet:invoke listener "LocalEndpoint") "Port"))
           (client (dotnet:new "System.Net.Sockets.TcpClient" "127.0.0.1" port))
           (server (dotnet:invoke listener "AcceptTcpClient")))
      ;; a regression shows as a timeout (stream error), not a hung suite
      (setf (dotnet:invoke client "ReceiveTimeout") 3000)
      (dotnet:invoke listener "Stop")
      (values (apply #'dotnet:to-stream (dotnet:invoke server "GetStream")
                     (case kind (:bivalent '(:bivalent t)) (:binary '(:binary t)) (t nil)))
              (dotnet:to-stream (dotnet:invoke client "GetStream") :bivalent t)))))

(defun %tsc-peer-sees (kind)
  "Write one byte from the server side, CLOSE it, and report what the client reads."
  (multiple-value-bind (s c) (%tsc-pair kind)
    (if (eq kind :binary) (write-byte 65 s) (write-char #\A s))
    (close s)
    (handler-case (list (read-char c nil :eof) (read-char c nil :eof))
      (stream-error () :no-eof))))

(deftest to-stream-close.bivalent-ends-connection
  (%tsc-peer-sees :bivalent)
  (#\A :eof))

(deftest to-stream-close.character-ends-connection
  (%tsc-peer-sees :character)
  (#\A :eof))

(deftest to-stream-close.binary-ends-connection
  (%tsc-peer-sees :binary)
  (#\A :eof))

(deftest to-stream-close.memory-stream-closed
  (let* ((ms (dotnet:new "System.IO.MemoryStream"))
         (s (dotnet:to-stream ms :binary t)))
    (write-byte 1 s)
    (close s)
    (dotnet:invoke ms "CanRead"))
  nil)
