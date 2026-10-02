;;; LISTEN and READ-CHAR-NO-HANG on a socket stream answer at once when nothing
;;; has arrived yet: LISTEN is NIL, READ-CHAR-NO-HANG is NIL (not end of file).
;;;
;;; Bug: both asked the reader to Peek, and on a socket Peek waits for the peer.
;;; Code that drains "what has arrived so far" with a LISTEN loop hung on the
;;; first request (AllegroServe's buffered socket streams, via acl-compat).
;;;
;;; The server side gets a receive timeout, so a regression fails the test with
;;; a stream error instead of hanging the suite.

(defun %sln-pair ()
  (let* ((addr (dotnet:static "System.Net.IPAddress" "Parse" "127.0.0.1"))
         (listener (dotnet:new "System.Net.Sockets.TcpListener" addr 0)))
    (dotnet:invoke listener "Start")
    (let* ((port (dotnet:invoke (dotnet:invoke listener "LocalEndpoint") "Port"))
           (client (dotnet:new "System.Net.Sockets.TcpClient" "127.0.0.1" port))
           (server (dotnet:invoke listener "AcceptTcpClient")))
      (setf (dotnet:invoke server "ReceiveTimeout") 3000)
      (dotnet:invoke listener "Stop")
      (values (dotnet:to-stream (dotnet:invoke server "GetStream") :bivalent t)
              (dotnet:to-stream (dotnet:invoke client "GetStream") :bivalent t)
              client))))

(deftest socket-listen-no-hang
  (multiple-value-bind (s c client) (%sln-pair)
    (write-string "ab" c)
    (force-output c)
    (sleep 0.2)
    (prog1
        (handler-case
            (list (listen s)
                  (read-char s) (read-char s)
                  (listen s)
                  (read-char-no-hang s nil :eof)
                  (progn (dotnet:invoke client "Close")
                         (sleep 0.2)
                         (read-char-no-hang s nil :eof)))
          (stream-error () :hung))
      (ignore-errors (close s))))
  (t #\a #\b nil nil :eof))
