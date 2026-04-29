;;; dotcl-socket.lisp — TCP socket support for dotcl
;;;
;;; Usage: (require "dotcl-socket")
;;;
;;; Provides DOTCL-SOCKET package with:
;;;   make-server-socket host port &key backlog → server-socket (LispDotNetObject)
;;;   socket-accept server-socket → bidirectional-stream
;;;   local-port server-socket → integer
;;;   socket-close socket-or-server-socket → t
;;;   socket-connect host port → bidirectional-stream
;;;   socket-stream socket → bidirectional-stream

(defpackage :dotcl-socket
  (:use :cl)
  (:export #:make-server-socket
           #:socket-accept
           #:local-port
           #:socket-close
           #:socket-connect
           #:socket-stream))

(in-package :dotcl-socket)

(defun make-server-socket (host port &key (backlog 5))
  "Create a TCP server socket, bind to HOST:PORT, and listen.
   Returns a .NET TcpListener object. Use LOCAL-PORT to get the actual port
   (useful when PORT is 0 for auto-assignment)."
  (let* ((addr (cond
                 ((or (string= host "0.0.0.0") (string= host ""))
                  (dotnet:static "System.Net.IPAddress" "Parse" "0.0.0.0"))
                 ((or (string= host "127.0.0.1") (string= host "localhost"))
                  (dotnet:static "System.Net.IPAddress" "Parse" "127.0.0.1"))
                 (t (dotnet:static "System.Net.IPAddress" "Parse" host))))
         (listener (dotnet:new "System.Net.Sockets.TcpListener" addr port)))
    (dotnet:invoke listener "Start" backlog)
    listener))

(defun socket-accept (server-socket &key binary)
  "Accept a connection on SERVER-SOCKET.
   Returns a bidirectional CL stream for the accepted connection.
   When BINARY is true, returns a binary stream for byte-level I/O."
  (let* ((client (dotnet:invoke server-socket "AcceptTcpClient"))
         (net-stream (dotnet:invoke client "GetStream")))
    (if binary
        (dotnet:to-stream net-stream :binary t)
        (dotnet:to-stream net-stream))))

(defun local-port (server-socket)
  "Return the local port number of SERVER-SOCKET."
  (let ((endpoint (dotnet:invoke server-socket "LocalEndpoint")))
    (dotnet:invoke endpoint "Port")))

(defun socket-close (socket)
  "Close a socket or server socket."
  (handler-case
    (dotnet:invoke socket "Stop")
    (error ()
      (handler-case
        (dotnet:invoke socket "Close")
        (error () nil))))
  t)

(defun socket-connect (host port &key binary)
  "Connect to HOST:PORT and return a bidirectional CL stream.
   When BINARY is true, returns a binary stream for byte-level I/O."
  (let* ((client (dotnet:new "System.Net.Sockets.TcpClient" host port))
         (net-stream (dotnet:invoke client "GetStream")))
    (if binary
        (dotnet:to-stream net-stream :binary t)
        (dotnet:to-stream net-stream))))

(defun socket-stream (socket)
  "Get a bidirectional CL stream from a raw socket object (TcpClient)."
  (let ((net-stream (dotnet:invoke socket "GetStream")))
    (dotnet:to-stream net-stream)))

(provide "dotcl-socket")
