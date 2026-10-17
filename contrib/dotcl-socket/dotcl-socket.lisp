;;; dotcl-socket.lisp: TCP sockets as Lisp streams, for dotcl
;;;
;;; Usage: (require "dotcl-socket")
;;;
;;; A small stream-oriented layer over the dotcl-bsd-sockets contrib, kept for
;;; the code that calls it (SLIME's and SLY's dotcl backends, the Quicklisp
;;; client).
;;;
;;; Provides DOTCL-SOCKET package with:
;;;   make-server-socket host port &key backlog -> server socket
;;;   socket-accept server-socket &key binary -> bidirectional stream
;;;   local-port server-socket -> integer
;;;   socket-close socket-or-server-socket -> t
;;;   socket-connect host port &key binary -> bidirectional stream
;;;   socket-stream socket -> bidirectional stream

(require "dotcl-bsd-sockets")

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
  "Create a TCP server socket listening on HOST:PORT. HOST \"0.0.0.0\" or \"\"
listens on every IPv4 address. Use LOCAL-PORT to get the actual port (useful
when PORT is 0 for auto-assignment)."
  (dotcl-bsd-sockets:tcp-listen host port :backlog backlog))

(defun %stream (socket binary)
  (dotcl-bsd-sockets:socket-make-stream
   socket :element-type (if binary '(unsigned-byte 8) :default)))

(defun socket-accept (server-socket &key binary)
  "Accept a connection on SERVER-SOCKET.
   Returns a bidirectional CL stream for the accepted connection. The non-binary
   stream is bivalent (char + byte I/O), as SBCL's socket streams are, so byte-oriented
   protocol code (read-byte/write-byte) works on it without :binary.
   When BINARY is true, returns a pure binary stream for byte-level I/O.
   Closing the stream closes the connection."
  (%stream (dotcl-bsd-sockets:socket-accept server-socket) binary))

(defun local-port (server-socket)
  "Return the local port number of SERVER-SOCKET."
  (nth-value 1 (dotcl-bsd-sockets:socket-name server-socket)))

(defun socket-close (socket)
  "Close a socket, a server socket, or a stream this package returned."
  (if (typep socket 'dotcl-bsd-sockets:socket)
      (dotcl-bsd-sockets:socket-close socket)
      (ignore-errors (close socket)))
  t)

(defun socket-connect (host port &key binary)
  "Connect to HOST:PORT and return a bidirectional CL stream.
   When BINARY is true, returns a binary stream for byte-level I/O.
   Closing the stream closes the connection."
  (%stream (dotcl-bsd-sockets:tcp-connect host port) binary))

(defun socket-stream (socket)
  "Get a bidirectional (bivalent: char + byte) CL stream from a connected socket."
  (%stream socket nil))

(provide "dotcl-socket")
