;;; An I/O failure on a stream must signal STREAM-ERROR (with the stream in
;;; stream-error-stream), not PROGRAM-ERROR. The portable peer-disconnect idiom
;;; is (handler-case (read-char s) (stream-error () ...)); before the fix the
;;; raw IOException surfaced as PROGRAM-ERROR and the handler never fired.
;;;
;;; Deterministic setup: build a loopback TCP pair, prime the server-side
;;; stream's reader/writer, then Dispose the underlying NetworkStream so every
;;; subsequent read/write fails like a dead peer — no RST timing, no platform
;;; variance (an actual reset can surface as clean EOF on some platforms).

(defun %dead-socket-stream ()
  (let* ((addr (dotnet:static "System.Net.IPAddress" "Parse" "127.0.0.1"))
         (listener (dotnet:new "System.Net.Sockets.TcpListener" addr 0)))
    (dotnet:invoke listener "Start")
    (let* ((port (dotnet:invoke (dotnet:invoke listener "LocalEndpoint") "Port"))
           (client (dotnet:new "System.Net.Sockets.TcpClient" "127.0.0.1" port))
           (server (dotnet:invoke listener "AcceptTcpClient"))
           (net (dotnet:invoke server "GetStream"))
           (s (dotnet:to-stream net :bivalent t))
           (cs (dotnet:to-stream (dotnet:invoke client "GetStream") :bivalent t)))
      ;; Prime the CL stream's reader and writer while the socket is healthy,
      ;; so the failure hits the I/O call itself, not lazy reader creation.
      (write-char #\x cs)
      (force-output cs)
      (read-char s)
      (write-char #\y s)
      (force-output s)
      (dotnet:invoke net "Dispose")
      (dotnet:invoke listener "Stop")
      s)))

(deftest dead-socket-read-signals-stream-error
  (let ((s (%dead-socket-stream)))
    (handler-case (progn (read-char s nil :eof) :no-error)
      (stream-error (c) (list :stream-error (eq (stream-error-stream c) s)))))
  (:stream-error t))

(deftest dead-socket-read-line-signals-stream-error
  (let ((s (%dead-socket-stream)))
    (handler-case (progn (read-line s nil :eof) :no-error)
      (stream-error () :stream-error)))
  :stream-error)

(deftest dead-socket-write-signals-stream-error
  (let ((s (%dead-socket-stream)))
    (handler-case (progn (write-char #\z s) (force-output s) :no-error)
      (stream-error () :stream-error)))
  :stream-error)

;;; The original CLR exception rides on the condition, so a handler can still
;;; classify the failure (dotnet:exception-object → error codes, inner chain).
(deftest dead-socket-error-carries-exception-object
  (let ((s (%dead-socket-stream)))
    (handler-case (progn (read-char s nil :eof) :no-error)
      (stream-error (c) (notnot (dotnet:exception-object c)))))
  t)

;;; A closed stream is a stream-error for FILE-POSITION, matching read/write
;;; (which already signalled) and matching SBCL. FILE-LENGTH deliberately does
;;; NOT signal: SBCL answers it on a closed stream too, so portable code that
;;; asks for the length after closing keeps working.
(defun %closed-file-stream (direction)
  (with-open-file (s "cfs-probe.tmp" :direction :output :if-exists :supersede)
    (write-char #\x s))
  (let ((s (open "cfs-probe.tmp" :direction direction
                                 :if-exists :append)))
    (close s)
    s))

(deftest closed-stream-file-position-signals
  (handler-case (progn (file-position (%closed-file-stream :input)) :no-error)
    (stream-error () :stream-error))
  :stream-error)

(deftest closed-stream-file-position-carries-stream
  (let ((s (%closed-file-stream :input)))
    (handler-case (progn (file-position s) :no-error)
      (stream-error (c) (eq (stream-error-stream c) s))))
  t)

(deftest closed-stream-file-length-does-not-signal
  (prog1 (handler-case (progn (file-length (%closed-file-stream :input)) :no-error)
           (stream-error () :stream-error))
    (when (probe-file "cfs-probe.tmp") (delete-file "cfs-probe.tmp")))
  :no-error)
