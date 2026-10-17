;;; Output on a binary DOTNET:TO-STREAM stream is buffered, as SBCL's socket
;;; streams are: it used to send every WRITE-BYTE on its own, so a TLS record
;;; written byte by byte (cl+ssl's Lisp BIO) to a peer that had just closed
;;; failed with a broken pipe on the second byte.

(defun %bbin-pair ()
  (let* ((addr (dotnet:static "System.Net.IPAddress" "Parse" "127.0.0.1"))
         (listener (dotnet:new "System.Net.Sockets.TcpListener" addr 0)))
    (dotnet:invoke listener "Start")
    (let* ((port (dotnet:invoke (dotnet:invoke listener "LocalEndpoint") "Port"))
           (client (dotnet:new "System.Net.Sockets.TcpClient" "127.0.0.1" port))
           (server (dotnet:invoke listener "AcceptTcpClient")))
      (setf (dotnet:invoke client "ReceiveTimeout") 3000)
      (setf (dotnet:invoke server "ReceiveTimeout") 3000)
      (dotnet:invoke listener "Stop")
      (values (dotnet:to-stream (dotnet:invoke server "GetStream") :binary t)
              (dotnet:to-stream (dotnet:invoke client "GetStream") :binary t)
              client))))

(deftest binary-buffered-write.sent-on-force-output
  (multiple-value-bind (s c client) (%bbin-pair)
    (unwind-protect
         (progn
           (write-byte 1 s)
           (write-sequence (make-array 2 :element-type '(unsigned-byte 8)
                                         :initial-contents '(2 3))
                           s)
           (sleep 0.1)
           (list (dotnet:invoke client "Available")
                 (progn (force-output s)
                        (handler-case (loop repeat 3 collect (read-byte c))
                          (stream-error () :timeout)))))
      (close c) (close s)))
  (0 (1 2 3)))

(deftest binary-buffered-write.read-sends-pending-output
  (multiple-value-bind (s c) (%bbin-pair)
    (unwind-protect
         (handler-case
             (progn
               (write-byte 7 s)
               (force-output s)
               (write-byte 8 c)
               (list (read-byte c) (read-byte s)))
           (stream-error () :timeout))
      (close c) (close s)))
  (7 8))

(deftest binary-buffered-write.one-send-after-peer-close
  (multiple-value-bind (s c) (%bbin-pair)
    (close c)
    (sleep 0.2)
    (unwind-protect
         (handler-case
             (progn (write-byte 1 s)
                    (sleep 0.1)
                    (write-byte 2 s)
                    (finish-output s)
                    :ok)
           (stream-error () :broken-pipe))
      (ignore-errors (close s))))
  :ok)
