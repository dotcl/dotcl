;;; A socket stream reads what has arrived in one receive and serves READ-BYTE / READ-SEQUENCE
;;; / LISTEN from that buffer. The bytes come out in order across read sizes, LISTEN sees
;;; buffered bytes, and end of file is still reported after the peer closes.

(defun %srb-pair ()
  (let* ((addr (dotnet:static "System.Net.IPAddress" "Parse" "127.0.0.1"))
         (listener (dotnet:new "System.Net.Sockets.TcpListener" addr 0)))
    (dotnet:invoke listener "Start")
    (let* ((port (dotnet:invoke (dotnet:invoke listener "LocalEndpoint") "Port"))
           (client (dotnet:new "System.Net.Sockets.TcpClient" "127.0.0.1" port))
           (server (dotnet:invoke listener "AcceptTcpClient")))
      (setf (dotnet:invoke server "ReceiveTimeout") 3000)
      (dotnet:invoke listener "Stop")
      (values (dotnet:to-stream (dotnet:invoke server "GetStream") :binary t)
              (dotnet:to-stream (dotnet:invoke client "GetStream") :binary t)
              client))))

(deftest socket-read-buffer.bytes-in-order-then-eof
  (multiple-value-bind (s c client) (%srb-pair)
    (prog1
        (handler-case
            (let ((buf (make-array 5 :element-type '(unsigned-byte 8)))
                  (rest (make-array 8 :element-type '(unsigned-byte 8))))
              (write-sequence (map '(vector (unsigned-byte 8)) #'char-code "abcdefghijkl") c)
              (force-output c)
              (sleep 0.2)
              (let* ((b0 (read-byte s))
                     (b1 (read-byte s))
                     (n1 (read-sequence buf s))
                     (n2 (read-sequence rest s :end 5)))
                (write-sequence (map '(vector (unsigned-byte 8)) #'char-code "XY") c)
                (force-output c)
                (sleep 0.2)
                (let ((b2 (read-byte s)) (b3 (read-byte s)))
                  (dotnet:invoke client "Close")
                  (sleep 0.2)
                  (list b0 b1 n1 (coerce buf 'list) n2 (coerce (subseq rest 0 5) 'list)
                        b2 b3 (read-byte s nil :eof)))))
          (stream-error () :hung))
      (ignore-errors (close s))))
  (97 98 5 (99 100 101 102 103) 5 (104 105 106 107 108) 88 89 :eof))
