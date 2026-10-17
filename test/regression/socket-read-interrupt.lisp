;;; A thread blocked reading from a socket stream made by DOTNET:TO-STREAM is
;;; reached by INTERRUPT-THREAD (and so by timeouts built on it), as on SBCL.

(defun %sri-pair (&rest to-stream-args)
  (let* ((addr (dotnet:static "System.Net.IPAddress" "Parse" "127.0.0.1"))
         (listener (dotnet:new "System.Net.Sockets.TcpListener" addr 0)))
    (dotnet:invoke listener "Start")
    (let* ((port (dotnet:invoke (dotnet:invoke listener "LocalEndpoint") "Port"))
           (client (dotnet:new "System.Net.Sockets.TcpClient" "127.0.0.1" port))
           (server (dotnet:invoke listener "AcceptTcpClient")))
      (dotnet:invoke listener "Stop")
      (values (apply #'dotnet:to-stream (dotnet:invoke server "GetStream") to-stream-args)
              (apply #'dotnet:to-stream (dotnet:invoke client "GetStream") to-stream-args)
              client))))

(defun %sri-interrupt-reader (read &rest to-stream-args)
  (multiple-value-bind (s c) (apply #'%sri-pair to-stream-args)
    (let* ((result :hang)
           (th (dotcl:make-thread
                (lambda ()
                  (setf result (catch 'stop (funcall read c) :read-returned))))))
      (unwind-protect
           (progn
             (sleep 0.3)
             (dotcl:interrupt-thread th (lambda () (throw 'stop :stopped)))
             (loop repeat 50 while (eq result :hang) do (sleep 0.1))
             result)
        (close s)
        (ignore-errors (close c))
        (dotcl:thread-join th)))))

(deftest socket-read-interrupt.binary
  (%sri-interrupt-reader #'read-byte :binary t)
  :stopped)

(deftest socket-read-interrupt.bivalent
  (%sri-interrupt-reader #'read-char :bivalent t)
  :stopped)

(deftest socket-read-interrupt.character
  (%sri-interrupt-reader #'read-line)
  :stopped)

;; An interrupt function that returns lets the read go on waiting.
(deftest socket-read-interrupt.read-resumes
  (multiple-value-bind (s c) (%sri-pair :binary t)
    (let* ((ran nil)
           (result :hang)
           (th (dotcl:make-thread (lambda () (setf result (read-byte c))))))
      (unwind-protect
           (progn
             (sleep 0.3)
             (dotcl:interrupt-thread th (lambda () (setf ran t)))
             (sleep 0.3)
             (write-byte 42 s)
             (force-output s)
             (loop repeat 50 while (eq result :hang) do (sleep 0.1))
             (list ran result))
        (close s)
        (ignore-errors (close c))
        (dotcl:thread-join th))))
  (t 42))

(deftest socket-read-interrupt.receive-timeout-still-applies
  (multiple-value-bind (s c client) (%sri-pair :binary t)
    (setf (dotnet:invoke client "ReceiveTimeout") 300)
    (unwind-protect
         (handler-case (read-byte c)
           (stream-error () :timeout))
      (close s)
      (close c)))
  :timeout)

;; Values returned by an interrupt function run during the wait are not the
;; read's values.
(deftest socket-read-interrupt.interrupt-values-do-not-leak
  (multiple-value-bind (s c) (%sri-pair :binary t)
    (let* ((result :hang)
           (th (dotcl:make-thread
                (lambda () (setf result (multiple-value-list (read-byte c)))))))
      (unwind-protect
           (progn
             (sleep 0.3)
             (dotcl:interrupt-thread th (lambda () (values 7 8 9)))
             (sleep 0.3)
             (write-byte 42 s)
             (force-output s)
             (loop repeat 50 while (eq result :hang) do (sleep 0.1))
             result)
        (close s)
        (ignore-errors (close c))
        (dotcl:thread-join th))))
  (42))

;; Bytes read from the socket behind the stream's back do not leave the stream
;; believing a read can not block.
(deftest socket-read-interrupt.after-raw-read
  (multiple-value-bind (s c client) (%sri-pair :binary t)
    (write-sequence (make-array 10 :element-type '(unsigned-byte 8) :initial-element 1) s)
    (force-output s)
    (sleep 0.2)
    (let ((first (read-byte c))
          (raw (dotnet:invoke (dotnet:invoke client "GetStream") "Read"
                              (dotnet:new "System.Byte[]" 64) 0 64))
          (result :hang))
      (let ((th (dotcl:make-thread
                 (lambda () (setf result (catch 'stop (read-byte c) :read-returned))))))
        (unwind-protect
             (progn
               (sleep 0.3)
               (dotcl:interrupt-thread th (lambda () (throw 'stop :stopped)))
               (loop repeat 50 while (eq result :hang) do (sleep 0.1))
               (list first raw result))
          (close s)
          (ignore-errors (close c))
          (dotcl:thread-join th)))))
  (1 9 :stopped))
