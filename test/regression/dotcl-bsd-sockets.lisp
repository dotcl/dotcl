;;; The dotcl-bsd-sockets contrib: TCP and UDP over loopback, name resolution,
;;; the wildcard addresses, timeouts and the error conditions. Also the
;;; dotcl-socket stream layer, which is built on it.
;;;
;;; "0.0.0.0" and "::" are IP literals and must never reach the resolver:
;;; System.Net.Dns rejects both, so passing every host string through it made
;;; a server listening on "0.0.0.0" impossible.
;;;
;;; Every blocking read below has a receive timeout, so a regression fails the
;;; test instead of hanging the suite.

(require "dotcl-bsd-sockets")
(require "dotcl-socket")

(defun %dns-port (socket)
  (nth-value 1 (dotcl-bsd-sockets:socket-name socket)))

(defun %dns-pair (listen-host connect-host)
  "A listening socket on LISTEN-HOST, a client connected to it through
CONNECT-HOST, and the accepted server side."
  (let* ((server (dotcl-bsd-sockets:tcp-listen listen-host 0))
         (client (dotcl-bsd-sockets:tcp-connect connect-host (%dns-port server) :timeout 5))
         (accepted (dotcl-bsd-sockets:socket-accept server)))
    (setf (dotcl-bsd-sockets:sockopt-receive-timeout client) 3
          (dotcl-bsd-sockets:sockopt-receive-timeout accepted) 3)
    (values server client accepted)))

(defun %dns-close (&rest sockets)
  (mapc #'dotcl-bsd-sockets:socket-close sockets))

(defun %dns-tcp-round-trip (listen-host connect-host)
  (multiple-value-bind (server client accepted) (%dns-pair listen-host connect-host)
    (unwind-protect
         (let ((c (dotcl-bsd-sockets:socket-make-stream client))
               (s (dotcl-bsd-sockets:socket-make-stream accepted)))
           (format c "ping~%")
           (finish-output c)
           (let ((line (read-line s)))
             (write-line "pong" s)
             (finish-output s)
             (list line (read-line c))))
      (%dns-close client accepted server))))

(deftest dotcl-bsd-sockets.tcp-round-trip-loopback
  (%dns-tcp-round-trip "127.0.0.1" "127.0.0.1")
  ("ping" "pong"))

(deftest dotcl-bsd-sockets.listen-on-0.0.0.0
  (%dns-tcp-round-trip "0.0.0.0" "127.0.0.1")
  ("ping" "pong"))

(deftest dotcl-bsd-sockets.listen-on-empty-host
  (%dns-tcp-round-trip "" "127.0.0.1")
  ("ping" "pong"))

(deftest dotcl-bsd-sockets.connect-by-name
  ;; localhost may resolve to ::1 first; the IPv4 listener is still reached
  (%dns-tcp-round-trip "127.0.0.1" "localhost")
  ("ping" "pong"))

(deftest dotcl-bsd-sockets.wildcard-socket-name
  (let ((s (dotcl-bsd-sockets:tcp-listen "0.0.0.0" 0)))
    (unwind-protect
         (multiple-value-bind (address port) (dotcl-bsd-sockets:socket-name s)
           (list (coerce address 'list) (typep s 'dotcl-bsd-sockets:inet-socket) (plusp port)))
      (%dns-close s)))
  ((0 0 0 0) t t))

(deftest dotcl-bsd-sockets.peer-names
  (multiple-value-bind (server client accepted) (%dns-pair "127.0.0.1" "127.0.0.1")
    (unwind-protect
         (list (coerce (dotcl-bsd-sockets:socket-peername client) 'list)
               (= (nth-value 1 (dotcl-bsd-sockets:socket-peername client)) (%dns-port server))
               (= (nth-value 1 (dotcl-bsd-sockets:socket-peername accepted))
                  (nth-value 1 (dotcl-bsd-sockets:socket-name client))))
      (%dns-close client accepted server)))
  ((127 0 0 1) t t))

(deftest dotcl-bsd-sockets.binary-stream
  (multiple-value-bind (server client accepted) (%dns-pair "127.0.0.1" "127.0.0.1")
    (unwind-protect
         (let ((c (dotcl-bsd-sockets:socket-make-stream client :element-type '(unsigned-byte 8)))
               (s (dotcl-bsd-sockets:socket-make-stream accepted :element-type '(unsigned-byte 8)))
               (in (make-array 3 :element-type '(unsigned-byte 8))))
           (write-sequence #(1 2 255) c)
           (finish-output c)
           (read-sequence in s)
           (coerce in 'list))
      (%dns-close client accepted server)))
  (1 2 255))

(deftest dotcl-bsd-sockets.close-ends-connection
  (multiple-value-bind (server client accepted) (%dns-pair "127.0.0.1" "127.0.0.1")
    (unwind-protect
         (progn
           (dotcl-bsd-sockets:socket-close accepted)
           (list (read-char (dotcl-bsd-sockets:socket-make-stream client) nil :eof)
                 (dotcl-bsd-sockets:socket-open-p accepted)
                 ;; closing twice is harmless
                 (dotcl-bsd-sockets:socket-close accepted)))
      (%dns-close client server)))
  (:eof nil t))

(deftest dotcl-bsd-sockets.get-host-by-name-literals
  ;; literals are returned without a lookup, the wildcards included
  (mapcar (lambda (h)
            (coerce (dotcl-bsd-sockets:host-ent-address (dotcl-bsd-sockets:get-host-by-name h)) 'list))
          '("0.0.0.0" "127.0.0.1" "::" "::1"))
  ((0 0 0 0) (127 0 0 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))

(deftest dotcl-bsd-sockets.get-host-by-name-localhost
  (and (member #(127 0 0 1)
               (dotcl-bsd-sockets:host-ent-addresses
                (dotcl-bsd-sockets:get-host-by-name "localhost"))
               :test #'equalp)
       t)
  t)

(deftest dotcl-bsd-sockets.host-not-found
  (handler-case (dotcl-bsd-sockets:get-host-by-name "no-such-host.invalid")
    (dotcl-bsd-sockets:host-not-found-error (e)
      (dotcl-bsd-sockets:name-service-error-host e)))
  "no-such-host.invalid")

(deftest dotcl-bsd-sockets.connect-host-not-found
  (handler-case (dotcl-bsd-sockets:tcp-connect "no-such-host.invalid" 80)
    (dotcl-bsd-sockets:name-service-error () :not-found))
  :not-found)

(deftest dotcl-bsd-sockets.get-host-name
  (let ((name (dotcl-bsd-sockets:get-host-name)))
    (and (stringp name) (plusp (length name))))
  t)

(deftest dotcl-bsd-sockets.connection-refused
  ;; a port that was just free: listen on 0, note the port, close
  (let* ((s (dotcl-bsd-sockets:tcp-listen "127.0.0.1" 0))
         (port (%dns-port s)))
    (dotcl-bsd-sockets:socket-close s)
    (handler-case (progn (dotcl-bsd-sockets:tcp-connect "127.0.0.1" port :timeout 5) :connected)
      (dotcl-bsd-sockets:connection-refused-error (e)
        (list (typep e 'dotcl-bsd-sockets:socket-error)
              (dotcl-bsd-sockets:socket-error-code e)))))
  (t 10061))

(deftest dotcl-bsd-sockets.address-in-use
  (let ((s (dotcl-bsd-sockets:tcp-listen "127.0.0.1" 0)))
    (unwind-protect
         (handler-case (progn (dotcl-bsd-sockets:tcp-listen "127.0.0.1" (%dns-port s)) :listening)
           (dotcl-bsd-sockets:address-in-use-error () :in-use))
      (%dns-close s)))
  :in-use)

(deftest dotcl-bsd-sockets.closed-socket
  (let ((s (dotcl-bsd-sockets:tcp-listen "127.0.0.1" 0)))
    (dotcl-bsd-sockets:socket-close s)
    (handler-case (dotcl-bsd-sockets:socket-accept s)
      (dotcl-bsd-sockets:bad-file-descriptor-error () :closed)))
  :closed)

(deftest dotcl-bsd-sockets.stream-read-timeout
  ;; the stream raises a .NET exception; SOCKET-ERROR-FROM-EXCEPTION names it
  (multiple-value-bind (server client accepted) (%dns-pair "127.0.0.1" "127.0.0.1")
    (unwind-protect
         (progn
           (setf (dotcl-bsd-sockets:sockopt-receive-timeout client) 0.3)
           (list (dotcl-bsd-sockets:sockopt-receive-timeout client)
                 (handler-case (read-char (dotcl-bsd-sockets:socket-make-stream client))
                   (error (e)
                     (type-of (dotcl-bsd-sockets:socket-error-from-exception e client))))))
      (%dns-close client accepted server)))
  (3/10 dotcl-bsd-sockets:operation-timeout-error))

(deftest dotcl-bsd-sockets.timeout-option-off
  (let ((s (dotcl-bsd-sockets:udp-open :local-host "127.0.0.1")))
    (unwind-protect
         (list (dotcl-bsd-sockets:sockopt-send-timeout s)
               (setf (dotcl-bsd-sockets:sockopt-send-timeout s) 2)
               (dotcl-bsd-sockets:sockopt-send-timeout s)
               (setf (dotcl-bsd-sockets:sockopt-send-timeout s) nil)
               (dotcl-bsd-sockets:sockopt-send-timeout s))
      (%dns-close s)))
  (nil 2 2 nil nil))

(deftest dotcl-bsd-sockets.flag-options
  (multiple-value-bind (server client accepted) (%dns-pair "127.0.0.1" "127.0.0.1")
    (let ((u (dotcl-bsd-sockets:udp-open))
          (fresh (make-instance 'dotcl-bsd-sockets:inet-socket :type :stream)))
      (unwind-protect
           (list (progn (setf (dotcl-bsd-sockets:sockopt-tcp-nodelay client) t)
                        (dotcl-bsd-sockets:sockopt-tcp-nodelay client))
                 (progn (setf (dotcl-bsd-sockets:sockopt-keep-alive client) t)
                        (dotcl-bsd-sockets:sockopt-keep-alive client))
                 (progn (setf (dotcl-bsd-sockets:sockopt-keep-alive client) nil)
                        (dotcl-bsd-sockets:sockopt-keep-alive client))
                 (progn (setf (dotcl-bsd-sockets:sockopt-broadcast u) t)
                        (dotcl-bsd-sockets:sockopt-broadcast u))
                 (progn (setf (dotcl-bsd-sockets:sockopt-reuse-address fresh) t)
                        (dotcl-bsd-sockets:sockopt-reuse-address fresh)))
        (%dns-close u fresh client accepted server))))
  (t t nil t t))

(deftest dotcl-bsd-sockets.udp-round-trip
  (let* ((a (dotcl-bsd-sockets:udp-open :local-host "127.0.0.1"))
         (b (dotcl-bsd-sockets:udp-open :host "127.0.0.1" :port (%dns-port a))))
    (setf (dotcl-bsd-sockets:sockopt-receive-timeout a) 3
          (dotcl-bsd-sockets:sockopt-receive-timeout b) 3)
    (unwind-protect
         (let ((sent (dotcl-bsd-sockets:socket-send b #(1 2 3) 3)))
           (multiple-value-bind (buffer n address port)
               (dotcl-bsd-sockets:socket-receive a nil 100)
             (let ((reply (dotcl-bsd-sockets:socket-send a (list 7 8 9 10) 2 :start 1
                                                     :address (list address port)))
                   (in (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
               (list sent (coerce buffer 'list) n (coerce address 'list) (= port (%dns-port b))
                     reply (coerce (subseq (dotcl-bsd-sockets:socket-receive b in nil) 0 2) 'list)))))
      (%dns-close a b)))
  (3 (1 2 3) 3 (127 0 0 1) t 2 (8 9)))

(deftest dotcl-bsd-sockets.udp-receive-timeout
  (let ((s (dotcl-bsd-sockets:udp-open :local-host "127.0.0.1")))
    (setf (dotcl-bsd-sockets:sockopt-receive-timeout s) 0.2)
    (unwind-protect
         (handler-case (dotcl-bsd-sockets:socket-receive s nil 10)
           (dotcl-bsd-sockets:operation-timeout-error () :timeout))
      (%dns-close s)))
  :timeout)

(deftest dotcl-bsd-sockets.wait-for-input
  (multiple-value-bind (server client accepted) (%dns-pair "127.0.0.1" "127.0.0.1")
    (unwind-protect
         (let ((before (dotcl-bsd-sockets:wait-for-input (list client accepted) :timeout 0.1)))
           (write-char #\x (dotcl-bsd-sockets:socket-make-stream client))
           (finish-output (dotcl-bsd-sockets:socket-make-stream client))
           (list before
                 (eq (first (dotcl-bsd-sockets:wait-for-input (list client accepted) :timeout 3))
                     accepted)))
      (%dns-close client accepted server)))
  (nil t))

(deftest dotcl-bsd-sockets.wait-for-input-listener
  (let* ((server (dotcl-bsd-sockets:tcp-listen "127.0.0.1" 0))
         (idle (dotcl-bsd-sockets:wait-for-input server :timeout 0.1))
         (client (dotcl-bsd-sockets:tcp-connect "127.0.0.1" (%dns-port server))))
    (unwind-protect
         (list idle (equal (dotcl-bsd-sockets:wait-for-input server :timeout 3) (list server)))
      (%dns-close client server)))
  (nil t))

(deftest dotcl-socket.round-trip-on-0.0.0.0
  (let* ((server (dotcl-socket:make-server-socket "0.0.0.0" 0))
         (c (dotcl-socket:socket-connect "127.0.0.1" (dotcl-socket:local-port server)))
         (s (dotcl-socket:socket-accept server)))
    (unwind-protect
         (progn
           (write-line "hi" c)
           (finish-output c)
           (prog1 (read-line s)
             (close s)))
      (close c)
      (dotcl-socket:socket-close server)))
  "hi")
