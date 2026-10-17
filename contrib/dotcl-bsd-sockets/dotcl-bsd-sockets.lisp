;;; dotcl-bsd-sockets.lisp: BSD-style sockets for dotcl over System.Net.Sockets
;;;
;;; Usage: (require "dotcl-bsd-sockets")
;;;
;;; The shape follows SBCL's sb-bsd-sockets, so that a portable library's SBCL
;;; code path is a good guide to what to call here: socket objects
;;; (INET-SOCKET, INET6-SOCKET), SOCKET-BIND / SOCKET-LISTEN / SOCKET-ACCEPT /
;;; SOCKET-CONNECT / SOCKET-CLOSE, SOCKET-MAKE-STREAM, SOCKET-SEND /
;;; SOCKET-RECEIVE for datagrams, SOCKOPT-* accessors, GET-HOST-BY-NAME /
;;; GET-HOST-BY-ADDRESS returning a HOST-ENT, and a condition per error kind.
;;;
;;; On top of that, TCP-CONNECT, TCP-LISTEN and UDP-OPEN do the whole job a
;;; socket library usually needs in one call (resolve, pick the address family,
;;; bind, connect with a timeout, listen), and WAIT-FOR-INPUT waits for any of
;;; several sockets to become readable.
;;;
;;; Addresses come back as octet vectors (4 octets for IPv4, 16 for IPv6; an
;;; IPv4 peer seen through a dual-stack socket comes back as 4 octets).
;;; Wherever an address is taken, any of these is accepted: such a vector, a
;;; string holding an IP literal ("127.0.0.1", "::1", "0.0.0.0", "::"), a
;;; host name to resolve, "" for the wildcard address, an integer (an IPv4
;;; address in host byte order) or a System.Net.IPAddress. IP literals are
;;; never sent to the resolver, so the wildcard addresses work everywhere.
;;;
;;; Errors are signalled as SOCKET-ERROR subclasses (or NAME-SERVICE-ERROR
;;; subclasses for name resolution), classified by the SocketError code, which
;;; is the same on every OS. SOCKET-ERROR-FROM-EXCEPTION does that translation
;;; for a .NET exception raised elsewhere, such as by a read on a socket stream
;;; whose receive timeout expired.

(defpackage :dotcl-bsd-sockets
  (:use :cl)
  (:export
   ;; socket objects
   #:socket #:inet-socket #:inet6-socket
   #:socket-handle #:socket-type #:socket-family #:socket-open-p
   #:socket-bind #:socket-listen #:socket-accept #:socket-connect
   #:socket-close #:socket-shutdown
   #:socket-name #:socket-peername
   #:socket-make-stream #:socket-send #:socket-receive
   ;; one-call helpers
   #:tcp-connect #:tcp-listen #:udp-open
   #:wait-for-input
   ;; options
   #:sockopt-reuse-address #:sockopt-keep-alive #:sockopt-tcp-nodelay
   #:sockopt-broadcast #:sockopt-receive-timeout #:sockopt-send-timeout
   ;; names
   #:host-ent #:host-ent-name #:host-ent-addresses #:host-ent-address
   #:get-host-by-name #:get-host-by-address #:get-host-name
   ;; conditions
   #:socket-error #:socket-error-code #:socket-error-socket
   #:interrupted-error #:bad-file-descriptor-error
   #:operation-not-permitted-error #:invalid-argument-error
   #:protocol-not-supported-error #:socket-type-not-supported-error
   #:operation-not-supported-error #:address-in-use-error
   #:address-not-available-error #:network-down-error
   #:network-unreachable-error #:network-reset-error
   #:connection-aborted-error #:connection-reset-error #:no-buffers-error
   #:not-connected-error #:already-shutdown-error #:operation-timeout-error
   #:connection-refused-error #:host-down-error #:host-unreachable-error
   #:name-service-error #:name-service-error-host #:name-service-error-code
   #:host-not-found-error #:try-again-error #:no-recovery-error
   #:socket-error-from-exception))

(in-package :dotcl-bsd-sockets)

;;; ------------------------------------------------------------------
;;; Conditions

(define-condition socket-error (error)
  ((code :initarg :code :initform nil :reader socket-error-code
         :documentation "The System.Net.Sockets.SocketError value, as an integer.")
   (socket :initarg :socket :initform nil :reader socket-error-socket)
   (message :initarg :message :initform nil :reader %socket-error-message))
  (:report (lambda (c s)
             (format s "Socket error~@[ ~a~]~@[ on ~a~]~@[: ~a~]"
                     (socket-error-code c) (socket-error-socket c)
                     (%socket-error-message c)))))

(defmacro %define-socket-errors (&rest names)
  `(progn
     ,@(loop for n in names collect `(define-condition ,n (socket-error) ()))))

(%define-socket-errors
 interrupted-error bad-file-descriptor-error operation-not-permitted-error
 invalid-argument-error protocol-not-supported-error
 socket-type-not-supported-error operation-not-supported-error
 address-in-use-error address-not-available-error network-down-error
 network-unreachable-error network-reset-error connection-aborted-error
 connection-reset-error no-buffers-error not-connected-error
 already-shutdown-error operation-timeout-error connection-refused-error
 host-down-error host-unreachable-error)

(define-condition name-service-error (error)
  ((host :initarg :host :initform nil :reader name-service-error-host)
   (code :initarg :code :initform nil :reader name-service-error-code)
   (message :initarg :message :initform nil :reader %name-service-error-message))
  (:report (lambda (c s)
             (format s "Name service error~@[ ~a~]~@[ for ~s~]~@[: ~a~]"
                     (name-service-error-code c) (name-service-error-host c)
                     (%name-service-error-message c)))))

(define-condition host-not-found-error (name-service-error) ())
(define-condition try-again-error (name-service-error) ())
(define-condition no-recovery-error (name-service-error) ())

;;; Keyed on SocketException.SocketErrorCode, whose values are the Winsock
;;; numbers on every OS (ErrorCode / NativeErrorCode are the OS errno and
;;; differ between Linux and macOS). The message text is never consulted:
;;; .NET localises it.
(defparameter *error-classes*
  '((10004 . interrupted-error)               ; Interrupted
    (10009 . bad-file-descriptor-error)       ; (WSAEBADF)
    (10013 . operation-not-permitted-error)   ; AccessDenied
    (10014 . invalid-argument-error)          ; Fault
    (10022 . invalid-argument-error)          ; InvalidArgument
    (10035 . operation-timeout-error)         ; WouldBlock: a blocking socket only
                                              ; sees it when SO_RCVTIMEO/SO_SNDTIMEO expire
    (10043 . protocol-not-supported-error)    ; ProtocolNotSupported
    (10044 . socket-type-not-supported-error) ; SocketNotSupported
    (10045 . operation-not-supported-error)   ; OperationNotSupported
    (10047 . protocol-not-supported-error)    ; AddressFamilyNotSupported
    (10048 . address-in-use-error)            ; AddressAlreadyInUse
    (10049 . address-not-available-error)     ; AddressNotAvailable
    (10050 . network-down-error)              ; NetworkDown
    (10051 . network-unreachable-error)       ; NetworkUnreachable
    (10052 . network-reset-error)             ; NetworkReset
    (10053 . connection-aborted-error)        ; ConnectionAborted
    (10054 . connection-reset-error)          ; ConnectionReset
    (10055 . no-buffers-error)                ; NoBufferSpaceAvailable
    (10057 . not-connected-error)             ; NotConnected
    (10058 . already-shutdown-error)          ; Shutdown
    (10060 . operation-timeout-error)         ; TimedOut
    (10061 . connection-refused-error)        ; ConnectionRefused
    (10064 . host-down-error)                 ; HostDown
    (10065 . host-unreachable-error)          ; HostUnreachable
    (11001 . host-not-found-error)            ; HostNotFound
    (11002 . try-again-error)                 ; TryAgain
    (11003 . no-recovery-error)               ; NoRecovery
    (11004 . host-not-found-error)))          ; NoData

(defun %find-exception (condition type)
  "The first exception of TYPE in CONDITION's InnerException chain. A socket
failure is wrapped in an IOException by NetworkStream and in an
AggregateException by a waited-on task."
  (loop with ex = (ignore-errors (dotnet:exception-object condition))
        repeat 6
        while ex
        do (when (dotnet:is-instance-of ex type)
             (return ex))
           (setf ex (ignore-errors (dotnet:invoke ex "get_InnerException")))))

(defun socket-error-from-exception (condition &optional socket host)
  "The SOCKET-ERROR or NAME-SERVICE-ERROR that CONDITION (a .NET exception
arriving as a Lisp condition) stands for, or NIL when it is not a socket
failure. SOCKET and HOST are recorded in the result."
  (typecase condition
    ((or socket-error name-service-error) condition)
    (t
     (let ((sockex (%find-exception condition "System.Net.Sockets.SocketException")))
       (cond
         (sockex
          (let* ((code (ignore-errors
                        (dotnet:static "System.Convert" "ToInt32"
                                       (dotnet:invoke sockex "get_SocketErrorCode"))))
                 (message (ignore-errors (dotnet:invoke sockex "get_Message")))
                 (class (or (cdr (assoc code *error-classes*)) 'socket-error)))
            (if (subtypep class 'name-service-error)
                (make-condition class :host host :code code :message message)
                (make-condition class :socket socket :code code :message message))))
         ((%find-exception condition "System.ObjectDisposedException")
          (make-condition 'bad-file-descriptor-error
                          :socket socket :message "the socket is closed"))
         (t nil))))))

(defmacro %with-socket-errors ((&optional socket host) &body body)
  "Run BODY, turning a .NET socket exception into the matching condition."
  (let ((c (gensym "C")) (mapped (gensym "MAPPED")))
    `(handler-bind ((error (lambda (,c)
                             (unless (typep ,c '(or socket-error name-service-error))
                               (let ((,mapped (socket-error-from-exception ,c ,socket ,host)))
                                 (when ,mapped (error ,mapped)))))))
       ,@body)))

;;; ------------------------------------------------------------------
;;; Addresses

(defun %ip-any (family)
  (dotnet:static "System.Net.IPAddress" (if (eq family :inet6) "IPv6Any" "Any")))

(defun %ip-family (ip)
  "Address family of a System.Net.IPAddress: :INET or :INET6. Going by the
byte count, not the printed AddressFamily (InterNetwork is a prefix of
InterNetworkV6)."
  (if (= 16 (length (%ip-octets ip))) :inet6 :inet))

(defun %unmap (ip)
  (if (ignore-errors (dotnet:invoke ip "get_IsIPv4MappedToIPv6"))
      (dotnet:invoke ip "MapToIPv4")
      ip))

(defun %ip-octets (ip)
  (let* ((bytes (dotnet:invoke ip "GetAddressBytes"))
         (n (dotnet:invoke bytes "get_Length"))
         (v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n v)
      (setf (aref v i) (aref bytes i)))))

(defun %ip-to-vector (ip)
  (%ip-octets (%unmap ip)))

(defun %parse-literal (string)
  "STRING as a System.Net.IPAddress when it is an IP literal, else NIL."
  (multiple-value-bind (ok ip) (dotnet:call-out "System.Net.IPAddress" "TryParse" string)
    (and ok ip)))

(defun %sort-addresses (ips)
  "IPv4 addresses first, as SBCL's GET-HOST-BY-NAME does; otherwise in the
resolver's order."
  (stable-sort (copy-list ips) (lambda (a b)
                                 (and (eq (%ip-family a) :inet)
                                      (eq (%ip-family b) :inet6)))))

(defun %resolve (name)
  "All addresses of host NAME (a string that is not an IP literal), sorted."
  (%with-socket-errors (nil name)
    (let* ((arr (dotnet:static "System.Net.Dns" "GetHostAddresses" name))
           (ips (loop for i below (dotnet:invoke arr "get_Length")
                      collect (aref arr i))))
      (unless ips
        (error 'host-not-found-error :host name))
      (%sort-addresses ips))))

(defun %addresses (host &optional family)
  "The candidate System.Net.IPAddress values for address designator HOST,
best first. FAMILY (:INET or :INET6) restricts them; the wildcard \"\" or NIL
needs it to choose between 0.0.0.0 and ::."
  (let ((ips
          (typecase host
            (null (list (%ip-any family)))
            (string (if (string= host "")
                        (list (%ip-any family))
                        (let ((ip (%parse-literal host)))
                          (if ip (list ip) (%resolve host)))))
            (vector (list (dotnet:new "System.Net.IPAddress"
                                      (%to-byte-array host 0 (length host)))))
            (integer (list (dotnet:new "System.Net.IPAddress"
                                       (%to-byte-array (list (ldb (byte 8 24) host)
                                                             (ldb (byte 8 16) host)
                                                             (ldb (byte 8 8) host)
                                                             (ldb (byte 8 0) host))
                                                       0 4))))
            (t (if (dotnet:is-instance-of host "System.Net.IPAddress")
                   (list host)
                   (error 'type-error :datum host
                                      :expected-type '(or string vector integer)))))))
    (if family
        (or (remove family ips :key #'%ip-family :test-not #'eq)
            (if (and (stringp host) (null (%parse-literal host)))
                (error 'host-not-found-error :host host)
                (error 'address-not-available-error
                       :message (format nil "~a is not an ~(~a~) address" host family))))
        ips)))

(defun %address (host &optional family)
  (first (%addresses host family)))

(defun %endpoint (ip port)
  (dotnet:new "System.Net.IPEndPoint" ip port))

(defun %endpoint-values (endpoint)
  (if endpoint
      (values (%ip-to-vector (dotnet:invoke endpoint "get_Address"))
              (dotnet:invoke endpoint "get_Port"))
      (values nil nil)))

;;; ------------------------------------------------------------------
;;; Socket objects

(defclass socket ()
  ((handle :initarg :handle :initform nil :reader socket-handle
           :documentation "The System.Net.Sockets.Socket.")
   (type :initarg :type :initform :stream :reader socket-type
         :documentation ":STREAM (TCP) or :DATAGRAM (UDP).")
   (stream :initform nil)
   (open :initform t)))

(defclass inet-socket (socket) ())
(defclass inet6-socket (socket) ())

(defgeneric socket-family (socket)
  (:method ((s inet-socket)) :inet)
  (:method ((s inet6-socket)) :inet6))

(defmethod initialize-instance :after ((s socket) &key protocol &allow-other-keys)
  (declare (ignore protocol))
  (unless (slot-value s 'handle)
    (setf (slot-value s 'handle)
          (%with-socket-errors (s)
            (dotnet:new "System.Net.Sockets.Socket"
                        (dotnet:enum-or "System.Net.Sockets.AddressFamily"
                                        (if (eq (socket-family s) :inet6)
                                            "InterNetworkV6" "InterNetwork"))
                        (dotnet:enum-or "System.Net.Sockets.SocketType"
                                        (if (eq (socket-type s) :datagram) "Dgram" "Stream"))
                        (dotnet:enum-or "System.Net.Sockets.ProtocolType"
                                        (if (eq (socket-type s) :datagram) "Udp" "Tcp")))))))

(defmethod print-object ((s socket) stream)
  (print-unreadable-object (s stream :type t :identity t)
    (format stream "~(~a~)~:[ closed~;~]" (socket-type s) (slot-value s 'open))))

(defun %socket-class (family)
  (if (eq family :inet6) 'inet6-socket 'inet-socket))

(defun %make-socket (family type)
  (make-instance (%socket-class family) :type type))

(defun socket-open-p (socket)
  (slot-value socket 'open))

(defun socket-bind (socket address port)
  "Bind SOCKET to ADDRESS (any address designator) and PORT (0 for any)."
  (%with-socket-errors (socket address)
    (dotnet:invoke (socket-handle socket) "Bind"
                   (%endpoint (%address address (socket-family socket)) (or port 0)))
    socket))

(defun socket-listen (socket backlog)
  (%with-socket-errors (socket)
    (dotnet:invoke (socket-handle socket) "Listen" backlog)
    socket))

(defun socket-accept (socket)
  "Wait for a connection on listening SOCKET. Returns the connected socket,
the peer's address and the peer's port."
  (%with-socket-errors (socket)
    (let* ((h (dotnet:invoke (socket-handle socket) "Accept"))
           (s (make-instance (class-of socket) :handle h :type :stream)))
      (multiple-value-bind (address port) (socket-peername s)
        (values s address port)))))

(defun socket-connect (socket address port &key timeout)
  "Connect SOCKET to ADDRESS and PORT. With TIMEOUT (seconds), give up with an
OPERATION-TIMEOUT-ERROR after that long; the socket is then closed."
  (%with-socket-errors (socket address)
    (let ((ep (%endpoint (%address address (socket-family socket)) port))
          (h (socket-handle socket)))
      (if timeout
          ;; .NET has no blocking connect with a timeout: wait on the task
          (let ((task (dotnet:invoke h "ConnectAsync" ep)))
            (unless (dotnet:invoke task "Wait" (max 0 (round (* 1000 timeout))))
              (socket-close socket)
              (error 'operation-timeout-error :socket socket
                                              :message "connect timed out")))
          (dotnet:invoke h "Connect" ep))
      socket)))

(defun socket-close (socket)
  "Close SOCKET and its stream. Closing again does nothing."
  (when (slot-value socket 'open)
    (setf (slot-value socket 'open) nil)
    (let ((stream (slot-value socket 'stream)))
      (when stream (ignore-errors (close stream))))
    (ignore-errors (dotnet:invoke (socket-handle socket) "Close")))
  t)

(defun socket-shutdown (socket &key (direction :io))
  "Shut down DIRECTION (:INPUT, :OUTPUT or :IO) of SOCKET."
  (%with-socket-errors (socket)
    (dotnet:invoke (socket-handle socket) "Shutdown"
                   (dotnet:enum-or "System.Net.Sockets.SocketShutdown"
                                   (ecase direction
                                     (:input "Receive")
                                     (:output "Send")
                                     (:io "Both"))))
    t))

(defun socket-name (socket)
  "The local address and port of SOCKET, or NIL when it is not bound."
  (%with-socket-errors (socket)
    (%endpoint-values (dotnet:invoke (socket-handle socket) "get_LocalEndPoint"))))

(defun socket-peername (socket)
  "The remote address and port of SOCKET, or NIL when it is not connected."
  (%with-socket-errors (socket)
    (%endpoint-values (dotnet:invoke (socket-handle socket) "get_RemoteEndPoint"))))

(defun %binary-element-type-p (element-type)
  (not (or (null element-type)
           (eq element-type :default)
           (subtypep element-type 'character))))

(defun socket-make-stream (socket &key (element-type :default))
  "A bidirectional Lisp stream over connected SOCKET. ELEMENT-TYPE :DEFAULT or a
character type gives a bivalent stream (characters and octets both work, as
on SBCL); an integer type gives a binary stream. The stream is made once and
returned again on later calls. Closing it closes the socket."
  (or (slot-value socket 'stream)
      (setf (slot-value socket 'stream)
            (%with-socket-errors (socket)
              (let ((ns (dotnet:new "System.Net.Sockets.NetworkStream"
                                    (socket-handle socket) t)))
                (if (%binary-element-type-p element-type)
                    (dotnet:to-stream ns :binary t)
                    (dotnet:to-stream ns :bivalent t)))))))

(defun %to-byte-array (buffer start length)
  (let ((arr (dotnet:static "System.Array" "CreateInstance"
                            (dotnet:resolve-type "System.Byte") length)))
    (etypecase buffer
      (vector (dotimes (i length) (setf (aref arr i) (aref buffer (+ start i)))))
      (list (loop for i below length
                  for x in (nthcdr start buffer)
                  do (setf (aref arr i) x))))
    arr))

(defun socket-send (socket buffer length &key address (start 0))
  "Send LENGTH octets of BUFFER (a vector or list of octets) from START.
ADDRESS, a list (host port), sends a datagram there; without it the socket
must be connected. Returns the number of octets sent."
  (let ((length (or length (- (length buffer) start))))
    (%with-socket-errors (socket (first address))
      (let ((arr (%to-byte-array buffer start length))
            (flags (dotnet:enum-or "System.Net.Sockets.SocketFlags" "None"))
            (h (socket-handle socket)))
        (if address
            (dotnet:invoke h "SendTo" arr flags
                           (%endpoint (%address (first address) (socket-family socket))
                                      (second address)))
            (dotnet:invoke h "Send" arr flags))))))

(defun socket-receive (socket buffer length &key (element-type '(unsigned-byte 8)))
  "Receive up to LENGTH octets (default: the length of BUFFER) into BUFFER, or
into a fresh vector of ELEMENT-TYPE when BUFFER is NIL. Returns the buffer, the
number of octets received, and the sender's address and port."
  (let ((n (or length (length buffer))))
    (%with-socket-errors (socket)
      (let* ((h (socket-handle socket))
             (arr (dotnet:static "System.Array" "CreateInstance"
                                 (dotnet:resolve-type "System.Byte") n)))
        (multiple-value-bind (received remote)
            (if (eq (socket-type socket) :datagram)
                ;; ReceiveFrom reports the sender through a ref EndPoint whose
                ;; initial value has to match the socket's family
                (dotnet:call-out h "ReceiveFrom" arr
                                 (%endpoint (%ip-any (socket-family socket)) 0))
                (values (dotnet:invoke h "Receive" arr
                                       (dotnet:enum-or "System.Net.Sockets.SocketFlags" "None"))
                        (dotnet:invoke h "get_RemoteEndPoint")))
          (let ((out (or buffer (make-array received :element-type element-type))))
            (dotimes (i (min received (length out)))
              (setf (aref out i) (aref arr i)))
            (multiple-value-bind (address port) (%endpoint-values remote)
              (values out received address port))))))))

;;; ------------------------------------------------------------------
;;; Options

(defun %level (name)
  (dotnet:enum-or "System.Net.Sockets.SocketOptionLevel" name))

(defun %option (name)
  (dotnet:enum-or "System.Net.Sockets.SocketOptionName" name))

(defun %get-flag (socket level name)
  (%with-socket-errors (socket)
    (not (eql 0 (dotnet:invoke (socket-handle socket) "GetSocketOption"
                               (%level level) (%option name))))))

(defun %set-flag (socket level name value)
  (%with-socket-errors (socket)
    (dotnet:invoke (socket-handle socket) "SetSocketOption"
                   (%level level) (%option name) (if value 1 0))
    value))

(defun sockopt-reuse-address (socket) (%get-flag socket "Socket" "ReuseAddress"))
(defun (setf sockopt-reuse-address) (value socket)
  (%set-flag socket "Socket" "ReuseAddress" value))

(defun sockopt-keep-alive (socket) (%get-flag socket "Socket" "KeepAlive"))
(defun (setf sockopt-keep-alive) (value socket)
  (%set-flag socket "Socket" "KeepAlive" value))

(defun sockopt-broadcast (socket) (%get-flag socket "Socket" "Broadcast"))
(defun (setf sockopt-broadcast) (value socket)
  (%set-flag socket "Socket" "Broadcast" value))

(defun sockopt-tcp-nodelay (socket) (%get-flag socket "Tcp" "NoDelay"))
(defun (setf sockopt-tcp-nodelay) (value socket)
  (%set-flag socket "Tcp" "NoDelay" value))

(defun %get-timeout (socket property)
  (let ((ms (%with-socket-errors (socket)
              (dotnet:invoke (socket-handle socket) (concatenate 'string "get_" property)))))
    (if (plusp ms) (/ ms 1000) nil)))

(defun %set-timeout (socket property seconds)
  (%with-socket-errors (socket)
    (dotnet:invoke (socket-handle socket) (concatenate 'string "set_" property)
                   ;; 0 means no timeout to .NET, so never round a small one down to it
                   (if (and seconds (plusp seconds))
                       (max 1 (round (* 1000 seconds)))
                       0))
    seconds))

(defun sockopt-receive-timeout (socket)
  "Seconds a blocking receive (including a read on the socket's stream) waits
before failing with OPERATION-TIMEOUT-ERROR, or NIL for no limit."
  (%get-timeout socket "ReceiveTimeout"))
(defun (setf sockopt-receive-timeout) (seconds socket)
  (%set-timeout socket "ReceiveTimeout" seconds))

(defun sockopt-send-timeout (socket)
  "Seconds a blocking send (including a write on the socket's stream) waits
before failing with OPERATION-TIMEOUT-ERROR, or NIL for no limit."
  (%get-timeout socket "SendTimeout"))
(defun (setf sockopt-send-timeout) (seconds socket)
  (%set-timeout socket "SendTimeout" seconds))

;;; ------------------------------------------------------------------
;;; One-call helpers

(defun tcp-connect (host port &key timeout nodelay local-host local-port)
  "A TCP socket connected to HOST and PORT. A host name is resolved and each of
its addresses tried in turn (IPv4 first) until one connects; when none does,
the error from the first one is signalled. TIMEOUT (seconds) bounds the whole
call: once it has passed, OPERATION-TIMEOUT-ERROR is signalled without trying
the remaining addresses. NODELAY sets TCP_NODELAY (:IF-SUPPORTED: set it,
ignoring failure). LOCAL-HOST and LOCAL-PORT bind the local end first."
  (let ((first-error nil)
        (deadline (when timeout
                    (+ (get-internal-real-time)
                       (round (* timeout internal-time-units-per-second))))))
    (dolist (ip (%addresses host))
      (let* ((family (%ip-family ip))
             (s (%make-socket family :stream))
             (remaining (when deadline
                          (max 0 (/ (- deadline (get-internal-real-time))
                                    internal-time-units-per-second)))))
        (handler-case
            (progn
              (when (or local-host local-port)
                (socket-bind s (if local-host (%address local-host family) "") (or local-port 0)))
              (case nodelay
                ((nil))
                (:if-supported (ignore-errors (setf (sockopt-tcp-nodelay s) t)))
                (t (setf (sockopt-tcp-nodelay s) t)))
              (socket-connect s ip port :timeout remaining)
              (return-from tcp-connect s))
          (operation-timeout-error (e)
            (socket-close s)
            (when deadline (error e))
            (setf first-error (or first-error e)))
          (socket-error (e)
            (socket-close s)
            (setf first-error (or first-error e))))))
    (error first-error)))

(defun tcp-listen (host port &key (backlog 5) reuse-address)
  "A TCP socket listening on HOST and PORT (0 for any free port; SOCKET-NAME
tells which). HOST \"0.0.0.0\" or \"\" listens on every IPv4 address, \"::\" on
every IPv6 address; a host name listens on its first address (IPv4 first)."
  (let* ((ip (%address (if (equal host "") "0.0.0.0" host)))
         (s (%make-socket (%ip-family ip) :stream)))
    (handler-bind ((error (lambda (e) (declare (ignore e)) (socket-close s))))
      (when reuse-address
        (setf (sockopt-reuse-address s) t))
      (socket-bind s ip (or port 0))
      (socket-listen s backlog))
    s))

(defun udp-open (&key host port local-host local-port)
  "A UDP socket, always bound (to LOCAL-HOST and LOCAL-PORT, or to the wildcard
address and any free port), and connected to HOST and PORT when both are
given. The address family follows HOST, else LOCAL-HOST, else IPv4."
  (let* ((target (when host (%address host)))
         (family (cond (target (%ip-family target))
                       (local-host (%ip-family (%address local-host)))
                       (t :inet)))
         (s (%make-socket family :datagram)))
    (handler-bind ((error (lambda (e) (declare (ignore e)) (socket-close s))))
      (socket-bind s (if local-host (%address local-host family) "") (or local-port 0))
      (when (and target port)
        (socket-connect s target port)))
    s))

(defun %socket-list ()
  (dotnet:new (dotnet:make-generic-type "System.Collections.Generic.List"
                                        (list (dotnet:resolve-type "System.Net.Sockets.Socket")))))

(defun wait-for-input (sockets &key timeout)
  "Wait until at least one of SOCKETS (a socket or a list) is readable, or
TIMEOUT seconds (NIL: no limit) pass. A listening socket is readable when a
connection is waiting; a socket whose stream already holds input counts as
readable. Returns the list of readable sockets (NIL on timeout). Closed
sockets are skipped."
  (let* ((sockets (remove-if-not #'socket-open-p
                                 (if (listp sockets) sockets (list sockets))))
         (buffered (remove-if-not (lambda (s)
                                    (let ((stream (slot-value s 'stream)))
                                      (and stream (ignore-errors (listen stream)))))
                                  sockets))
         (read-list (%socket-list)))
    (when sockets
      (%with-socket-errors ()
        (dolist (s sockets)
          (dotnet:invoke read-list "Add" (socket-handle s)))
        ;; Select takes microseconds; a negative value waits forever. It
        ;; leaves only the readable sockets in the list.
        (dotnet:static "System.Net.Sockets.Socket" "Select"
                       read-list nil nil
                       (cond (buffered 0)
                             (timeout (max 0 (round (* timeout 1000000))))
                             (t -1)))
        (remove-if-not (lambda (s)
                         (or (member s buffered)
                             (dotnet:invoke read-list "Contains" (socket-handle s))))
                       sockets)))))

;;; ------------------------------------------------------------------
;;; Names

(defstruct (host-ent (:constructor %make-host-ent (name addresses)))
  "A host's name and addresses (octet vectors), as GET-HOST-BY-NAME returns."
  name addresses)

(defun host-ent-address (host-ent)
  "The first address of HOST-ENT."
  (first (host-ent-addresses host-ent)))

(defun get-host-by-name (name)
  "Resolve host NAME. An IP literal is returned as it is, without asking the
resolver. The addresses list IPv4 first. Signals HOST-NOT-FOUND-ERROR (or
another NAME-SERVICE-ERROR) when the name does not resolve."
  (let ((ip (%parse-literal name)))
    (if ip
        (%make-host-ent name (list (%ip-to-vector ip)))
        (%with-socket-errors (nil name)
          (let* ((entry (dotnet:static "System.Net.Dns" "GetHostEntry" name))
                 (arr (dotnet:invoke entry "get_AddressList"))
                 (ips (loop for i below (dotnet:invoke arr "get_Length")
                            collect (aref arr i))))
            (unless ips
              (error 'host-not-found-error :host name))
            (%make-host-ent (dotnet:invoke entry "get_HostName")
                            (mapcar #'%ip-to-vector (%sort-addresses ips))))))))

(defun get-host-by-address (address)
  "Reverse-resolve ADDRESS (an octet vector or IP literal string)."
  (%with-socket-errors (nil address)
    (let* ((ip (%address address))
           (entry (dotnet:static "System.Net.Dns" "GetHostEntry" ip)))
      (%make-host-ent (dotnet:invoke entry "get_HostName")
                      (list (%ip-to-vector ip))))))

(defun get-host-name ()
  "The name of this machine."
  (dotnet:static "System.Net.Dns" "GetHostName"))

(provide "dotcl-bsd-sockets")
