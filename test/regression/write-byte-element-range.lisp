;;; WRITE-BYTE / WRITE-SEQUENCE to a binary stream signal a TYPE-ERROR for an
;;; integer the stream's element cannot hold.
;;;
;;; Bug: the integer was cut to the low bits of the element (256 to a byte
;;; stream wrote 0, -1 wrote #xFF), and a non-integer was written as 0, with no
;;; error. That held for files opened with an integer element type and for the
;;; bivalent streams DOTNET:TO-STREAM :BIVALENT hands out. SBCL 2.6.8 signals a
;;; TYPE-ERROR whose expected type is the stored element type: (unsigned-byte 7)
;;; is stored in a byte and reported as (UNSIGNED-BYTE 8).

(defvar *wber-path* (regression-temp-file "write-byte-element-range.bin"))

(defun %wber-try (element-type writer)
  "Open a fresh file with ELEMENT-TYPE, call WRITER on it, and return :OK, or
the TYPE-ERROR's datum and expected type."
  (unwind-protect
       (with-open-file (s *wber-path* :direction :output :if-exists :supersede
                                      :element-type element-type)
         (handler-case (progn (funcall writer s) (finish-output s) :ok)
           (type-error (e) (list (type-error-datum e) (type-error-expected-type e)))))
    (ignore-errors (delete-file *wber-path*))))

(defun %wber-byte (element-type value)
  (%wber-try element-type (lambda (s) (write-byte value s))))

(deftest write-byte-element-range.ub8-in-range
  (list (%wber-byte '(unsigned-byte 8) 0) (%wber-byte '(unsigned-byte 8) 255))
  (:ok :ok))

(deftest write-byte-element-range.ub8-out-of-range
  (list (%wber-byte '(unsigned-byte 8) 256) (%wber-byte '(unsigned-byte 8) -1))
  ((256 (unsigned-byte 8)) (-1 (unsigned-byte 8))))

(deftest write-byte-element-range.not-an-integer
  (%wber-byte '(unsigned-byte 8) 1.0)
  (1.0 (unsigned-byte 8)))

(deftest write-byte-element-range.sb8
  (list (%wber-byte '(signed-byte 8) -128) (%wber-byte '(signed-byte 8) 127)
        (%wber-byte '(signed-byte 8) 128) (%wber-byte '(signed-byte 8) -129))
  (:ok :ok (128 (signed-byte 8)) (-129 (signed-byte 8))))

(deftest write-byte-element-range.wider
  (list (%wber-byte '(unsigned-byte 16) 65535) (%wber-byte '(unsigned-byte 16) 65536)
        (%wber-byte '(signed-byte 32) -1) (%wber-byte '(signed-byte 32) #x80000000)
        (%wber-byte '(unsigned-byte 64) (1- (expt 2 64)))
        (%wber-byte '(unsigned-byte 64) (expt 2 64))
        (%wber-byte '(signed-byte 64) (- (expt 2 63)))
        (%wber-byte '(signed-byte 64) (expt 2 63)))
  (:ok (65536 (unsigned-byte 16)) :ok (#x80000000 (signed-byte 32))
   :ok (#.(expt 2 64) (unsigned-byte 64)) :ok (#.(expt 2 63) (signed-byte 64))))

(deftest write-byte-element-range.rounded-up-size
  ;; (unsigned-byte 7) is stored in a byte: 255 fits, 256 does not
  (list (%wber-byte '(unsigned-byte 7) 255) (%wber-byte '(unsigned-byte 7) 256))
  (:ok (256 (unsigned-byte 8))))

(deftest write-byte-element-range.nothing-written-on-error
  (unwind-protect
       (progn
         (with-open-file (s *wber-path* :direction :output :if-exists :supersede
                                        :element-type '(unsigned-byte 8))
           (write-byte 1 s)
           (handler-case (write-byte 300 s) (type-error () nil))
           (write-byte 2 s))
         (with-open-file (in *wber-path* :element-type '(unsigned-byte 8))
           (loop for b = (read-byte in nil nil) while b collect b)))
    (ignore-errors (delete-file *wber-path*)))
  (1 2))

(deftest write-byte-element-range.write-sequence
  (list (%wber-try '(unsigned-byte 8) (lambda (s) (write-sequence '(1 300) s)))
        (%wber-try '(unsigned-byte 8) (lambda (s) (write-sequence (vector 1 -1) s)))
        (%wber-try '(signed-byte 8)
                   (lambda (s)
                     (write-sequence (make-array 1 :element-type '(unsigned-byte 8)
                                                   :initial-element 200)
                                     s))))
  ((300 (unsigned-byte 8)) (-1 (unsigned-byte 8)) (200 (signed-byte 8))))

;;; A bivalent stream: integers go out as single octets.

(defun %wber-bivalent (writer)
  "Call WRITER on a bivalent server stream; return :OK with the octets the
binary peer received, or the TYPE-ERROR's datum and expected type."
  (let* ((addr (dotnet:static "System.Net.IPAddress" "Parse" "127.0.0.1"))
         (listener (dotnet:new "System.Net.Sockets.TcpListener" addr 0)))
    (dotnet:invoke listener "Start")
    (let* ((port (dotnet:invoke (dotnet:invoke listener "LocalEndpoint") "Port"))
           (client (dotnet:new "System.Net.Sockets.TcpClient" "127.0.0.1" port))
           (server (dotnet:invoke listener "AcceptTcpClient")))
      (setf (dotnet:invoke client "ReceiveTimeout") 3000)
      (dotnet:invoke listener "Stop")
      (let ((s (dotnet:to-stream (dotnet:invoke server "GetStream") :bivalent t))
            (c (dotnet:to-stream (dotnet:invoke client "GetStream") :binary t)))
        (unwind-protect
             (let ((r (handler-case (progn (funcall writer s) :ok)
                        (type-error (e) (list (type-error-datum e)
                                              (type-error-expected-type e))))))
               (close s)
               (list r (loop for b = (read-byte c nil nil) while b collect b)))
          (close s) (close c))))))

(deftest write-byte-element-range.bivalent
  (list (%wber-bivalent (lambda (s) (write-byte 255 s) (write-byte 0 s)))
        (%wber-bivalent (lambda (s) (write-byte 7 s) (write-byte 256 s)))
        (%wber-bivalent (lambda (s) (write-byte -1 s)))
        (%wber-bivalent (lambda (s) (write-byte #\a s))))
  ((:ok (255 0))
   ((256 (unsigned-byte 8)) (7))
   ((-1 (unsigned-byte 8)) ())
   ((#\a (unsigned-byte 8)) ())))
