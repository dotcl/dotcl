;;; Output on a bivalent socket stream is buffered and goes out as one piece.
;;;
;;; Bug: every WRITE-BYTE, WRITE-CHAR and WRITE-SEQUENCE on a
;;; DOTNET:TO-STREAM :BIVALENT stream was its own send (WRITE-SEQUENCE of a byte
;;; vector even went byte by byte), so one reply left in many TCP segments. A
;;; client that splits its reads where the segments fall then read differently:
;;; AllegroServe's chunked reader returned a stale byte when a chunk header
;;; ended a segment.
;;;
;;; Buffered as SBCL's socket streams are. Output also goes out before the same
;;; stream reads, so code that writes a request and reads the reply without
;;; FORCE-OUTPUT still works.

(defun %bbw-pair ()
  (let* ((addr (dotnet:static "System.Net.IPAddress" "Parse" "127.0.0.1"))
         (listener (dotnet:new "System.Net.Sockets.TcpListener" addr 0)))
    (dotnet:invoke listener "Start")
    (let* ((port (dotnet:invoke (dotnet:invoke listener "LocalEndpoint") "Port"))
           (client (dotnet:new "System.Net.Sockets.TcpClient" "127.0.0.1" port))
           (server (dotnet:invoke listener "AcceptTcpClient")))
      (setf (dotnet:invoke client "ReceiveTimeout") 3000)
      (setf (dotnet:invoke server "ReceiveTimeout") 3000)
      (dotnet:invoke listener "Stop")
      (values (dotnet:to-stream (dotnet:invoke server "GetStream") :bivalent t)
              (dotnet:to-stream (dotnet:invoke client "GetStream") :bivalent t)))))

(deftest bivalent-buffered-write.mixed-order
  ;; chars, a byte, and a byte vector interleave in the order written
  (multiple-value-bind (s c) (%bbw-pair)
    (unwind-protect
         (progn
           (write-string "ab" c)
           (write-byte 67 c)
           (write-sequence (make-array 2 :element-type '(unsigned-byte 8)
                                         :initial-contents '(68 69))
                           c :start 1)
           (write-char #\f c)
           (force-output c)
           (handler-case (loop repeat 5 collect (read-char s))
             (stream-error () :timeout)))
      (close c) (close s)))
  (#\a #\b #\C #\E #\f))

(deftest bivalent-buffered-write.read-sends-pending-output
  ;; the client writes without FORCE-OUTPUT and then reads: its request still
  ;; reaches the server
  (multiple-value-bind (s c) (%bbw-pair)
    (unwind-protect
         (handler-case
             (progn
               (write-char #\y s)
               (force-output s)
               (write-char #\x c)          ; no force-output
               (list (read-char c)         ; sends #\x before it reads
                     (read-char s)))
           (stream-error () :timeout))
      (close c) (close s)))
  (#\y #\x))

;;; The writer's buffer is also sent by the paired reader before it reads, and
;;; the reader usually runs in another thread than the writer (one thread reads
;;; the next request while another writes a reply). The two used to copy into
;;; and send the same buffer at once, and the lines below reached the peer
;;; garbled: most of them out of order, missing or repeated. Here one thread
;;; reads a long run of characters from the stream while another writes
;;; numbered lines to it.
(require "dotcl-thread")

(deftest bivalent-buffered-write.reader-and-writer-threads
  (multiple-value-bind (s c) (%bbw-pair)
    (unwind-protect
         (let* ((n-lines 5000)
                (n-chars 50000)
                (feeder (dotcl-thread:make-thread
                         (lambda ()
                           (dotimes (i n-chars)
                             (write-char #\x s)
                             (when (zerop (mod i 100)) (force-output s)))
                           (force-output s))))
                (reader (dotcl-thread:make-thread
                         (lambda ()
                           (handler-case (dotimes (i n-chars) (read-char c))
                             (stream-error () nil)))))
                (writer (dotcl-thread:make-thread
                         (lambda ()
                           (dotimes (i n-lines) (format c "~6,'0d~%" i))
                           (force-output c))))
                (lines (handler-case (loop repeat n-lines collect (read-line s))
                         (stream-error () nil))))
           (dotcl-thread:thread-join writer)
           (dotcl-thread:thread-join reader)
           (dotcl-thread:thread-join feeder)
           (list (length lines)
                 (loop for l in lines for i from 0
                       count (not (equal l (format nil "~6,'0d" i))))))
      (close c) (close s)))
  (5000 0))
