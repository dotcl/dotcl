;;; WRITE-SEQUENCE to a bivalent stream writes integers as octets.
;;;
;;; Bug: on a DOTNET:TO-STREAM :BIVALENT stream (what DOTCL-SOCKET hands out),
;;; WRITE-SEQUENCE of an (UNSIGNED-BYTE 8) vector took the character path and
;;; wrote each octet as a character in UTF-8, so #xE6 #x97 #xA5 reached the peer
;;; as C3 A6 C2 97 C2 A5. WRITE-BYTE one at a time and READ-SEQUENCE were right.
;;;
;;; Each element is now written as what it is, as SBCL's bivalent streams do: a
;;; character as a character, an integer as one octet (an integer outside
;;; (UNSIGNED-BYTE 8) is a TYPE-ERROR). The peer reads through a :BINARY stream
;;; on the raw socket, so what it sees is the bytes on the wire.

(defun %bwsb-pair ()
  "A bivalent server stream and a :BINARY client stream on one TCP connection."
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
              (dotnet:to-stream (dotnet:invoke client "GetStream") :binary t)))))

(defun %bwsb-wire (writer)
  "Call WRITER on the bivalent end, close it, and return the octets the
binary end received, as a list (or :ERROR with the condition's type)."
  (multiple-value-bind (s c) (%bwsb-pair)
    (unwind-protect
         (handler-case
             (progn
               (funcall writer s)
               (close s)
               (loop for b = (read-byte c nil nil) while b collect b))
           (error (e) (list :error (type-of e))))
      (close s) (close c))))

(defun %bwsb-ub8 (&rest octets)
  (make-array (length octets) :element-type '(unsigned-byte 8) :initial-contents octets))

(deftest bivalent-write-sequence-bytes.ub8-vector
  (%bwsb-wire (lambda (s) (write-sequence (%bwsb-ub8 #xE6 #x97 #xA5) s)))
  (#xE6 #x97 #xA5))

(deftest bivalent-write-sequence-bytes.start-end
  (%bwsb-wire (lambda (s) (write-sequence (%bwsb-ub8 #xE6 #x97 #xA5 #x80) s :start 1 :end 3)))
  (#x97 #xA5))

(deftest bivalent-write-sequence-bytes.echo-request
  ;; the issue's scenario: read the request as octets, reply with octets
  (multiple-value-bind (s c) (%bwsb-pair)
    (unwind-protect
         (handler-case
             (progn
               (write-sequence (%bwsb-ub8 #xE6 #x97 #xA5 #xE6 #x9C #xAC) c)
               (force-output c)
               (let* ((buf (make-array 6 :element-type '(unsigned-byte 8)))
                      (n (read-sequence buf s)))
                 (write-sequence buf s :end n)
                 (force-output s)
                 (loop repeat n collect (read-byte c))))
           (error (e) (list :error (type-of e))))
      (close s) (close c)))
  (#xE6 #x97 #xA5 #xE6 #x9C #xAC))

(deftest bivalent-write-sequence-bytes.simple-vector-of-integers
  (%bwsb-wire (lambda (s) (write-sequence (vector #xE6 #x97 #xA5) s)))
  (#xE6 #x97 #xA5))

(deftest bivalent-write-sequence-bytes.wider-integer-vectors
  (%bwsb-wire (lambda (s)
                (write-sequence (make-array 2 :element-type '(unsigned-byte 16)
                                              :initial-contents '(#x80 #xFF))
                                s)
                (write-sequence (make-array 2 :element-type 'fixnum
                                              :initial-contents '(200 5))
                                s)))
  (#x80 #xFF 200 5))

(deftest bivalent-write-sequence-bytes.list-of-integers
  (%bwsb-wire (lambda (s) (write-sequence (list #xE6 #x97 #xA5) s :start 1)))
  (#x97 #xA5))

(deftest bivalent-write-sequence-bytes.characters-stay-characters
  ;; a string, a vector of characters and a list of characters are UTF-8 text
  (%bwsb-wire (lambda (s)
                (write-sequence (coerce (list #\a (code-char #x65E5)) 'string) s)
                (write-sequence (vector #\b (code-char #x65E5)) s)
                (write-sequence (make-array 2 :element-type 'character
                                              :initial-contents (list #\c (code-char #x65E5)))
                                s :start 1)
                (write-sequence (list #\d (code-char #x65E5)) s :end 1)))
  (#x61 #xE6 #x97 #xA5 #x62 #xE6 #x97 #xA5 #xE6 #x97 #xA5 #x64))

(deftest bivalent-write-sequence-bytes.mixed-in-order
  (%bwsb-wire (lambda (s) (write-sequence (vector #\a #xE6 (code-char #x65E5) #x97) s)))
  (#x61 #xE6 #xE6 #x97 #xA5 #x97))

(deftest bivalent-write-sequence-bytes.displaced-and-fill-pointer
  (%bwsb-wire (lambda (s)
                (let ((base (%bwsb-ub8 1 #xE6 #x97 #xA5)))
                  (write-sequence (make-array 3 :element-type '(unsigned-byte 8)
                                                :displaced-to base :displaced-index-offset 1)
                                  s))
                (write-sequence (make-array 4 :element-type '(unsigned-byte 8)
                                              :initial-contents '(#x80 #x81 #x82 #x83)
                                              :fill-pointer 2 :adjustable t)
                                s)))
  (#xE6 #x97 #xA5 #x80 #x81))

(deftest bivalent-write-sequence-bytes.through-composite-streams
  (%bwsb-wire (lambda (s)
                (write-sequence (%bwsb-ub8 #xE6) (make-two-way-stream (make-string-input-stream "") s))
                (write-sequence (%bwsb-ub8 #x97) (make-broadcast-stream s))
                (let ((*bwsb-out* s))
                  (declare (special *bwsb-out*))
                  (write-sequence (%bwsb-ub8 #xA5) (make-synonym-stream '*bwsb-out*)))))
  (#xE6 #x97 #xA5))

(deftest bivalent-write-sequence-bytes.large-vector
  ;; larger than the writer's buffer: every octet arrives, in order
  (let* ((n (emit-free-scale 100000 20000))
         (v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n) (setf (aref v i) (mod (* i 7) 256)))
    (let ((got (%bwsb-wire (lambda (s) (write-sequence v s)))))
      (list (= (length got) n)
            (loop for b in got for i from 0 count (/= b (aref v i))))))
  (t 0))

(deftest bivalent-write-sequence-bytes.out-of-range-integer
  (multiple-value-bind (s c) (%bwsb-pair)
    (unwind-protect
         (list (handler-case (progn (write-sequence (vector 300) s) :no-error)
                 (type-error () :type-error))
               (handler-case (progn (write-sequence (vector -1) s) :no-error)
                 (type-error () :type-error)))
      (close s) (close c)))
  (:type-error :type-error))
