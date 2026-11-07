;;; READ-CHAR and PEEK-CHAR on a plain string or file input stream take a short path;
;;; every other kind of stream, and every error, behaves as before.

(defun %cip-scan (in)
  (let ((out '()))
    (loop for c = (peek-char nil in nil :eof)
          until (eq c :eof)
          do (push (read-char in) out))
    (coerce (nreverse out) 'string)))

(deftest char-input-plain-stream-path.string
  (list (with-input-from-string (in "abc d") (%cip-scan in))
        (with-input-from-string (in "  x y") (list (peek-char t in) (read-char in) (peek-char t in)))
        (with-input-from-string (in "abcdef") (list (peek-char #\d in) (read-char in)))
        (with-input-from-string (in "ab") (list (read-char in) (progn (unread-char #\a in) (peek-char nil in)) (read-char in) (read-char in) (read-char in nil :end)))
        (with-input-from-string (in "") (list (peek-char nil in nil :e) (read-char in nil :r))))
  ("abc d" (#\x #\x #\y) (#\d #\d) (#\a #\a #\a #\b :end) (:e :r)))

(deftest char-input-plain-stream-path.file
  (let ((p (format nil "cip-~a.tmp" (random 1000000))))
    (unwind-protect
         (progn
           (with-open-file (o p :direction :output :if-exists :supersede) (write-string "hello world" o))
           (list (with-open-file (in p) (%cip-scan in))
                 (with-open-file (in p) (list (peek-char #\w in) (read-char in)))
                 (with-open-file (in p :element-type '(unsigned-byte 8))
                   (handler-case (read-char in) (error () :error)))
                 (with-open-file (o p :direction :output :if-exists :append)
                   (handler-case (read-char o) (error () :error)))))
      (ignore-errors (delete-file p))))
  ("hello world" (#\w #\w) :error :error))

(deftest char-input-plain-stream-path.composite
  (list (let ((in (make-concatenated-stream (make-string-input-stream "ab") (make-string-input-stream "cd"))))
          (%cip-scan in))
        (let ((in (make-two-way-stream (make-string-input-stream "xy") (make-broadcast-stream))))
          (list (peek-char nil in) (read-char in) (read-char in)))
        (let* ((out (make-string-output-stream))
               (in (make-echo-stream (make-string-input-stream "pq") out)))
          (list (read-char in) (peek-char nil in) (read-char in) (get-output-stream-string out)))
        (let ((s (make-string-input-stream "m")))
          (let ((*standard-input* s)) (list (peek-char) (read-char)))))
  ("abcd" (#\x #\x #\y) (#\p #\q #\q "pq") (#\m #\m)))
