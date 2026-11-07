;;; READ-CHAR on a file or string input stream goes straight to the stream's reader.
;;; The results, the end-of-file handling, the errors, and the composite streams
;;; (synonym, two-way, echo, concatenated) are as before.

(defun %rcp-path () (regression-temp-file (format nil "rcp-~a" (random 1000000))))

(deftest read-char-plain.file-and-string
  (let ((path (%rcp-path)))
    (with-open-file (o path :direction :output :if-exists :supersede) (write-string (format nil "ab~%c~a" (code-char 233)) o))
    (unwind-protect
         (with-open-file (s path)
           (list (read-char s) (read-char s) (read-char s) (read-char s) (char-code (read-char s))
                 (read-char s nil :eof) (read-char s nil :end) (handler-case (read-char s) (end-of-file () :eof-error))))
      (ignore-errors (delete-file path))))
  (#\a #\b #\Newline #\c 233 :eof :end :eof-error))

(deftest read-char-plain.string-stream
  (let ((s (make-string-input-stream "xyz")))
    (list (read-char s) (peek-char nil s) (read-char s) (read-char s) (read-char s nil 0)))
  (#\x #\y #\y #\z 0))

(deftest read-char-plain.after-unread-char
  (let ((s (make-string-input-stream "abc")))
    (let ((c (read-char s)))
      (unread-char c s)
      (list (read-char s) (read-char s) (progn (unread-char #\b s) (read-char s)) (read-char s))))
  (#\a #\b #\b #\c))

(deftest read-char-plain.binary-file-is-an-error
  (let ((path (%rcp-path)))
    (with-open-file (o path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8)) (write-byte 65 o))
    (unwind-protect
         (with-open-file (s path :element-type '(unsigned-byte 8))
           (list (handler-case (read-char s) (error () :error)) (read-byte s)))
      (ignore-errors (delete-file path))))
  (:error 65))

(deftest read-char-plain.closed-stream-is-an-error
  (let ((s (make-string-input-stream "abc")))
    (close s)
    (handler-case (read-char s) (error () :error)))
  :error)

(deftest read-char-plain.output-stream-is-an-error
  (handler-case (read-char (make-string-output-stream)) (error () :error))
  :error)

(deftest read-char-plain.composite-streams
  (let* ((in (make-string-input-stream "abcdef"))
         (out (make-string-output-stream))
         (echo (make-echo-stream in out))
         (two (make-two-way-stream (make-string-input-stream "12") (make-string-output-stream)))
         (cat (make-concatenated-stream (make-string-input-stream "x") (make-string-input-stream "y"))))
    (list (read-char echo) (read-char echo) (get-output-stream-string out)
          (read-char two) (read-char two) (read-char two nil :eof)
          (read-char cat) (read-char cat) (read-char cat nil :eof)))
  (#\a #\b "ab" #\1 #\2 :eof #\x #\y :eof))

(deftest read-char-plain.synonym-stream
  (let ((*standard-input* (make-string-input-stream "q")))
    (list (read-char) (read-char *standard-input* nil :eof)))
  (#\q :eof))

(deftest read-char-plain.io-file-stream
  (let ((path (%rcp-path)))
    (with-open-file (o path :direction :output :if-exists :supersede) (write-string "hello" o))
    (unwind-protect
         (with-open-file (s path :direction :io :if-exists :overwrite)
           (list (read-char s) (read-char s) (file-position s)))
      (ignore-errors (delete-file path))))
  (#\h #\e 2))
