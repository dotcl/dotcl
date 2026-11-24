;;; A string input stream reads its string directly (no StringReader behind it).

(deftest sis-reader.read-and-peek
  (with-input-from-string (in "abc")
    (list (peek-char nil in) (read-char in) (peek-char nil in) (read-char in) (read-char in)
          (peek-char nil in nil :eof) (read-char in nil :eof)
          (handler-case (read-char in) (end-of-file () :error))
          (handler-case (peek-char nil in) (end-of-file () :error))))
  (#\a #\a #\b #\b #\c :eof :eof :error :error))

(deftest sis-reader.start-and-end
  (list (with-input-from-string (in "abcdef" :start 2 :end 5)
          (list (read-char in) (read-char in) (read-char in) (read-char in nil :eof)))
        (with-input-from-string (in "abcdef" :start 2)
          (list (read-char in) (read-char in)))
        (with-input-from-string (in "abc" :index nil) (read-char in)))
  ((#\c #\d #\e :eof) (#\c #\d) #\a))

(deftest sis-reader.read-line
  (with-input-from-string (in (format nil "one~%two~%~%last"))
    (loop for l = (read-line in nil nil) while l collect l))
  ("one" "two" "" "last"))

(deftest sis-reader.unread-and-peek-types
  (with-input-from-string (in "  x y")
    (let ((a (read-char in)))
      (unread-char a in)
      (list (peek-char nil in) (peek-char t in) (read-char in) (peek-char #\y in) (read-char in))))
  (#\Space #\x #\x #\y #\y))

(deftest sis-reader.file-position-set
  (with-input-from-string (in "abcdef")
    (read-char in) (read-char in)
    (let ((p (file-position in)))
      (file-position in 0)
      (list p (read-char in) (progn (file-position in 4) (read-char in)))))
  (2 #\a #\e))

(deftest sis-reader.read-forms
  (with-input-from-string (in "(a b) 42 \"s\" foo")
    (list (read in) (read in) (read in) (read in) (read in nil :eof)))
  ((a b) 42 "s" foo :eof))

(deftest sis-reader.listen-and-read-sequence
  (with-input-from-string (in "hello")
    (let ((buf (make-string 3)))
      (list (listen in) (read-sequence buf in) buf (read-char in) (listen in))))
  (t 3 "hel" #\l t))

(deftest sis-reader.make-string-input-stream-bad-bounds
  (list (handler-case (make-string-input-stream "abc" 5) (type-error () :error))
        (handler-case (make-string-input-stream "abc" 0 9) (type-error () :error))
        (handler-case (make-string-input-stream "abc" 2 1) (type-error () :error))
        (read-char (make-string-input-stream "abc" 1 3)))
  (:error :error :error #\b))

(deftest sis-reader.closed-stream-is-an-error
  (list (handler-case (let ((s (make-string-input-stream "abc"))) (close s) (read-char s)) (stream-error () :error))
        (handler-case (let ((s (make-string-input-stream "abc"))) (close s) (peek-char nil s)) (stream-error () :error))
        (handler-case (let ((s (make-string-input-stream "abc"))) (close s) (read-line s)) (stream-error () :error)))
  (:error :error :error))
