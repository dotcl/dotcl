;;; READ through an ECHO-STREAM echoes what it consumed.
;;;
;;; The Lisp reader takes the TextReader from underneath the echo-stream
;;; wrapper, so READ and READ-PRESERVING-WHITESPACE echoed nothing at all,
;;; while READ-CHAR and READ-LINE (which handle the echo themselves) were fine.

(defun %esr (fn text)
  (let* ((in (make-string-input-stream text))
         (out (make-string-output-stream))
         (e (make-echo-stream in out)))
    (let ((r (funcall fn e)))
      (list r (get-output-stream-string out)))))

(deftest echo-stream-read.read-echoes
  (%esr (lambda (e) (read e nil :eof)) "abc def")
  (abc "abc "))

;; READ-PRESERVING-WHITESPACE leaves the delimiter unread, and does not echo it
;; either: it never consumed it (it peeked). SBCL echoes the space here because
;; its reader reads the character and unreads it; both are defensible, and
;; echoing only what was consumed is the more literal reading of CLHS 21.1.3.
(deftest echo-stream-read.read-preserving-whitespace-echoes-what-it-consumed
  (%esr (lambda (e) (read-preserving-whitespace e nil :eof)) "abc def")
  (abc "abc"))

(deftest echo-stream-read.read-list
  (%esr (lambda (e) (read e nil :eof)) "(a b) tail")
  ((a b) "(a b) "))

;; Peeking is not reading.
(deftest echo-stream-read.peek-does-not-echo
  (%esr (lambda (e) (peek-char nil e nil :eof)) "abc")
  (#\a ""))

;; The character-level functions keep working (they echo through their own path).
(deftest echo-stream-read.char-and-line-still-echo
  (list (%esr (lambda (e) (read-char e)) "abc def")
        (%esr (lambda (e) (read-line e nil :eof)) "abc def"))
  ((#\a "a") ("abc def" "abc def")))

;; Reading twice echoes both forms, and the echo does not replay anything.
(deftest echo-stream-read.two-reads
  (let* ((in (make-string-input-stream "one two"))
         (out (make-string-output-stream))
         (e (make-echo-stream in out)))
    (let ((a (read e nil :eof)) (b (read e nil :eof)))
      (list a b (get-output-stream-string out))))
  (one two "one two"))
