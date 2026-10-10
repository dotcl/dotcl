;;; READ-CHAR, UNREAD-CHAR, PEEK-CHAR and READ-CHAR-NO-HANG on a binary Gray
;;; stream go to the Gray generics, as on SBCL, which sends them to
;;; STREAM-READ-CHAR and friends whatever the stream's class. They went to
;;; standard input instead: READ-CHAR signalled END-OF-FILE (stdin closed) and
;;; UNREAD-CHAR put the character back onto standard input. circular-streams
;;; is a binary Gray stream that defines STREAM-READ-CHAR and failed 8 of its
;;; 15 checks. READ-CHAR-NO-HANG on a character Gray stream read standard input
;;; too. Expected values are SBCL 2.6.8's answers.
(require "dotcl-gray")

(defclass %gbc-in (dotcl-gray:fundamental-binary-input-stream)
  ((data :initarg :data :accessor %gbc-data)
   (back :initform nil :accessor %gbc-back)))
(defmethod stream-element-type ((s %gbc-in)) '(unsigned-byte 8))
(defmethod dotcl-gray:stream-read-byte ((s %gbc-in))
  (if (%gbc-data s) (char-code (pop (%gbc-data s))) :eof))
(defmethod dotcl-gray:stream-read-char ((s %gbc-in))
  (or (pop (%gbc-back s))
      (let ((b (dotcl-gray:stream-read-byte s))) (if (eq b :eof) :eof (code-char b)))))
(defmethod dotcl-gray:stream-unread-char ((s %gbc-in) c) (push c (%gbc-back s)) nil)
(defmethod dotcl-gray:stream-peek-char ((s %gbc-in))
  (let ((c (dotcl-gray:stream-read-char s)))
    (unless (eq c :eof) (dotcl-gray:stream-unread-char s c))
    c))
(defmethod dotcl-gray:stream-read-char-no-hang ((s %gbc-in))
  (dotcl-gray:stream-read-char s))

(deftest gray-binary-character-reads.read-char
  (let ((s (make-instance '%gbc-in :data (coerce "ab" 'list))))
    (list (read-char s) (read-byte s) (read-char s nil :eof)))
  (#\a 98 :eof))

(deftest gray-binary-character-reads.unread-peek
  (let ((s (make-instance '%gbc-in :data (coerce "xy" 'list))))
    (let ((c (read-char s)))
      (unread-char c s)
      (list (peek-char nil s) (read-char s) (read-char-no-hang s) (read-char-no-hang s nil :eof))))
  (#\x #\x #\y :eof))

(deftest gray-binary-character-reads.end-of-file
  (handler-case (read-char (make-instance '%gbc-in :data nil))
    (end-of-file () :eof))
  :eof)

(defclass %gbc-chars (dotcl-gray:fundamental-character-input-stream)
  ((chars :initarg :chars :accessor %gbc-chars)))
(defmethod dotcl-gray:stream-read-char ((s %gbc-chars))
  (if (%gbc-chars s) (pop (%gbc-chars s)) :eof))
(defmethod dotcl-gray:stream-read-char-no-hang ((s %gbc-chars))
  (if (%gbc-chars s) (pop (%gbc-chars s)) nil))

;; READ-CHAR-NO-HANG on a character Gray stream: its own method's answer,
;; NIL meaning nothing is ready yet.
(deftest gray-binary-character-reads.read-char-no-hang-character-stream
  (let ((s (make-instance '%gbc-chars :chars (list #\q))))
    (list (read-char-no-hang s) (read-char-no-hang s)))
  (#\q nil))
