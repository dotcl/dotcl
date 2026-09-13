;;; FILE-POSITION after UNREAD-CHAR on a string input stream.
;;;
;;; A character pushed back has not been consumed -- the next READ-CHAR returns
;;; it again -- so the position has to be where it was before the READ. Code
;;; that peeks by reading and unreading (Eclector's whitespace skipping, and any
;;; reader that records where a form began) reported one character too far.

(deftest ucp-read-then-unread-is-back-at-start
  (let ((s (make-string-input-stream "foo")))
    (read-char s)
    (unread-char #\f s)
    (file-position s))
  0)

(deftest ucp-read-twice-then-unread
  (let ((s (make-string-input-stream "foo")))
    (read-char s)
    (read-char s)
    (unread-char #\o s)
    (file-position s))
  1)

;;; The pushed-back character still comes back, and reading it advances again.
(deftest ucp-unread-then-read-again
  (let ((s (make-string-input-stream "foo")))
    (read-char s)
    (unread-char #\f s)
    (list (read-char s) (file-position s)))
  (#\f 1))

;;; Plain reading is unaffected.
(deftest ucp-position-without-pushback
  (let ((s (make-string-input-stream "hello")))
    (dotimes (i 3) (read-char s))
    (file-position s))
  3)

(deftest ucp-fresh-stream-is-zero
  (file-position (make-string-input-stream "foo"))
  0)

;;; PEEK-CHAR does not consume, so it never moved the position to begin with.
(deftest ucp-peek-does-not-move
  (let ((s (make-string-input-stream "  foo")))
    (list (peek-char t s) (file-position s)))
  (#\f 2))

;;; Seeking abandons a pushed-back character: it belonged to the old position,
;;; and returning it after a seek would hand back a character from elsewhere.
(deftest ucp-seek-clears-pushback
  (let ((s (make-string-input-stream "foo")))
    (read-char s)
    (unread-char #\f s)
    (file-position s 1)
    (list (read-char s) (file-position s)))
  (#\o 2))
