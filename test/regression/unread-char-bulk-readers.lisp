;;; A character given back with UNREAD-CHAR is the first one READ-LINE and READ-SEQUENCE
;;; return: they must not skip it.

(deftest unread-char-bulk-readers.read-line
  (let ((s (make-string-input-stream (format nil "xa~%b"))))
    (read-char s)
    (unread-char #\x s)
    (list (read-line s) (read-line s)))
  ("xa" "b"))

(deftest unread-char-bulk-readers.read-sequence
  (let ((s (make-string-input-stream "xa")) (b (make-string 4 :initial-element #\_)))
    (read-char s)
    (unread-char #\x s)
    (list (read-sequence b s) b))
  (2 "xa__"))

(deftest unread-char-bulk-readers.read-sequence-vector-and-list
  (let ((s1 (make-string-input-stream "xa")) (v (make-array 3 :initial-element 0))
        (s2 (make-string-input-stream "yb")) (l (list 0 0 0)))
    (read-char s1) (unread-char #\x s1)
    (read-char s2) (unread-char #\y s2)
    (list (read-sequence v s1) (coerce v 'list) (read-sequence l s2) l))
  (2 (#\x #\a 0) 2 (#\y #\b 0)))

(deftest unread-char-bulk-readers.empty-and-eof
  (let ((s (make-string-input-stream "")))
    (list (read-line s nil :eof) (read-sequence (make-string 2) s)))
  (:eof 0))

(deftest unread-char-bulk-readers.empty-range-keeps-the-character
  (let ((s (make-string-input-stream "xa")) (b (make-string 4 :initial-element #\_)))
    (read-char s)
    (unread-char #\x s)
    (list (read-sequence b s :start 2 :end 2) (read-line s)))
  (2 "xa"))

(deftest unread-char-bulk-readers.block-read-takes-pending-first
  (let ((s (make-string-input-stream "xabcdefgh")) (b (make-string 6 :initial-element #\_)))
    (read-char s)
    (unread-char #\x s)
    (list (read-sequence b s) b (read-char s)))
  (6 "xabcde" #\f))

(deftest unread-char-bulk-readers.block-read-pending-with-start
  (let ((s (make-string-input-stream "yz")) (b (make-string 5 :initial-element #\_)))
    (read-char s)
    (unread-char #\y s)
    (list (read-sequence b s :start 1 :end 4) b))
  (3 "_yz__"))
