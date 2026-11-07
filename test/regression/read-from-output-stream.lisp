;;; Reading from an output-only stream is a TYPE-ERROR saying so, not an
;;; END-OF-FILE.

(deftest read-from-output-stream.read-char
  (handler-case (with-output-to-string (s) (read-char s))
    (type-error () :type-error)
    (end-of-file () :end-of-file))
  :type-error)

(deftest read-from-output-stream.read-line
  (handler-case (with-output-to-string (s) (read-line s))
    (type-error () :type-error)
    (end-of-file () :end-of-file))
  :type-error)

(deftest read-from-output-stream.peek-char
  (handler-case (with-output-to-string (s) (peek-char nil s))
    (type-error () :type-error)
    (end-of-file () :end-of-file))
  :type-error)

(deftest read-from-output-stream.input-still-works
  (with-input-from-string (s "ab") (list (read-char s) (peek-char nil s) (read-line s)))
  (#\a #\b "b"))
