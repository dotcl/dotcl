;;; WRITE-CHAR to a string output stream is a direct write to the stream's own writer. Same text,
;;; same line-start state (FRESH-LINE, ~&), same errors as every other output stream.

(deftest write-char-string-stream.text
  (let ((o (make-string-output-stream)))
    (write-char #\a o) (write-char #\Space o) (write-char (code-char 955) o) (write-char #\Newline o)
    (write-char #\z o)
    (list (get-output-stream-string o) (write-char #\q o) (get-output-stream-string o)))
  ("a λ
z" #\q "q"))

(deftest write-char-string-stream.line-state
  (let ((o (make-string-output-stream)))
    (write-char #\a o) (fresh-line o)
    (write-char #\Newline o) (fresh-line o)
    (write-char #\b o) (format o "~&c")
    (write-char #\Newline o) (format o "~&d")
    (get-output-stream-string o))
  "a

b
c
d")

(deftest write-char-string-stream.errors
  (let ((o (make-string-output-stream)) (w (with-output-to-string (s) (write-char #\k s))))
    (list (handler-case (write-char 65 o) (type-error () :type-error))
          (handler-case (write-char "a" o) (type-error () :type-error))
          (handler-case (write-char #\a 5) (type-error () :type-error))
          (progn (close o) (handler-case (write-char #\a o) (stream-error () :stream-error)))
          w))
  (:type-error :type-error :type-error :stream-error "k"))

(deftest write-char-string-stream.other-streams
  (let ((s (make-string-output-stream)) (b (make-broadcast-stream)) (str nil))
    (let ((two (make-two-way-stream (make-string-input-stream "x") s))
          (both (make-broadcast-stream s)))
      (write-char #\t two) (write-char #\u both))
    (write-char #\v b)
    (setq str (with-output-to-string (o)
                (let ((*standard-output* o)) (write-char #\w) (write-char #\x *standard-output*))))
    (list (get-output-stream-string s) str))
  ("tu" "wx"))

(deftest write-char-string-stream.pretty-printing
  (list (with-output-to-string (o) (let ((*print-pretty* t)) (pprint-logical-block (o nil) (write-char #\a o) (pprint-newline :fill o) (write-char #\b o))))
        (let ((*print-right-margin* 10))
          (with-output-to-string (o)
            (pprint-logical-block (o nil :prefix "(" :suffix ")")
              (dotimes (i 6) (write-char #\x o) (write-char #\Space o) (pprint-newline :fill o))))))
  ("ab" "(x x x x
 x x )"))
