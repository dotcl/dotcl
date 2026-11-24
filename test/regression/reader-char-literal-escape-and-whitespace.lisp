;;; #\ reads a token: a single escape inside the name takes the next character
;;; literally, and the whitespace that ends the name is discarded by READ.

(deftest reader-char-literal.escape-in-name
  (list (read-from-string "#\\S\\pace")
        (read-from-string "#\\sp\\acE")
        (read-from-string "#\\RUbou\\t")
        (read-from-string "#\\\\")
        (handler-case (read-from-string "#\\a\\") (end-of-file () :eof)))
  (#\Space #\Space #\Rubout #\\ :eof))

(deftest reader-char-literal.discards-terminating-whitespace
  (list (with-input-from-string (s "#\\a x") (list (read s) (read-char s)))
        (with-input-from-string (s "#\\Space x") (list (read s) (read-char s)))
        (with-input-from-string (s "#\\a) x") (list (ignore-errors (read s)) (read-char s nil :none))))
  ((#\a #\x) (#\Space #\x) (#\a #\))))

(deftest reader-char-literal.read-preserving-whitespace-keeps-it
  (with-input-from-string (s "#\\a x") (list (read-preserving-whitespace s) (read-char s)))
  (#\a #\Space))
