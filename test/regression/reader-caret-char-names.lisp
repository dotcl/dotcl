;;; #\^X reads as a control character (as SBCL does); linedit's key tables use #\^F.

(deftest reader-caret-char-names.codes
  (list (char-code #\^@) (char-code #\^A) (char-code #\^a) (char-code #\^F) (char-code #\^Z)
        (char-code #\^[) (char-code #\^]) (char-code #\^^) (char-code #\^_))
  (0 1 1 6 26 27 29 30 31))

(deftest reader-caret-char-names.same-as-named
  (list (eql #\^[ #\Esc) (eql #\^@ #\Nul) (eql #\^F #\Ack))
  (t t t))

(deftest reader-caret-char-names.name-char
  (list (char-code (name-char "^F")) (char-code (name-char "^f")) (name-char "^1") (name-char "^") (name-char "^FF"))
  (6 6 nil nil nil))

(deftest reader-caret-char-names.plain-caret
  (list (char-code #\^) (char-code #\^ ) (char-name #\^))
  (94 94 "CIRCUMFLEX_ACCENT"))
