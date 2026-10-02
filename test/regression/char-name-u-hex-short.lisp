;;; Regression: NAME-CHAR and the #\ reader took U followed by hex digits as a
;;; code point only with 4, 6 or 8 digits, so #\U20 and #\u41 were "Unknown
;;; character name". SBCL reads any width; mu-json writes its JSON character
;;; ranges as #\U20 #\U21 #\U23 #\U5B #\U5D.

(deftest char-name-u-hex-short
  (list (name-char "U20") (name-char "u41") (name-char "U5D")
        (read-from-string "#\\U20") (read-from-string "#\\U5B")
        (name-char "U0041") (name-char "U+41"))
  (#\Space #\A #\] #\Space #\[ #\A #\A))

(deftest char-name-u-hex-short-not-a-code-point
  (list (name-char "U") (name-char "UZ") (name-char "U4G")
        (name-char "Up"))
  (nil nil nil nil))
