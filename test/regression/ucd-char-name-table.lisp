;;; The Unicode character name table behind NAME-CHAR / CHAR-NAME is one text
;;; constant parsed when first used. These check both ends of the table, code
;;; points with hex letters in them, and that names match case-insensitively
;;; with either spaces or underscores.

(deftest ucd-char-name-table.first-entries
  (list (char-code (name-char "EXCLAMATION_MARK"))
        (char-code (name-char "quotation mark"))
        (char-name (code-char #x21)))
  (#x21 #x22 "EXCLAMATION_MARK"))

(deftest ucd-char-name-table.last-entry
  (list (char-code (name-char "REPLACEMENT_CHARACTER"))
        (char-name (code-char #xFFFD)))
  (#xFFFD "REPLACEMENT_CHARACTER"))

(deftest ucd-char-name-table.hex-letters
  (list (char-code (name-char "HALFWIDTH_HANGUL_LETTER_RIEUL-THIEUTH"))
        (char-name (code-char #xFFAE))
        (char-code (name-char "greek small letter alpha")))
  (#xFFAE "HALFWIDTH_HANGUL_LETTER_RIEUL-THIEUTH" #x3B1))

(deftest ucd-char-name-table.unknown
  (list (name-char "NO_SUCH_CHARACTER_NAME_AT_ALL")
        (char-name (code-char #x0378)))
  (nil "U+0378"))
