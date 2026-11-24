;;; CHAR-NAME of a character the Unicode name table names gives that name in upper case
;;; with underscores, whichever spelling NAME-CHAR also reads for it.

(deftest char-name-unicode-spelling.names
  (mapcar (lambda (code) (char-name (code-char code)))
          '(160 12288 65279 955 58 45 126 46 59 44 47 92 124 34 233))
  ("NO-BREAK_SPACE" "IDEOGRAPHIC_SPACE" "ZERO_WIDTH_NO-BREAK_SPACE" "GREEK_SMALL_LETTER_LAMDA"
   "COLON" "HYPHEN-MINUS" "TILDE" "FULL_STOP" "SEMICOLON" "COMMA" "SOLIDUS" "REVERSE_SOLIDUS"
   "VERTICAL_LINE" "QUOTATION_MARK" "LATIN_SMALL_LETTER_E_WITH_ACUTE"))

(deftest char-name-unicode-spelling.printed
  (list (prin1-to-string (code-char 160)) (prin1-to-string (code-char 955))
        (char-name (code-char 133)))
  ("#\\NO-BREAK_SPACE" "#\\GREEK_SMALL_LETTER_LAMDA" "Next-Line"))

(deftest char-name-unicode-spelling.read-back
  (list (char-code (name-char "No-break_space")) (char-code (name-char "Greek_small_letter_lamda"))
        (char-code (name-char "Slash")) (char-code (name-char "Period")) (char-code (name-char "Nel"))
        (char-code (name-char "SOLIDUS")) (char-code (name-char "Next-Line")))
  (160 955 47 46 133 47 133))

(deftest char-name-unicode-spelling.c0-names
  (list (char-name (code-char 0)) (char-name (code-char 27))
        (prin1-to-string (code-char 0)) (prin1-to-string (code-char 27))
        (char-code (name-char "Null")) (char-code (name-char "Escape"))
        (char-code (name-char "Nul")) (char-code (name-char "Esc")))
  ("Nul" "Esc" "#\\Nul" "#\\Esc" 0 27 0 27))
