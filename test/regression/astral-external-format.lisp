;;; BMP-outside (astral) characters through native :external-format streams.
;;;
;;; dotcl characters are UTF-16 code units (char-code-limit 65536, ABCL-style):
;;; code-char of a code point above #xFFFF is NIL. When DECODING, an astral
;;; code point must not vanish or split the line — it streams as a surrogate
;;; pair (two characters), matching .NET string semantics. This is what keeps
;;; e.g. Unicode data files (NormalizationTest.txt contains U+242EE in a
;;; comment) readable line-by-line: read-line must yield ONE line
;;; "A<hi><lo>B", not a silently split "A" / "B".

(deftest astral-utf8-read-line
  (let ((path "astral-regression-tmp.bin"))
    (unwind-protect
        (progn
          ;; "A<U+242EE>B\nC\n" — U+242EE encodes as F0 A4 8B AE
          (with-open-file (s path :direction :output
                             :element-type '(unsigned-byte 8)
                             :if-exists :supersede)
            (dolist (b '(#x41 #xF0 #xA4 #x8B #xAE #x42 #x0A #x43 #x0A))
              (write-byte b s)))
          (with-open-file (s path :direction :input :external-format :utf-8)
            (let ((l1 (read-line s nil nil))
                  (l2 (read-line s nil nil))
                  (l3 (read-line s nil nil)))
              (list (map 'list #'char-code l1)
                    (map 'list #'char-code l2)
                    l3))))
      (ignore-errors (delete-file path))))
  ((65 55376 57070 66) (67) nil))

;;; The same pair round-trips back out as the original 4-byte sequence.
(deftest astral-utf8-write-roundtrip
  (let ((path "astral-regression-tmp2.bin"))
    (unwind-protect
        (progn
          (with-open-file (s path :direction :output :external-format :utf-8
                             :if-exists :supersede)
            (write-char (code-char #xD850) s)
            (write-char (code-char #xDEEE) s))
          (with-open-file (s path :direction :input
                             :element-type '(unsigned-byte 8))
            (loop for b = (read-byte s nil nil)
                  while b collect b)))
      (ignore-errors (delete-file path))))
  (#xF0 #xA4 #x8B #xAE))
