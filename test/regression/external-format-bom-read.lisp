;;; :UTF-16 and :UTF-32 (no endianness) write a byte order mark; reading the file back with the same
;;; external format consumes it, so the text comes back without U+FEFF and FILE-POSITION counts the mark.

(defun %ebr-roundtrip (ef text)
  (let ((path (merge-pathnames "ebr.dat" (regression-temp-dir))))
    (with-open-file (o path :direction :output :if-exists :supersede :external-format ef)
      (write-string text o))
    (with-open-file (i path :external-format ef)
      (let ((line (read-line i nil nil)))
        (list line (file-position i) (file-length i))))))

(deftest external-format.bom-read-utf16
  (%ebr-roundtrip :utf-16 (coerce (list #\a (code-char 233)) 'string))
  (#.(coerce (list #\a (code-char 233)) 'string) 6 6))

(deftest external-format.bom-read-utf32
  (%ebr-roundtrip :utf-32 (coerce (list #\a (code-char 233)) 'string))
  (#.(coerce (list #\a (code-char 233)) 'string) 12 12))

(deftest external-format.bom-read-char-and-seek
  (let ((path (merge-pathnames "ebr2.dat" (regression-temp-dir)))
        (text (coerce (list #\x #\y #\z) 'string)))
    (with-open-file (o path :direction :output :if-exists :supersede :external-format :utf-16)
      (write-string text o))
    (with-open-file (i path :external-format :utf-16)
      (let ((first (read-char i)) (pos (file-position i)))
        (file-position i 2)
        (list first pos (read-char i) (read-char i)))))
  (#\x 4 #\x #\y))
