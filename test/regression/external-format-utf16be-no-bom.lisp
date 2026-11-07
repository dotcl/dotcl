;;; :UTF-16BE writes no byte order mark (a BOM belongs to the unmarked UTF-16), as :UTF-16LE does.

(defun %e16-bytes (ef text)
  (let ((path (merge-pathnames "e16.dat" (regression-temp-dir))))
    (with-open-file (o path :direction :output :if-exists :supersede :external-format ef)
      (write-string text o))
    (with-open-file (i path :element-type '(unsigned-byte 8))
      (let ((v (make-array (file-length i))))
        (read-sequence v i)
        (coerce v 'list)))))

(deftest external-format.utf16be-no-bom
  (list (%e16-bytes :utf-16be (coerce '(#\a #\LATIN_SMALL_LETTER_E_WITH_ACUTE) 'string))
        (%e16-bytes :utf-16le (coerce '(#\a #\LATIN_SMALL_LETTER_E_WITH_ACUTE) 'string)))
  ((0 97 0 233) (97 0 233 0)))

(deftest external-format.utf16be-round-trip
  (let ((path (merge-pathnames "e16rt.dat" (regression-temp-dir)))
        (text (coerce (list #\a #\LATIN_SMALL_LETTER_E_WITH_ACUTE #\Newline #\z) 'string)))
    (with-open-file (o path :direction :output :if-exists :supersede :external-format :utf-16be)
      (write-string text o))
    (with-open-file (i path :external-format :utf-16be)
      (list (read-line i) (read-line i))))
  (#.(coerce (list #\a (code-char 233)) 'string) "z"))
