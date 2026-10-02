;;; :external-format names served by .NET code-page encodings (Shift_JIS,
;;; EUC-JP, ISO-2022-JP, GBK, Big5, EUC-KR, KOI8-R, CP125x, ISO-8859-N) and the
;;; escape hatches (.NET encoding names, (:code-page N)). Only UTF-8, Latin-1,
;;; ASCII, UTF-16 and UTF-32 used to be known; everything else signalled
;;; "unsupported external format".
;;;
;;; Non-ASCII text is built from code points so this file stays ASCII.

(defun %efcp-string (&rest codes)
  (coerce (mapcar #'code-char codes) 'string))

;; U+65E5 U+672C U+8A9E: the word "Japanese" in Japanese.
(defparameter *efcp-nihongo* (%efcp-string #x65E5 #x672C #x8A9E))

(defun %efcp-write-bytes (string format)
  "Write STRING through FORMAT and return the file's octets."
  (let ((path "efcp-regression-tmp.bin"))
    (unwind-protect
         (progn
           (with-open-file (s path :direction :output :if-exists :supersede
                                   :external-format format)
             (write-string string s))
           (with-open-file (s path :element-type '(unsigned-byte 8))
             (let ((v (make-list (file-length s))))
               (map-into v (lambda () (read-byte s))))))
      (ignore-errors (delete-file path)))))

(defun %efcp-read-bytes (octets format)
  "Write OCTETS raw and read them back as characters through FORMAT."
  (let ((path "efcp-regression-tmp2.bin"))
    (unwind-protect
         (progn
           (with-open-file (s path :direction :output :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
             (dolist (b octets) (write-byte b s)))
           (with-open-file (s path :external-format format)
             (with-output-to-string (out)
               (loop for ch = (read-char s nil nil)
                     while ch do (write-char ch out)))))
      (ignore-errors (delete-file path)))))

(defun %efcp-codes (string) (map 'list #'char-code string))

;;; The Japanese encodings, byte for byte.
(deftest efcp-cp932-bytes
  (list (%efcp-write-bytes *efcp-nihongo* :cp932)
        (%efcp-write-bytes *efcp-nihongo* :shift_jis)
        (%efcp-write-bytes *efcp-nihongo* :sjis))
  ((#x93 #xFA #x96 #x7B #x8C #xEA)
   (#x93 #xFA #x96 #x7B #x8C #xEA)
   (#x93 #xFA #x96 #x7B #x8C #xEA)))

(deftest efcp-euc-jp-bytes
  (%efcp-write-bytes *efcp-nihongo* :euc-jp)
  (#xC6 #xFC #xCB #xDC #xB8 #xEC))

(deftest efcp-iso-2022-jp-bytes
  (%efcp-write-bytes *efcp-nihongo* :iso-2022-jp)
  (#x1B #x24 #x42 #x46 #x7C #x4B #x5C #x38 #x6C #x1B #x28 #x42))

;;; ... and back.
(deftest efcp-japanese-decode
  (list (string= (%efcp-read-bytes '(#x93 #xFA #x96 #x7B #x8C #xEA) :cp932)
                 *efcp-nihongo*)
        (string= (%efcp-read-bytes '(#x93 #xFA #x96 #x7B #x8C #xEA) :shift_jis)
                 *efcp-nihongo*)
        (string= (%efcp-read-bytes '(#xC6 #xFC #xCB #xDC #xB8 #xEC) :euc-jp)
                 *efcp-nihongo*)
        (string= (%efcp-read-bytes '(#x1B #x24 #x42 #x46 #x7C #x4B #x5C #x38 #x6C
                                     #x1B #x28 #x42)
                                   :iso-2022-jp)
                 *efcp-nihongo*))
  (t t t t))

;;; Shift_JIS means code page 932, so the Windows extensions round-trip:
;;; CIRCLED DIGIT ONE is 87 40 in cp932 and absent from JIS X 0208.
(deftest efcp-shift-jis-is-cp932
  (list (%efcp-write-bytes (%efcp-string #x2460) :shift_jis)
        (%efcp-codes (%efcp-read-bytes '(#x87 #x40) :shift_jis)))
  ((#x87 #x40) (#x2460)))

;;; A character the code page lacks is written as "?", not signalled and not
;;; replaced by a best-fit look-alike (e-acute would otherwise become "e").
(deftest efcp-unencodable-is-question-mark
  (%efcp-write-bytes (%efcp-string #xE9 #x65E5) :cp932)
  (#x3F #x93 #xFA))

;;; A byte sequence the code page cannot decode (here a lead byte cut off by
;;; the end of the file) reads as U+FFFD, as in UTF-8.
(deftest efcp-undecodable-is-replacement-char
  (%efcp-codes (%efcp-read-bytes '(#x41 #x93) :cp932))
  (#x41 #xFFFD))

;;; Other SBCL names: Chinese, Korean, Cyrillic, Windows code pages, ISO-8859-N.
(deftest efcp-other-scripts
  (list (%efcp-write-bytes (%efcp-string #x4E2D) :gbk)          ; CJK "middle"
        (%efcp-write-bytes (%efcp-string #x4E2D) :cp936)
        (%efcp-write-bytes (%efcp-string #x4E2D) :big5)
        (%efcp-write-bytes (%efcp-string #xD55C) :euc-kr)       ; Hangul HAN
        (%efcp-write-bytes (%efcp-string #xD55C) :cp949)
        (%efcp-write-bytes (%efcp-string #x0416) :koi8-r)       ; Cyrillic ZHE
        (%efcp-write-bytes (%efcp-string #x0416) :cp1251)
        (%efcp-write-bytes (%efcp-string #x20AC) :cp1252)       ; euro sign
        (%efcp-write-bytes (%efcp-string #x0416) :iso-8859-5)
        (%efcp-write-bytes (%efcp-string #x20AC) :latin-9))
  ((#xD6 #xD0) (#xD6 #xD0) (#xA4 #xA4) (#xC7 #xD1) (#xC7 #xD1)
   (#xF6) (#xC6) (#x80) (#xB6) (#xA4)))

;;; Escape hatches: a name .NET knows (string or keyword) and (:code-page N).
(deftest efcp-escape-hatches
  (list (%efcp-write-bytes (%efcp-string #x0416) "windows-1251")
        (%efcp-write-bytes (%efcp-string #x0416) :windows-1251)
        (%efcp-write-bytes (%efcp-string #x0416) '(:code-page 1251))
        (%efcp-write-bytes *efcp-nihongo* '(:code-page 932)))
  ((#xC6) (#xC6) (#xC6) (#x93 #xFA #x96 #x7B #x8C #xEA)))

;;; A list designator with options after the name still picks the encoding.
(deftest efcp-list-designator-with-options
  (%efcp-write-bytes *efcp-nihongo* '(:shift_jis :replacement #\?))
  (#x93 #xFA #x96 #x7B #x8C #xEA))

;;; Line structure survives: read-line splits on LF and CR LF alike.
(deftest efcp-read-line-cp932
  (let ((s (%efcp-read-bytes '(#x93 #xFA #x0D #x0A #x96 #x7B #x0A) :cp932)))
    (with-input-from-string (in s)
      (list (%efcp-codes (string-right-trim '(#\Return) (read-line in)))
            (%efcp-codes (read-line in)))))
  ((#x65E5) (#x672C)))

;;; stream-external-format reports the designator as given.
(deftest efcp-reported-by-stream
  (let ((path "efcp-regression-tmp3.bin"))
    (unwind-protect
         (progn
           (with-open-file (s path :direction :output :if-exists :supersede
                                   :external-format :shift_jis)
             (write-char #\a s))
           (with-open-file (s path :external-format :shift_jis)
             (stream-external-format s)))
      (ignore-errors (delete-file path))))
  :shift_jis)

;;; Unknown names and code pages still signal instead of falling back to UTF-8.
(deftest efcp-unknown-signals
  (list (handler-case (progn (%efcp-write-bytes "a" :no-such-codepage) :no-error)
          (error () :errored))
        (handler-case (progn (%efcp-write-bytes "a" "no-such-encoding") :no-error)
          (error () :errored))
        (handler-case (progn (%efcp-write-bytes "a" '(:code-page 99999)) :no-error)
          (error () :errored))
        (handler-case (progn (%efcp-write-bytes "a" '(:code-page 1)) :no-error)
          (error () :errored)))
  (:errored :errored :errored :errored))
