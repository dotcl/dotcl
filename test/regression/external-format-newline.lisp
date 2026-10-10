;;; The line end of an external format: (:latin-1 :newline :crlf) and friends.
;;;
;;; :NEWLINE is SBCL's option; :EOL-STYLE (LispWorks, ABCL, flexi-streams) is
;;; accepted as the same thing. The value is :LF, :CRLF or :CR. Without one,
;;; #\Newline is the single LF byte on every OS, as before.
;;;
;;; Output writes #\Newline as the line end. Input reads the line end as
;;; #\Newline; under :CRLF a CR or LF on its own is read as itself. Expected
;;; values are SBCL 2.6.8's, except for :EOL-STYLE and the encodings SBCL
;;; does not name (:cp932, :utf-16le), which it rejects.
;;;
;;; Non-ASCII text is built from code points so this file stays ASCII.

(defparameter *efnl-dir* (regression-temp-dir))
(defun %efnl-path (name) (concatenate 'string *efnl-dir* "/" name))

(defun %efnl-write (string format)
  "Write STRING through FORMAT and return the file's octets as a list."
  (let ((path (%efnl-path "efnl-out.bin")))
    (with-open-file (s path :direction :output :if-exists :supersede
                            :external-format format)
      (write-string string s))
    (with-open-file (s path :element-type '(unsigned-byte 8))
      (loop for b = (read-byte s nil) while b collect b))))

(defun %efnl-read (octets format)
  "Write OCTETS raw and read them back through FORMAT, as a list of char codes."
  (let ((path (%efnl-path "efnl-in.bin")))
    (with-open-file (s path :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (dolist (b octets) (write-byte b s)))
    (with-open-file (s path :external-format format)
      (loop for c = (read-char s nil) while c collect (char-code c)))))

(defun %efnl-round-trip (string format)
  (let ((path (%efnl-path "efnl-rt.txt")))
    (with-open-file (s path :direction :output :if-exists :supersede
                            :external-format format)
      (write-string string s))
    (with-open-file (s path :external-format format)
      (let ((r (make-string (length string))))
        (subseq r 0 (read-sequence r s))))))

(defparameter *efnl-text* (format nil "a~%b~%"))

;;; Bytes written, per line end, in a one-byte encoding.

(deftest external-format-newline.latin-1-bytes
  (list (%efnl-write *efnl-text* :latin-1)
        (%efnl-write *efnl-text* '(:latin-1 :newline :lf))
        (%efnl-write *efnl-text* '(:latin-1 :newline :crlf))
        (%efnl-write *efnl-text* '(:latin-1 :newline :cr)))
  ((97 10 98 10) (97 10 98 10) (97 13 10 98 13 10) (97 13 98 13)))

;;; The same through other encodings: UTF-8 with a non-ASCII character, cp932
;;; with a kanji (U+65E5 is 93 FA), UTF-16LE where CR LF is four bytes.

(deftest external-format-newline.utf-8-bytes
  (%efnl-write (format nil "~C~%" (code-char #xE9)) '(:utf-8 :newline :crlf))
  (195 169 13 10))

(deftest external-format-newline.cp932-bytes
  (list (%efnl-write (format nil "~C~%" (code-char #x65E5)) '(:cp932 :newline :crlf))
        (%efnl-write (format nil "~C~%" (code-char #x65E5)) '(:cp932 :newline :cr)))
  ((147 250 13 10) (147 250 13)))

(deftest external-format-newline.utf-16le-bytes
  (%efnl-write (format nil "a~%") '(:utf-16le :newline :crlf))
  (97 0 13 0 10 0))

;;; Round trips through every line end and several encodings.

(deftest external-format-newline.round-trips
  (let ((text (format nil "x~%~%y~C~%" (code-char #x65E5))))
    (loop for enc in '(:utf-8 :cp932 :utf-16le :euc-jp)
          append (loop for nl in '(:lf :crlf :cr)
                       collect (string= text (%efnl-round-trip text (list enc :newline nl))))))
  (t t t t t t t t t t t t))

(deftest external-format-newline.latin-1-round-trips
  (let ((text (format nil "~C~%~%z" (code-char #xFF))))
    (loop for nl in '(:lf :crlf :cr)
          collect (string= text (%efnl-round-trip text (list :latin-1 :newline nl)))))
  (t t t))

;;; Reading: under :CRLF only CR LF is a newline; under :CR every CR is, and LF
;;; stays LF. Bytes: a LF b CR c CR LF d CR (a CR at end of file).

(deftest external-format-newline.reading
  (let ((bytes '(97 10 98 13 99 13 10 100 13)))
    (list (%efnl-read bytes :latin-1)
          (%efnl-read bytes '(:latin-1 :newline :lf))
          (%efnl-read bytes '(:latin-1 :newline :crlf))
          (%efnl-read bytes '(:latin-1 :newline :cr))))
  ((97 10 98 13 99 13 10 100 13)
   (97 10 98 13 99 13 10 100 13)
   (97 10 98 13 99 10 100 13)
   (97 10 98 10 99 10 10 100 10)))

;;; CR CR LF is a CR followed by a newline.

(deftest external-format-newline.cr-cr-lf
  (%efnl-read '(97 13 13 10 98) '(:latin-1 :newline :crlf))
  (97 13 10 98))

;;; READ-LINE splits at the line end.

(deftest external-format-newline.read-line
  (progn
    (%efnl-read '(97 13 10 98 13 99 13 10) :latin-1)
    (with-open-file (s (%efnl-path "efnl-in.bin") :external-format '(:latin-1 :newline :crlf))
      (list (read-line s) (char-code (char (read-line s) 1)) (read-line s nil :eof))))
  ("a" 13 :eof))

;;; A CR LF that straddles the reader's 4096-byte block is still one newline.

(deftest external-format-newline.crlf-across-block
  (let* ((bytes (append (make-list 4095 :initial-element 120) '(13 10 121)))
         (codes (%efnl-read bytes '(:latin-1 :newline :crlf))))
    (list (length codes) (nth 4095 codes) (nth 4096 codes)))
  (4097 10 121))

;;; FILE-POSITION counts the bytes of the line end, also when the newline has
;;; been pushed back with UNREAD-CHAR.

(deftest external-format-newline.file-position
  (progn
    (%efnl-read '(97 13 10 98) :latin-1)
    (with-open-file (s (%efnl-path "efnl-in.bin") :external-format '(:latin-1 :newline :crlf))
      (let* ((a (progn (read-char s) (file-position s)))
             (nl (read-char s))
             (b (file-position s)))
        (unread-char nl s)
        (list a b (file-position s) (read-char s) (read-char s) (file-position s)))))
  (1 3 1 #\Newline #\b 4))

;;; A CR on its own under :CRLF: the position after it is past the CR only.

(deftest external-format-newline.lone-cr-position
  (progn
    (%efnl-read '(97 13 98) :latin-1)
    (with-open-file (s (%efnl-path "efnl-in.bin") :external-format '(:latin-1 :newline :crlf))
      (list (read-char s) (char-code (read-char s)) (file-position s) (read-char s))))
  (#\a 13 2 #\b))

;;; :EOL-STYLE is the same option.

(deftest external-format-newline.eol-style
  (list (%efnl-write *efnl-text* '(:latin-1 :eol-style :crlf))
        (%efnl-read '(97 13 10) '(:latin-1 :eol-style :crlf)))
  ((97 13 10 98 13 10) (97 10)))

;;; With the default encoding.

(deftest external-format-newline.default-encoding
  (%efnl-write *efnl-text* '(:default :newline :crlf))
  (97 13 10 98 13 10))

;;; STREAM-EXTERNAL-FORMAT reports the designator as given.

(deftest external-format-newline.stream-external-format
  (with-open-file (s (%efnl-path "efnl-sef.txt") :direction :output :if-exists :supersede
                                                 :external-format '(:latin-1 :newline :crlf))
    (stream-external-format s))
  (:latin-1 :newline :crlf))

;;; FILE-STRING-LENGTH counts the line end and the encoding.

(deftest external-format-newline.file-string-length
  (list (with-open-file (s (%efnl-path "efnl-fsl.txt") :direction :output :if-exists :supersede
                                                       :external-format '(:utf-8 :newline :crlf))
          (file-string-length s (format nil "a~%")))
        (with-open-file (s (%efnl-path "efnl-fsl.txt") :direction :output :if-exists :supersede
                                                       :external-format :latin-1)
          (file-string-length s (string (code-char #xE9)))))
  (3 1))

;;; :IO streams write and read with the line end.

(deftest external-format-newline.io-stream
  (let ((path (%efnl-path "efnl-io.txt")))
    (with-open-file (s path :direction :io :if-exists :supersede
                            :external-format '(:latin-1 :newline :crlf))
      (write-string *efnl-text* s)
      (file-position s 0)
      (list (read-line s) (read-line s) (read-line s nil :eof))))
  ("a" "b" :eof))

;;; FORMAT ~% and TERPRI go through the same path.

(deftest external-format-newline.terpri-and-format
  (let ((path (%efnl-path "efnl-terpri.txt")))
    (with-open-file (s path :direction :output :if-exists :supersede
                            :external-format '(:latin-1 :newline :crlf))
      (write-char #\a s) (terpri s) (format s "b~%") (write-line "c" s)
      (write-sequence (list #\d #\Newline) s))
    (with-open-file (s path :element-type '(unsigned-byte 8))
      (loop for b = (read-byte s nil) while b collect b)))
  (97 13 10 98 13 10 99 13 10 100 13 10))

;;; An unknown line end is an error.

(deftest external-format-newline.bad-value
  (handler-case (progn (%efnl-write "a" '(:latin-1 :newline :bogus)) :no-error)
    (error () :error))
  :error)
