;;; FILE-POSITION on a character file stream reports where the next character is.
;;;
;;; It used to answer the underlying StreamReader's BaseStream.Position, which is
;;; where the byte BUFFER was filled to -- so one READ-CHAR of a ten-byte file
;;; answered 10. It was right only when the buffer happened to be empty: before
;;; the first read, right after a seek, at end of file. The seek half was already
;;; correct, and takes a byte offset, so the two halves disagreed on what the
;;; number meant.
;;;
;;; The value is a byte offset, which is what SBCL reports and what the seek half
;;; already took. In ASCII that is indistinguishable from a character count; the
;;; UTF-8 tests below are where the two differ.
;;;
;;; Expected values are SBCL's.

(defparameter *fpc-dir*
  (substitute #\/ #\ (or (dotcl:getenv "TMPDIR") (dotcl:getenv "TEMP") "/tmp")))
(defun fpc-path (name) (concatenate 'string *fpc-dir* "/" name))

(defparameter *fpc-ascii* (fpc-path "dotcl-fpc-ascii.txt"))
(defparameter *fpc-utf8* (fpc-path "dotcl-fpc-utf8.txt"))

(with-open-file (o *fpc-ascii* :direction :output :if-exists :supersede)
  (write-string "abcdefghij" o))

;; a=1 byte, the kana are 3 each: 1+3+1+3+1 = 9 bytes for 5 characters.
(with-open-file (o *fpc-utf8* :direction :output :if-exists :supersede
                              :external-format :utf-8)
  (write-string (concatenate 'string "a" (string (code-char #x3042))
                             "b" (string (code-char #x3044)) "c") o))

;;; Reading advances the position, one step per character.

(deftest file-position-characters.advances-with-reading
  (list (with-open-file (s *fpc-ascii*) (file-position s))
        (with-open-file (s *fpc-ascii*) (read-char s) (file-position s))
        (with-open-file (s *fpc-ascii*) (dotimes (i 3) (read-char s)) (file-position s))
        (with-open-file (s *fpc-ascii*) (dotimes (i 10) (read-char s)) (file-position s)))
  (0 1 3 10))

;;; The unit is bytes, not characters: this is the case that tells them apart.

(deftest file-position-characters.counts-bytes-not-characters
  (list (with-open-file (s *fpc-utf8* :external-format :utf-8)
          (read-char s) (file-position s))
        (with-open-file (s *fpc-utf8* :external-format :utf-8)
          (dotimes (i 2) (read-char s)) (file-position s))
        (with-open-file (s *fpc-utf8* :external-format :utf-8)
          (dotimes (i 5) (read-char s)) (file-position s))
        (with-open-file (s *fpc-utf8* :external-format :utf-8) (file-length s)))
  (1 4 9 9))

;;; A character pushed back is not consumed, so the position steps back with it.

(deftest file-position-characters.unread-char-steps-back
  (with-open-file (s *fpc-ascii*) (read-char s) (read-char s) (unread-char #\b s)
    (file-position s))
  1)

;;; Seeking still works, and the position it reports afterwards is the one asked
;;; for -- the two halves have to agree on the unit.

(deftest file-position-characters.seeking-agrees-with-reporting
  (list (with-open-file (s *fpc-ascii*) (file-position s 3) (read-char s))
        (with-open-file (s *fpc-ascii*) (file-position s 3) (file-position s))
        (with-open-file (s *fpc-ascii*) (dotimes (i 5) (read-char s))
          (file-position s 2) (read-char s))
        (with-open-file (s *fpc-ascii*) (read-char s)
          (list (progn (file-position s :start) (file-position s))
                (progn (file-position s :end) (file-position s)))))
  (#\d 3 #\c (0 10)))

;;; Reading itself is unchanged -- the whole LOAD path goes through this reader.

(deftest file-position-characters.reading-is-unchanged
  (list (with-open-file (s *fpc-ascii*) (read-line s))
        (with-open-file (s *fpc-utf8* :external-format :utf-8)
          (let ((chars (loop for c = (read-char s nil nil) while c collect c)))
            (list (length chars) (char-code (second chars)))))
        (with-open-file (s *fpc-ascii*)
          (let ((buf (make-string 4))) (list (read-sequence buf s) buf))))
  ("abcdefghij" (5 #x3042) (4 "abcd")))
