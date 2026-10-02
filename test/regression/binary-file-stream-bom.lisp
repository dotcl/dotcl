;;; Regression: an input file stream with a binary :ELEMENT-TYPE dropped a
;;; leading byte order mark (EF BB BF, FF FE, FE FF, FF FE 00 00). The byte
;;; order mark sniffing meant for character streams also ran on the binary
;;; path, so READ-BYTE / READ-SEQUENCE lost those bytes and FILE-POSITION
;;; started past them. Character streams still skip the mark.

(defun bfsb-write (bytes)
  (let ((path (regression-temp-file "binary-file-stream-bom.bin")))
    (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (dolist (b bytes) (write-byte b s)))
    path))

(defun bfsb-read-bytes (bytes type)
  (with-open-file (s (bfsb-write bytes) :element-type type)
    (list (file-length s) (file-position s)
          (loop for b = (read-byte s nil) while b collect b)
          (file-position s))))

(defun bfsb-read-sequence (bytes)
  (with-open-file (s (bfsb-write bytes) :element-type '(unsigned-byte 8))
    (let ((v (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
      (list (read-sequence v s) (coerce v 'list) (file-position s)))))

(deftest binary-file-stream-bom-utf8-read-byte
  (bfsb-read-bytes '(#xEF #xBB #xBF 65 66) '(unsigned-byte 8))
  (5 0 (#xEF #xBB #xBF 65 66) 5))

(deftest binary-file-stream-bom-utf16-read-byte
  (list (bfsb-read-bytes '(#xFF #xFE 65 66) '(unsigned-byte 8))
        (bfsb-read-bytes '(#xFE #xFF 65 66) '(unsigned-byte 8))
        (bfsb-read-bytes '(#xFF #xFE 0 0 65) '(unsigned-byte 8)))
  ((4 0 (#xFF #xFE 65 66) 4)
   (4 0 (#xFE #xFF 65 66) 4)
   (5 0 (#xFF #xFE 0 0 65) 5)))

(deftest binary-file-stream-bom-read-sequence
  (bfsb-read-sequence '(#xEF #xBB #xBF 65 66))
  (5 (#xEF #xBB #xBF 65 66 0 0 0) 5))

(deftest binary-file-stream-bom-signed-byte-8
  (bfsb-read-bytes '(#xEF #xBB #xBF 65) '(signed-byte 8))
  (4 0 (-17 -69 -65 65) 4))

(deftest binary-file-stream-bom-unsigned-byte-16
  (bfsb-read-bytes '(#xFF #xFE 1 0) '(unsigned-byte 16))
  (2 0 (#xFEFF 1) 2))

(deftest binary-file-stream-bom-file-position-seek
  (with-open-file (s (bfsb-write '(#xEF #xBB #xBF 65)) :element-type '(unsigned-byte 8))
    (list (read-byte s) (file-position s)
          (file-position s 0) (read-byte s)
          (file-position s 2) (read-byte s) (file-position s)))
  (#xEF 1 t #xEF t #xBF 3))

(deftest binary-file-stream-bom-io
  (with-open-file (s (bfsb-write '(#xEF #xBB #xBF 65)) :direction :io
                     :if-exists :overwrite :element-type '(unsigned-byte 8))
    (list (read-byte s) (file-position s)))
  (#xEF 1))

;; Character streams keep skipping a UTF-8 mark: the first character is the
;; one after it, and FILE-POSITION counts the mark's bytes.
(deftest binary-file-stream-bom-character-still-skips
  (list (with-open-file (s (bfsb-write '(#xEF #xBB #xBF 65 66)))
          (list (read-char s) (file-position s) (read-line s)))
        (with-open-file (s (bfsb-write '(#xEF #xBB #xBF 65 66)) :element-type :default)
          (read-line s)))
  ((#\A 4 "B") "AB"))
