;;; (:utf-8 :replacement #\?) in :EXTERNAL-FORMAT: the replacement character was
;;; parsed and ignored, so an undecodable byte still came back as U+FFFD.

(defun %efr-file ()
  (format nil "~A/dotcl-efr-~D.bin" (regression-temp-dir) (random 1000000)))

(deftest external-format-replacement.decode-utf-8
  (let ((f (%efr-file)))
    (unwind-protect
         (progn
           (with-open-file (s f :direction :output :element-type '(unsigned-byte 8))
             (write-sequence (vector 97 233 98) s))
           (with-open-file (s f :external-format '(:utf-8 :replacement #\?))
             (values (read-line s))))
      (ignore-errors (delete-file f))))
  "a?b")

(deftest external-format-replacement.decode-default-is-fffd
  (let ((f (%efr-file)))
    (unwind-protect
         (progn
           (with-open-file (s f :direction :output :element-type '(unsigned-byte 8))
             (write-sequence (vector 97 233 98) s))
           (with-open-file (s f :external-format :utf-8)
             (map 'list #'char-code (read-line s))))
      (ignore-errors (delete-file f))))
  (97 65533 98))

(deftest external-format-replacement.encode-latin-1
  (let ((f (%efr-file)))
    (unwind-protect
         (progn
           (with-open-file (s f :direction :output :external-format '(:latin-1 :replacement #\_))
             (write-string (coerce (list #\a (code-char 26085) #\b) 'string) s))
           (with-open-file (s f :element-type '(unsigned-byte 8))
             (let ((v (make-array 3))) (read-sequence v s) (coerce v 'list))))
      (ignore-errors (delete-file f))))
  (97 95 98))

;;; An invalid UTF-8 lead byte followed by an ASCII byte: the replacement
;;; character used to come out after the ASCII byte (the ASCII fast path ran
;;; while the decoder still held the lead byte).
(deftest external-format-replacement.order-with-ascii-after-bad-byte
  (let ((f (%efr-file)))
    (unwind-protect
         (progn
           (with-open-file (s f :direction :output :element-type '(unsigned-byte 8))
             (write-sequence (vector 97 233 98 99) s))
           (with-open-file (s f :external-format :utf-8)
             (map 'list #'char-code (values (read-line s)))))
      (ignore-errors (delete-file f))))
  (97 65533 98 99))
