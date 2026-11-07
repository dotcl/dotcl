;;; READ-CHAR, PEEK-CHAR and READ-LINE take a character input stream (CLHS); an
;;; (unsigned-byte 8) stream is a type-error, as in SBCL and ECL, instead of
;;; reading the bytes as characters. A character stream, and one opened with
;;; :element-type :default, still read characters.

(defun %bsc-file (name bytes &optional (type 'character))
  (let ((path (regression-temp-file name)))
    (with-open-file (o path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (dolist (b bytes) (write-byte b o)))
    (values path type)))

(deftest binary-stream-character-read.rejected
  (let ((path (%bsc-file "binary-stream-character-read.bin" '(65 66 10 67))))
    (flet ((try (fn)
             (with-open-file (s path :element-type '(unsigned-byte 8))
               (handler-case (funcall fn s) (type-error () :type-error)))))
      (list (try #'read-char) (try (lambda (s) (peek-char nil s)))
            (try #'read-line) (try #'read-byte))))
  (:type-error :type-error :type-error 65))

(deftest binary-stream-character-read.character-streams-unchanged
  (let ((path (%bsc-file "binary-stream-character-read2.bin" '(65 66 10 67))))
    (list (with-open-file (s path) (list (read-char s) (peek-char nil s) (read-line s)))
          (with-open-file (s path :element-type :default) (list (read-char s) (read-line s)))))
  ((#\A #\B "B") (#\A "B")))

(deftest binary-stream-character-read.composite
  (let ((path (%bsc-file "binary-stream-character-read3.bin" '(65 66))))
    (with-open-file (s path :element-type '(unsigned-byte 8))
      (handler-case (read-char (make-two-way-stream s (make-broadcast-stream)))
        (type-error () :type-error))))
  :type-error)
