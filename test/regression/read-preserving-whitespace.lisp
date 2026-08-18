;;; READ-PRESERVING-WHITESPACE leaves the delimiter in the stream.
;;;
;;; The implementation carried a note saying this was "best-effort: works for
;;; string streams where Peek works". It holds for file and concatenated
;;; streams too -- these tests pin that, so the note does not have to be taken
;;; on trust.

(defun %rpw-rest (stream)
  (with-output-to-string (out)
    (loop for c = (read-char stream nil nil) while c do (write-char c out))))

(defun %rpw-string (text)
  (let ((s (make-string-input-stream text)))
    (let ((o (read-preserving-whitespace s))) (list o (%rpw-rest s)))))

(defun %rpw-file (text)
  (let ((p (format nil "rpw-test-~a.txt" (get-internal-real-time))))
    (with-open-file (out p :direction :output :if-exists :supersede)
      (write-string text out))
    (unwind-protect
         (with-open-file (in p)
           (let ((o (read-preserving-whitespace in))) (list o (%rpw-rest in))))
      (delete-file p))))

(deftest read-preserving-whitespace.string-stream
  (list (%rpw-string "abc def") (%rpw-string "(a b) x") (%rpw-string "\"s\" y"))
  ((abc " def") ((a b) " x") ("s" " y")))

(deftest read-preserving-whitespace.file-stream
  (list (%rpw-file "abc def") (%rpw-file "42	z"))
  ((abc " def") (42 "	z")))

;; A newline is whitespace too, and a terminating macro character is not
;; consumed either way.
(deftest read-preserving-whitespace.newline-and-terminating-char
  (list (%rpw-string (format nil "abc~%def")) (%rpw-string "abc)rest"))
  ((abc #.(format nil "~%def")) (abc ")rest")))

(deftest read-preserving-whitespace.concatenated-stream
  (let* ((a (make-string-input-stream "abc "))
         (b (make-string-input-stream "def"))
         (c (make-concatenated-stream a b)))
    (let ((o (read-preserving-whitespace c))) (list o (%rpw-rest c))))
  (abc " def"))

;; The contrast that makes it meaningful: plain READ eats the delimiter.
(deftest read-preserving-whitespace.read-consumes-delimiter
  (let ((s (make-string-input-stream "abc def")))
    (let ((o (read s))) (list o (%rpw-rest s))))
  (abc "def"))
