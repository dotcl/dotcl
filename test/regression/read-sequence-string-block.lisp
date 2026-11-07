;;; READ-SEQUENCE into a string reads a block at a time. The characters stored, the
;;; returned position, and the stream position afterwards are those of reading one
;;; character at a time.

(defun %rsb-path () (regression-temp-file (format nil "rsb-~a" (random 1000000))))

(defun %rsb-write (path text &optional (external-format :utf-8))
  (with-open-file (o path :direction :output :if-exists :supersede :external-format external-format)
    (write-string text o))
  path)

(defun %rsb-by-block (path n &key (start 0) end (external-format :utf-8))
  (with-open-file (s path :external-format external-format)
    (let* ((buf (make-string n :initial-element #\?))
           (pos (if end (read-sequence buf s :start start :end end) (read-sequence buf s :start start))))
      (list pos buf (file-position s) (read-char s nil :eof)))))

(defun %rsb-by-char (path n &key (start 0) end (external-format :utf-8))
  (with-open-file (s path :external-format external-format)
    (let* ((buf (make-string n :initial-element #\?))
           (limit (or end n))
           (pos start))
      (loop while (< pos limit)
            do (let ((c (read-char s nil nil)))
                 (if c (progn (setf (char buf pos) c) (incf pos)) (return))))
      (list pos buf (file-position s) (read-char s nil :eof)))))

(defmacro %rsb-same (text n &rest keys)
  `(let ((path (%rsb-write (%rsb-path) ,text ,@(let ((ef (getf keys :external-format))) (and ef (list ef))))))
     (unwind-protect
          (list (equal (%rsb-by-block path ,n ,@keys) (%rsb-by-char path ,n ,@keys))
                (first (%rsb-by-block path ,n ,@keys)))
       (ignore-errors (delete-file path)))))

(deftest read-sequence-block.exact-fit
  (%rsb-same "abcdefghij" 10)
  (t 10))

(deftest read-sequence-block.shorter-than-file
  (%rsb-same "abcdefghij" 4)
  (t 4))

(deftest read-sequence-block.longer-than-file
  (%rsb-same "abcdefghij" 25)
  (t 10))

(deftest read-sequence-block.start-and-end
  (list (%rsb-same "abcdefghij" 10 :start 3)
        (%rsb-same "abcdefghij" 10 :start 2 :end 6)
        (%rsb-same "abcdefghij" 10 :start 5 :end 5)
        (%rsb-same "abcdefghij" 10 :start 10))
  ((t 10) (t 6) (t 5) (t 10)))

(deftest read-sequence-block.empty-file
  (%rsb-same "" 5)
  (t 0))

(deftest read-sequence-block.across-buffer-boundary
  (%rsb-same (make-string 9000 :initial-element #\z) 8500)
  (t 8500))

(deftest read-sequence-block.newlines-and-non-ascii
  (list (%rsb-same (format nil "ab~%cd~%~a~a~%x" (code-char 233) (code-char 12354)) 20)
        (%rsb-same (format nil "a~ab~a" (code-char 12354) (code-char 233)) 4)
        (%rsb-same (format nil "a~ab" (code-char #x1F600)) 8))
  ((t 10) (t 4) (t 5)))

(deftest read-sequence-block.latin-1-and-crlf
  (list (%rsb-same (format nil "caf~a~%x" (code-char 233)) 8 :external-format :latin-1)
        (%rsb-same (format nil "ab~%cd") 8 :external-format '(:utf-8 :eol-style :crlf)))
  ((t 6) (t 5)))

(deftest read-sequence-block.string-stream
  (let ((s (make-string-input-stream "hello world"))
        (buf (make-string 8 :initial-element #\-)))
    (list (read-sequence buf s) (copy-seq buf) (read-char s) (read-sequence buf s :start 2 :end 6) (copy-seq buf)))
  (8 "hello wo" #\r 4 "heldo wo"))

(deftest read-sequence-block.mixed-with-read-char-and-line
  (let ((path (%rsb-write (%rsb-path) (format nil "first line~%second line~%tail"))))
    (unwind-protect
         (with-open-file (s path)
           (let ((buf (make-string 5)))
             (list (read-char s) (read-sequence buf s) (copy-seq buf) (read-line s) (read-sequence buf s) (copy-seq buf))))
      (ignore-errors (delete-file path))))
  (#\f 5 "irst " "line" 5 "secon"))

(deftest read-sequence-block.echo-stream-still-echoes
  (let* ((out (make-string-output-stream))
         (in (make-string-input-stream "abcdef"))
         (echo (make-echo-stream in out))
         (buf (make-string 4)))
    (list (read-sequence buf echo) buf (get-output-stream-string out)))
  (4 "abcd" "abcd"))

(deftest read-sequence-block.closed-and-wrong-direction
  (list (let ((path (%rsb-write (%rsb-path) "abc")))
          (unwind-protect (let ((s (open path))) (close s) (handler-case (read-sequence (make-string 2) s) (error () :error)))
            (ignore-errors (delete-file path))))
        (handler-case (read-sequence (make-string 2) (make-string-output-stream)) (error () :error)))
  (:error :error))
