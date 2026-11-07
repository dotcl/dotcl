;;; String and file streams keep the state that few streams use (element type, external
;;; format, the reader cached for READ, the #n= labels) apart from the stream itself;
;;; creation, reuse, :element-type, column and the line-start flag behave as before.

(defun %ssc-path () (regression-temp-file (format nil "ssc-~a" (random 1000000))))

(deftest string-stream-creation.output-basics
  (let ((o (make-string-output-stream)))
    (write-string "abc" o)
    (list (stream-element-type o) (output-stream-p o) (input-stream-p o) (open-stream-p o)
          (get-output-stream-string o) (get-output-stream-string o)
          (progn (write-char #\x o) (get-output-stream-string o))
          (progn (close o) (open-stream-p o))))
  (character t nil t "abc" "" "x" nil))

(deftest string-stream-creation.element-type
  (list (stream-element-type (make-string-output-stream :element-type 'character))
        (stream-element-type (make-string-output-stream :element-type 'base-char))
        (stream-element-type (make-string-input-stream "abc"))
        (type-of (let ((o (make-string-output-stream :element-type 'base-char))) (write-string "ab" o) (get-output-stream-string o)))
        (handler-case (let ((o (make-string-output-stream :element-type 'base-char))) (write-char (code-char 955) o) :no-error)
          (error () :error)))
  (character character character (simple-base-string 2) :error))

(deftest string-stream-creation.column-and-line-start
  (let ((o (make-string-output-stream)))
    (list (fresh-line o) (progn (write-string "ab" o) (fresh-line o)) (fresh-line o)
          (format o "~&x~10t|~%y") (get-output-stream-string o)))
  (nil t nil nil "ab
x         |
y"))

(deftest string-stream-creation.with-output-to-string
  (list (with-output-to-string (s) (write-string "a" s) (princ 12 s))
        (with-output-to-string (*standard-output*) (princ "in") (terpri) (fresh-line) (write-string "z"))
        (with-output-to-string (s nil :element-type 'base-char) (write-string "q" s))
        (let ((fp (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
          (with-output-to-string (s fp) (write-string "fp" s))
          fp)
        (let ((*standard-output* *standard-output*)) (with-output-to-string (s) (format s "~a-~s" 'a "b"))))
  ("a12" "in
z" "q" "fp" "A-\"b\""))

(deftest string-stream-creation.input-and-read
  ;; READ keeps its reader and #n= labels with the stream
  (let ((i (make-string-input-stream "(#1=(a) #1#) foo (b) (#2=(c) #2#) \"s\" 12")))
    (let* ((a (read i)) (b (read i)) (c (read i)) (d (read i)) (e (read i)) (f (read i)))
      (list (eq (first a) (second a)) b c (eq (first d) (second d)) e f (read i nil :eof)
            (stream-element-type i) (input-stream-p i) (output-stream-p i))))
  (t foo (b) t "s" 12 :eof character t nil))

(deftest string-stream-creation.file-streams
  (let ((path (%ssc-path)))
    (unwind-protect
         (progn
           (with-open-file (o path :direction :output :if-exists :supersede :external-format :utf-8)
             (list (stream-element-type o) (stream-external-format o)) 
             (print '(#1=(x) #1#) o) (princ "tail" o))
           (list (with-open-file (b path :element-type '(unsigned-byte 8)) (stream-element-type b))
                 (with-open-file (i path :external-format :utf-8)
                   (let ((x (read i))) (list (eq (first x) (second x)) (stream-external-format i) (read-line i nil) (read-line i nil))))
                 (with-open-file (o2 path :direction :output :if-exists :append) (stream-element-type o2))))
      (ignore-errors (delete-file path))))
  ((unsigned-byte 8) (nil :utf-8 " tail" nil) character))
