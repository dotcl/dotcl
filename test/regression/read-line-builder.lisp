;;; READ-LINE collects a line in a builder that is kept between calls. Lines of every length, the
;;; missing-newline flag, end of file, and streams that read another line while one is being read.

(defun %rlb-lines (text)
  (with-input-from-string (s text)
    (loop for (line missing) = (multiple-value-list (read-line s nil :eof))
          until (eq line :eof) collect (list line missing))))

(deftest read-line-builder.basics
  (list (%rlb-lines "a
bb

ccc")
        (%rlb-lines "x
")
        (%rlb-lines "")
        (%rlb-lines "no newline")
        (%rlb-lines "

"))
  (( ("a" nil) ("bb" nil) ("" nil) ("ccc" t))
   (("x" nil))
   nil
   (("no newline" t))
   (("" nil) ("" nil))))

(deftest read-line-builder.long-lines
  (let* ((long (make-string 20000 :initial-element #\q))
         (text (format nil "~A~%short~%~A" long long))
         (lines (%rlb-lines text)))
    (list (mapcar (lambda (l) (length (first l))) lines) (mapcar #'second lines)
          (mapcar #'first (%rlb-lines (format nil "~A~%tiny~%" (make-string 9000 :initial-element #\z))))
          ))
  ((20000 5 20000) (nil nil t)
   (#.(make-string 9000 :initial-element #\z) "tiny")))

(deftest read-line-builder.eof-handling
  (with-input-from-string (s "only")
    (list (multiple-value-list (read-line s))
          (handler-case (read-line s) (end-of-file () :eof-error))
          (read-line s nil :dflt) (multiple-value-list (read-line s nil nil))))
  (("only" t) :eof-error :dflt (nil t)))

(deftest read-line-builder.concatenated-and-two-way
  (let* ((a (make-string-input-stream "ab
cd"))
         (b (make-string-input-stream "ef
gh
"))
         (cat (make-concatenated-stream a b))
         (out (make-string-output-stream))
         (two (make-two-way-stream (make-string-input-stream "12
34") out))
         (echo-out (make-string-output-stream))
         (echo (make-echo-stream (make-string-input-stream "xy
z") echo-out)))
    (list (read-line cat) (read-line cat) (read-line cat) (read-line cat nil :end)
          (read-line two) (read-line two) (read-line echo) (get-output-stream-string echo-out)))
  ("ab" "cdef" "gh" :end "12" "34" "xy" "xy
"))

(deftest read-line-builder.file
  (let ((path "test/regression/.tmp-read-line-builder.txt"))
    (with-open-file (o path :direction :output :if-exists :supersede)
      (format o "first line~%second, a bit longer line~%~%last no newline"))
    (prog1 (with-open-file (s path)
             (loop for (line missing) = (multiple-value-list (read-line s nil :eof))
                   until (eq line :eof) collect (list line missing)))
      (delete-file path)))
  (("first line" nil) ("second, a bit longer line" nil) ("" nil) ("last no newline" t)))

(require "dotcl-gray")
(defclass %rlb-gray (dotcl-gray:fundamental-character-input-stream)
  ((n :initform 0)))
(defvar *rlb-inner* nil)
(defmethod dotcl-gray:stream-read-char ((s %rlb-gray))
  (with-slots (n) s
    (incf n)
    ;; the first character read asks another stream for a whole line, in the middle of this one
    (when (= n 1) (setq *rlb-inner* (with-input-from-string (i "inner line
") (read-line i))))
    (cond ((<= n 3) (char "abc" (1- n))) ((= n 4) #\Newline) (t :eof))))
(defmethod dotcl-gray:stream-unread-char ((s %rlb-gray) c) (declare (ignore c)) (with-slots (n) s (decf n)) nil)

(deftest read-line-builder.reads-another-line-meanwhile
  (let* ((echo-out (make-string-output-stream))
         (e (make-echo-stream (make-instance '%rlb-gray) echo-out)))
    (list (read-line e) *rlb-inner*))
  ("abc" "inner line"))
