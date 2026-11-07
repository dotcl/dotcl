;;; READ-LINE on a character file takes a line that is wholly in the read buffer and
;;; ASCII straight from the bytes. Every line, MISSING-NEWLINE-P and FILE-POSITION come
;;; out as they do when the line is read a character at a time.


(defun %rlf-path (name) (regression-temp-file (format nil "~a-~a" name (random 1000000))))

(defun %rlf-write (path text &key (external-format :utf-8))
  (with-open-file (o path :direction :output :if-exists :supersede :external-format external-format)
    (write-string text o))
  path)

(defun %rlf-lines-by-line (path &rest open-args)
  (with-open-file (s path :external-format (or (getf open-args :external-format) :utf-8))
    (loop for (l nl) = (multiple-value-list (read-line s nil nil))
          while l collect (list l nl (file-position s)))))

(defun %rlf-lines-by-char (path &rest open-args)
  (with-open-file (s path :external-format (or (getf open-args :external-format) :utf-8))
    (let ((out '()) (acc '()))
      (loop for c = (read-char s nil nil)
            while c
            do (if (char= c #\Newline)
                   (progn (push (list (coerce (nreverse acc) 'string) nil (file-position s)) out)
                          (setf acc '()))
                   (push c acc)))
      (when acc (push (list (coerce (nreverse acc) 'string) t (file-position s)) out))
      (nreverse out))))

(defun %rlf-same (text &rest open-args)
  (let ((path (apply #'%rlf-write (%rlf-path "rlf") text open-args)))
    (unwind-protect
         (list (equal (apply #'%rlf-lines-by-line path open-args)
                      (apply #'%rlf-lines-by-char path open-args))
               (length (apply #'%rlf-lines-by-line path open-args)))
      (ignore-errors (delete-file path)))))

(deftest read-line-file.plain-ascii
  (%rlf-same (format nil "one~%two~%~%four~%five"))
  (t 5))

(deftest read-line-file.ends-with-newline
  (%rlf-same (format nil "a~%b~%"))
  (t 2))

(deftest read-line-file.empty-lines-and-single-char
  (%rlf-same (format nil "~%~%x~%~%"))
  (t 4))

(deftest read-line-file.long-line-spans-buffer
  (%rlf-same (format nil "~a~%short~%~a~%" (make-string 10000 :initial-element #\a)
                     (make-string 5000 :initial-element #\b)))
  (t 3))

(deftest read-line-file.many-short-lines-cross-buffer-edges
  (%rlf-same (with-output-to-string (o) (dotimes (i 2000) (format o "line ~d~%" i))))
  (t 2000))

(deftest read-line-file.non-ascii-lines
  (%rlf-same (format nil "ascii~%caf~a au lait~%~a~%tail" (code-char 233) (code-char 12354)))
  (t 4))

(deftest read-line-file.astral-and-mixed
  (%rlf-same (format nil "a~a~%~ab~%" (code-char #x1F600) (code-char #x1F600)))
  (t 2))

(deftest read-line-file.carriage-returns-stay-in-the-line
  (%rlf-same (format nil "dos~a~%unix~%" #\Return))
  (t 2))

(deftest read-line-file.latin-1
  (%rlf-same (format nil "caf~a~%x~%" (code-char 233)) :external-format :latin-1)
  (t 2))

(deftest read-line-file.crlf-external-format
  (%rlf-same (format nil "one~%two~%") :external-format '(:utf-8 :eol-style :crlf))
  (t 2))

(deftest read-line-file.after-read-char
  (let ((path (%rlf-write (%rlf-path "rlf") (format nil "hello~%world~%last"))))
    (unwind-protect
         (with-open-file (s path)
           (let* ((c (read-char s)) (p (peek-char nil s)))
             (list c p (read-line s) (read-char s) (read-line s) (multiple-value-list (read-line s nil :eof))
                   (read-line s nil :eof))))
      (ignore-errors (delete-file path))))
  (#\h #\e "ello" #\w "orld" ("last" t) :eof))

(deftest read-line-file.eof-handling
  (let ((path (%rlf-write (%rlf-path "rlf") (format nil "only~%"))))
    (unwind-protect
         (with-open-file (s path)
           (list (read-line s) (read-line s nil :eof) (handler-case (read-line s) (end-of-file () :eof-error))))
      (ignore-errors (delete-file path))))
  ("only" :eof :eof-error))

(deftest read-line-file.io-stream
  (let ((path (%rlf-write (%rlf-path "rlf") (format nil "first~%second~%"))))
    (unwind-protect
         (with-open-file (s path :direction :io :if-exists :overwrite)
           (list (read-line s) (read-line s)))
      (ignore-errors (delete-file path))))
  ("first" "second"))

(deftest read-line-file.closed-stream-errors
  (let ((path (%rlf-write (%rlf-path "rlf") (format nil "x~%"))))
    (unwind-protect
         (let ((s (open path)))
           (close s)
           (handler-case (read-line s) (error () :error)))
      (ignore-errors (delete-file path))))
  :error)
