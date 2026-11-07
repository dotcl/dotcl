;;; LOAD and COMPILE-FILE read the source in the :EXTERNAL-FORMAT they are given (they
;;; ignored it, so a Latin-1 "café" became U+FFFD). The fasl keeps the characters as read,
;;; and an unsupported format is an error, as for OPEN. Without the option the source is
;;; read as UTF-8 (a byte order mark honoured), as before.

(defvar *sef-char* nil)

(defun %sef-write (name bytes)
  (let ((path (merge-pathnames name (regression-temp-dir))))
    (with-open-file (s path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-sequence (coerce bytes '(vector (unsigned-byte 8))) s))
    path))

(defun %sef-encode (text external-format)
  "The bytes of TEXT (ASCII and one e-acute) in the format: written out by hand, so the
   test does not depend on the code it tests."
  (ecase external-format
    (:latin-1 (map 'list #'char-code text))
    (:utf-8 (loop for ch across text
                  for c = (char-code ch)
                  if (< c 128) collect c
                  else append (list (logior #xC0 (ash c -6)) (logior #x80 (logand c #x3F)))))
    (:utf-16 (append (list #xFF #xFE)
                     (loop for ch across text append (list (char-code ch) 0))))))

(defparameter *sef-source* (format nil "(setq *sef-char* (char-code (char \"caf~Ae\" 3)))~%" (code-char 233)))

(defun %sef-load (name format &rest load-args)
  (setf *sef-char* nil)
  (let ((path (%sef-write name (%sef-encode *sef-source* format))))
    (apply #'load path load-args)
    *sef-char*))

(deftest source-external-format.load-latin-1
  (%sef-load "sef1.lisp" :latin-1 :external-format :latin-1)
  233)

(deftest source-external-format.load-utf-8
  (list (%sef-load "sef2.lisp" :utf-8)
        (%sef-load "sef3.lisp" :utf-8 :external-format :utf-8))
  (233 233))

(deftest source-external-format.load-utf-16
  (list (%sef-load "sef4.lisp" :utf-16 :external-format :utf-16)
        (%sef-load "sef5.lisp" :utf-16))
  (233 233))

(deftest-compiled-only source-external-format.compile-file-latin-1
  (progn
    (setf *sef-char* nil)
    (let* ((src (%sef-write "sef6.lisp" (%sef-encode *sef-source* :latin-1)))
           (fasl (merge-pathnames "sef6.fasl" (regression-temp-dir))))
      (let ((*error-output* (make-broadcast-stream)))
        (compile-file src :output-file fasl :external-format :latin-1))
      (load fasl)
      *sef-char*))
  233)

(deftest source-external-format.unsupported-format-is-an-error
  (let ((src (%sef-write "sef7.lisp" (%sef-encode *sef-source* :utf-8))))
    (list (handler-case (progn (load src :external-format :no-such-format) :no-error) (error () :error))
          (handler-case (let ((*error-output* (make-broadcast-stream)))
                          (compile-file src :external-format :no-such-format
                                            :output-file (merge-pathnames "sef7.fasl" (regression-temp-dir)))
                          :no-error)
            (error () :error))))
  (:error :error))

(deftest source-external-format.default-is-utf-8
  (%sef-load "sef8.lisp" :utf-8)
  233)

;;; A Shift-JIS (code page 932) source: the same path as Latin-1 (the format is parsed by
;;; the code OPEN uses), checked here with the bytes of HIRAGANA A, 82 A0.
(deftest source-external-format.load-cp932
  (progn
    (setf *sef-char* nil)
    (let ((path (%sef-write "sef9.lisp"
                            (append (map 'list #'char-code "(setq *sef-char* (char-code (char \"")
                                    (list #x82 #xA0)
                                    (map 'list #'char-code (format nil "\" 0)))~%"))))))
      (load path :external-format :cp932)
      *sef-char*))
  12354)
