;;; A file whose name holds * or ? (not possible on Windows): DIRECTORY, PROBE-FILE
;;; and TRUENAME answer a non-wild pathname whose namestring quotes the character
;;; and reads back as the same file; OPEN of the unquoted spelling is a FILE-ERROR
;;; (a wild pathname), of the quoted spelling opens the file.

(defvar *sq-dir*
  (format nil "~a/star-q-~d/" (regression-temp-dir) (get-internal-real-time)))

(unless (dotnet:static "System.OperatingSystem" "IsWindows")
  (ensure-directories-exist *sq-dir*)
  (dolist (n '("star*name" "q?mark"))
    (dotnet:static "System.IO.File" "WriteAllText"
                   (concatenate 'string *sq-dir* n) "x")))

(defmacro deftest-unix (name form &rest expected)
  `(unless (dotnet:static "System.OperatingSystem" "IsWindows")
     (deftest ,name ,form ,@expected)))

(deftest-unix star-names.directory-not-wild
  (let ((ps (directory (concatenate 'string *sq-dir* "*"))))
    (list (length ps) (some #'wild-pathname-p ps)))
  (2 nil))

(deftest-unix star-names.namestring-quoted
  (sort (mapcar (lambda (p) (file-namestring p)) (directory (concatenate 'string *sq-dir* "*")))
        #'string<)
  ("q\\?mark" "star\\*name"))

(deftest-unix star-names.round-trip
  (let ((ps (directory (concatenate 'string *sq-dir* "*"))))
    (every (lambda (p) (and (equal (pathname (namestring p)) p)
                            (probe-file p)
                            (equal (namestring (truename p)) (namestring p))))
           ps))
  t)

(deftest-unix star-names.open-quoted
  (with-open-file (s (concatenate 'string *sq-dir* "star\\*name")) (values (read-line s)))
  "x")

(deftest-unix star-names.open-unquoted-is-file-error
  (handler-case (open (concatenate 'string *sq-dir* "star*name"))
    (file-error () :file-error))
  :file-error)

(deftest-unix star-names.open-wild-pathname-is-file-error
  (handler-case (open (make-pathname :name :wild :defaults *sq-dir*))
    (file-error () :file-error))
  :file-error)
