;;; DIRECTORY entries whose names hold * ? or [ print with those characters quoted,
;;; so the namestring reads back as the same non-wild pathname. OPEN of the
;;; unescaped * spelling stays a FILE-ERROR; ordinary names are unchanged.

(defvar *dnw-dir*
  (format nil "~a/dnw-~d/" (regression-temp-dir) (get-internal-real-time)))

(ensure-directories-exist *dnw-dir*)
(dolist (n '("plain.txt" "dot.name.txt" "empty"))
  (dotnet:static "System.IO.File" "WriteAllText"
                 (concatenate 'string *dnw-dir* n) "x"))
(unless (dotnet:static "System.OperatingSystem" "IsWindows")
  (dolist (n '("star*name" "q?mark" "br[a]cket"))
    (dotnet:static "System.IO.File" "WriteAllText"
                   (concatenate 'string *dnw-dir* n) "x")))

(defmacro deftest-unix (name form &rest expected)
  `(unless (dotnet:static "System.OperatingSystem" "IsWindows")
     (deftest ,name ,form ,@expected)))

(deftest directory-namestring-escapes-wild.ordinary-names
  (list (file-namestring (first (directory (concatenate 'string *dnw-dir* "plain.txt"))))
        (file-namestring (first (directory (concatenate 'string *dnw-dir* "dot.name.txt"))))
        (file-namestring (first (directory (concatenate 'string *dnw-dir* "empty")))))
  ("plain.txt" "dot.name.txt" "empty"))

(deftest-unix directory-namestring-escapes-wild.names
  (sort (mapcar #'file-namestring (directory (concatenate 'string *dnw-dir* "*.*")))
        #'string<)
  ("br\\[a]cket" "dot.name.txt" "empty" "plain.txt" "q\\?mark" "star\\*name"))

(deftest-unix directory-namestring-escapes-wild.round-trip
  (let ((rows
          (mapcar (lambda (p)
                    (let ((q (pathname (namestring p))))
                      (list (file-namestring p)
                            (file-namestring q)
                            (and (wild-pathname-p q) t)
                            (and (probe-file q) t)
                            (equal q p))))
                  (directory (concatenate 'string *dnw-dir* "*.*")))))
    (sort rows #'string< :key #'car))
  (("br\\[a]cket" "br\\[a]cket" nil t t)
   ("dot.name.txt" "dot.name.txt" nil t t)
   ("empty" "empty" nil t t)
   ("plain.txt" "plain.txt" nil t t)
   ("q\\?mark" "q\\?mark" nil t t)
   ("star\\*name" "star\\*name" nil t t)))

(deftest-unix directory-namestring-escapes-wild.open-unquoted-wild-is-file-error
  (list (handler-case (open (concatenate 'string *dnw-dir* "star*name"))
          (file-error () :file-error))
        (handler-case (open (concatenate 'string *dnw-dir* "q?mark"))
          (file-error () :file-error)))
  (:file-error :file-error))
