;;; RENAME-FILE with a wild new name, and ENSURE-DIRECTORIES-EXIST where a part of the
;;; path is an ordinary file, signal FILE-ERROR (SBCL), as a FILE-ERROR handler expects.
;;; RENAME-FILE made a file called "*.lisp", and ENSURE-DIRECTORIES-EXIST signalled a
;;; PROGRAM-ERROR. Ordinary renames and directory creation are unchanged.

(defun %few-dir ()
  (format nil "~a/few/" (regression-temp-dir)))

(defun %few-write (path text)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede) (write-string text s))
  path)

(deftest file-error-wild-and-blocked.rename-wild
  (let ((from (%few-write (concatenate 'string (%few-dir) "r1.txt") "a")))
    (list (handler-case (rename-file from (concatenate 'string (%few-dir) "*.lisp")) (file-error () :file-error))
          (handler-case (rename-file from (concatenate 'string (%few-dir) "r1.*")) (file-error () :file-error))
          (and (probe-file from) t)))
  (:file-error :file-error t))

(deftest file-error-wild-and-blocked.rename-ordinary
  (let* ((from (%few-write (concatenate 'string (%few-dir) "r2.txt") "a"))
         (to (concatenate 'string (%few-dir) "r3.txt")))
    (rename-file from to)
    (list (and (probe-file to) t) (null (probe-file from))))
  (t t))

(deftest file-error-wild-and-blocked.blocked-directory
  (let ((blocker (%few-write (concatenate 'string (%few-dir) "blocker") "x")))
    (handler-case (ensure-directories-exist (concatenate 'string blocker "/sub/x.txt"))
      (file-error () :file-error)))
  :file-error)

(deftest file-error-wild-and-blocked.new-directories-still-made
  (let ((p (concatenate 'string (%few-dir) "n1/n2/x.txt")))
    (multiple-value-bind (path created) (ensure-directories-exist p)
      (list (equal path p) created
            (nth-value 1 (ensure-directories-exist p)))))
  (t t nil))
