;;; A file name the host cannot hold (a NUL character, an empty name for OPEN
;;; :OUTPUT) is a FILE-ERROR whose text names the Lisp pathname, never host
;;; library wording; DIRECTORY of such a name finds nothing.

(defun %fshp-nul-name ()
  (format nil "a~Cb" (code-char 0)))

(defun %fshp-message (thunk)
  (handler-case (progn (funcall thunk) nil)
    (file-error (e) (princ-to-string e))))

(deftest fs-hostile-paths.directory-nul
  (directory (%fshp-nul-name))
  nil)

(deftest fs-hostile-paths.open-output-nul
  (let ((m (%fshp-message (lambda () (open (%fshp-nul-name) :direction :output)))))
    (and m (not (search "Parameter" m)) (search "OPEN:" m) t))
  t)

(deftest fs-hostile-paths.open-output-empty
  (let ((m (%fshp-message (lambda () (open "" :direction :output)))))
    (and m (not (search "Parameter" m)) t))
  t)

(deftest fs-hostile-paths.rename-nul
  (let ((m (%fshp-message (lambda () (rename-file (%fshp-nul-name) "zz-fshp-target")))))
    (and m (not (search "Parameter" m)) (search "RENAME-FILE:" m) t))
  t)

(deftest fs-hostile-paths.open-missing-dir-wording
  (let ((m (%fshp-message (lambda () (open (concatenate 'string (regression-temp-dir/) "nonexistent-fshp-dir/x")
                                                    :direction :output)))))
    (and m (not (search "Could not find" m)) (search "no such file or directory" m) t))
  t)

(deftest fs-hostile-paths.ensure-directories-exist-nul
  (let ((m (%fshp-message (lambda () (ensure-directories-exist
                                      (format nil "~C/x/y" (code-char 0)))))))
    (and m (not (search "Parameter" m)) (not (search "Null character" m))
         (search "ENSURE-DIRECTORIES-EXIST:" m) t))
  t)

;;; A directory cannot be made below a plain file, on any host. (The path has to
;;; fail for sure: a made-up absolute path can be creatable, and a test must not
;;; create directories outside its scratch directory.)
(deftest fs-hostile-paths.ensure-directories-exist-below-a-file
  (let ((file (format nil "~a/fshp-plain-file" (regression-temp-dir))))
    (with-open-file (s file :direction :output :if-exists :supersede)
      (write-line "x" s))
    (let ((m (%fshp-message (lambda () (ensure-directories-exist (format nil "~a/sub/x" file))))))
      (ignore-errors (delete-file file))
      (and m
           (eql 0 (search "ENSURE-DIRECTORIES-EXIST:" m))
           (not (search " : '" m))
           (not (search "(Parameter" m))
           t)))
  t)
