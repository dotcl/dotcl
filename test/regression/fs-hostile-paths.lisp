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
  (let ((m (%fshp-message (lambda () (open "/nonexistent-fshp-dir/x" :direction :output)))))
    (and m (not (search "Could not find" m)) (search "no such file or directory" m) t))
  t)
