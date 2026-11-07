;;; DELETE-FILE failing says why: a file that is not there, or a directory.

(defun %dfr-message (path)
  (handler-case (progn (delete-file path) nil)
    (file-error (e) (princ-to-string e))))

(deftest delete-file-reason.missing-file
  (let ((m (%dfr-message (regression-temp-file "dfr-missing.txt"))))
    (and (search "could not delete" m)
         (search ": no such file or directory" m)
         t))
  t)

(deftest delete-file-reason.file-form-directory
  (let* ((dir (regression-temp-file "dfr-dir"))
         (m (progn (ensure-directories-exist (concatenate 'string dir "/"))
                   (%dfr-message dir))))
    (and (search ": is a directory" m) t))
  t)
