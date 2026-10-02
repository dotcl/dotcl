;;; DELETE-FILE given a file-form pathname ("x.tmp") that names a directory
;;; signals FILE-ERROR and leaves the directory alone, as SBCL does. It used to
;;; delete the directory. tmpdir makes its directory at the path of UIOP's
;;; temporary file, and UIOP then deletes "the file" by that name, which took
;;; the fresh directory away. A directory-form pathname ("x.tmp/") still
;;; deletes an empty directory.

(defun %dfdf-paths ()
  (let ((file (concatenate 'string (regression-temp-dir) "/dfdf-d.tmp")))
    (values file (concatenate 'string file "/"))))

(deftest delete-file-directory-file-form.file-form-signals
  (multiple-value-bind (file dir) (%dfdf-paths)
    (ensure-directories-exist dir)
    (list (handler-case (progn (delete-file file) :deleted)
            (file-error () :file-error))
          (and (probe-file dir) t)))
  (:file-error t))

(deftest delete-file-directory-file-form.directory-form-deletes
  (multiple-value-bind (file dir) (%dfdf-paths)
    (declare (ignore file))
    (ensure-directories-exist dir)
    (list (delete-file dir) (and (probe-file dir) t)))
  (t nil))
