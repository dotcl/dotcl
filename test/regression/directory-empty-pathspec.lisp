;;; (DIRECTORY "") lists *DEFAULT-PATHNAME-DEFAULTS*, not the process
;;; working directory (every other relative pathspec already merged with it).

(deftest directory-empty-pathspec.uses-defaults
  (let* ((dir (format nil "~A/dotcl-dir-empty-~D/" (regression-temp-dir) (random 1000000)))
         (*default-pathname-defaults* (pathname dir)))
    (ensure-directories-exist dir)
    (unwind-protect
         (let ((r (directory "")))
           (list (length r)
                 (string= (string-right-trim "/" (namestring (car r)))
                          (string-right-trim "/" (namestring (truename dir))))))
      (ignore-errors (dotcl:delete-directory dir))))
  (1 t))
