;;; TRUENAME and PROBE-FILE resolve symbolic links along the path, as SBCL
;;; does: the truename of a file reached through a link to its directory, or
;;; through a link to the file itself, is the file's own name. A link to
;;; nothing is its own truename. (Windows links are left alone.)

#-windows
(progn
  (defun %tsl-setup ()
    (let* ((dir (regression-temp-dir))
           (real (format nil "~a/tsl-real" dir)))
      (ensure-directories-exist (format nil "~a/" real))
      (with-open-file (s (format nil "~a/f.txt" real) :direction :output
                                                       :if-exists :supersede)
        (write-line "x" s))
      (dolist (l '("tsl-dirlink" "tsl-filelink" "tsl-dangling"))
        (ignore-errors (dotnet:static "System.IO.File" "Delete" (format nil "~a/~a" dir l))))
      (dotnet:static "System.IO.Directory" "CreateSymbolicLink"
                     (format nil "~a/tsl-dirlink" dir) real)
      (dotnet:static "System.IO.File" "CreateSymbolicLink"
                     (format nil "~a/tsl-filelink" dir) (format nil "~a/f.txt" real))
      (dotnet:static "System.IO.File" "CreateSymbolicLink"
                     (format nil "~a/tsl-dangling" dir) (format nil "~a/nowhere" dir))
      dir))

  (deftest truename-symlink.resolved
    (let* ((dir (%tsl-setup))
           (real (namestring (truename (format nil "~a/tsl-real/f.txt" dir)))))
      (list (equal (namestring (truename (format nil "~a/tsl-dirlink/f.txt" dir))) real)
            (equal (namestring (probe-file (format nil "~a/tsl-dirlink/f.txt" dir))) real)
            (equal (namestring (truename (format nil "~a/tsl-filelink" dir))) real)
            (equal (namestring (probe-file (format nil "~a/tsl-filelink" dir))) real)
            (equal (namestring (truename (format nil "~a/tsl-dirlink/" dir)))
                   (namestring (truename (format nil "~a/tsl-real/" dir))))
            (with-open-file (s (format nil "~a/tsl-dirlink/f.txt" dir))
              (equal (namestring (truename s)) real))))
    (t t t t t t))

  (deftest truename-symlink.dangling-link-is-itself
    (let* ((dir (%tsl-setup))
           (tn (probe-file (format nil "~a/tsl-dangling" dir))))
      (and tn (string= (file-namestring tn) "tsl-dangling") t))
    t))
