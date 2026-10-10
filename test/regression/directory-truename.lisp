;;; DIRECTORY returns truenames, as SBCL does: an entry reached through a
;;; symbolic link (to its directory, or to the file itself) is listed under
;;; the name of what the link points at, and two links to one file give one
;;; entry. A link to nothing is listed as itself. :RESOLVE-SYMLINKS NIL
;;; (SBCL's extension) lists the entries under the names they were found by.
;;; (Windows links are left alone.)

#-windows
(progn
  (defun %dtn-setup ()
    (let* ((dir (namestring (truename (regression-temp-dir))))
           (root (format nil "~a/dtn/" (string-right-trim "/" dir))))
      (ensure-directories-exist (format nil "~areal/" root))
      (ensure-directories-exist (format nil "~abase/" root))
      (with-open-file (s (format nil "~areal/f.txt" root) :direction :output
                                                         :if-exists :supersede)
        (write-line "x" s))
      (with-open-file (s (format nil "~abase/own.txt" root) :direction :output
                                                           :if-exists :supersede)
        (write-line "y" s))
      (dolist (l '("dirlink" "base/lnk.txt" "base/lnk2.txt" "base/dang.txt"))
        (ignore-errors (dotnet:static "System.IO.File" "Delete" (format nil "~a~a" root l))))
      (dotnet:static "System.IO.Directory" "CreateSymbolicLink"
                     (format nil "~adirlink" root) (format nil "~areal" root))
      (dolist (l '("lnk.txt" "lnk2.txt"))
        (dotnet:static "System.IO.File" "CreateSymbolicLink"
                       (format nil "~abase/~a" root l) (format nil "~areal/f.txt" root)))
      (dotnet:static "System.IO.File" "CreateSymbolicLink"
                     (format nil "~abase/dang.txt" root) (format nil "~anowhere" root))
      root))

  (defun %dtn-names (list root)
    (mapcar (lambda (p)
              (let ((n (namestring p)))
                (if (and (>= (length n) (length root)) (string= root n :end2 (length root)))
                    (subseq n (length root))
                    n)))
            list))

  (deftest directory-truename.through-directory-link
    (let ((root (%dtn-setup)))
      (%dtn-names (directory (format nil "~adirlink/*.txt" root)) root))
    ("real/f.txt"))

  (deftest directory-truename.file-links
    (let ((root (%dtn-setup)))
      (%dtn-names (directory (format nil "~abase/*.txt" root)) root))
    ("base/dang.txt" "base/own.txt" "real/f.txt"))

  (deftest directory-truename.not-wild
    (let ((root (%dtn-setup)))
      (list (%dtn-names (directory (format nil "~abase/lnk.txt" root)) root)
            (%dtn-names (directory (format nil "~adirlink/" root)) root)))
    (("real/f.txt") ("real/")))

  (deftest directory-truename.wild-directories
    (let ((root (%dtn-setup)))
      (%dtn-names (directory (format nil "~a*/" root)) root))
    ("base/" "real/"))

  (deftest directory-truename.resolve-symlinks-nil
    (let ((root (%dtn-setup)))
      (list (%dtn-names (directory (format nil "~abase/lnk*.txt" root) :resolve-symlinks nil)
                        root)
            (%dtn-names (directory (format nil "~adirlink/*.txt" root) :resolve-symlinks nil)
                        root)))
    (("base/lnk.txt" "base/lnk2.txt")
     ("dirlink/f.txt"))))
