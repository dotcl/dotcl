;;; DIRECTORY returns its entries sorted by namestring, by character code.
;;;
;;; The standard leaves the order open, but the file system's enumeration
;;; order differs between OSes and file systems, and libraries depend on the
;;; sorted order SBCL gives. local-time indexes its zone files by pushing them
;;; in DIRECTORY order and answers "GMT" with the first match; its own test
;;; expects "Etc/Greenwich", which is what sorted order produces.
;;;
;;; The files are created out of order so that an unsorted enumeration is
;;; unlikely to come out sorted by accident.

(defun dirsort-root ()
  (let ((root (pathname (regression-temp-file "dirsort-tree/"))))
    (dolist (sub '("m/" "b/" "x/" "a/"))
      (ensure-directories-exist (merge-pathnames sub root)))
    (dolist (name '("GMT-10" "Zulu" "GMT+12" "Greenwich" "GMT" "UTC" "GMT0"
                    "GMT+0" "b.txt" "a.txt" "GMT-1" "Universal"))
      (with-open-file (s (merge-pathnames name root)
                         :direction :output :if-exists :supersede)
        (write-string "x" s)))
    root))

(defun dirsort-sorted-p (paths)
  (let ((names (mapcar #'namestring paths)))
    (and names
         (equal names (sort (copy-list names) #'string<)))))

(deftest directory-files-sorted-by-namestring
  (let ((files (remove-if (lambda (p) (null (pathname-name p)))
                          (directory (merge-pathnames "*.*" (dirsort-root))))))
    (values (length files) (dirsort-sorted-p files)))
  12 t)

(deftest directory-wild-name-sorted-by-namestring
  (let ((files (directory (merge-pathnames "GMT*" (dirsort-root)))))
    (mapcar #'file-namestring files))
  ("GMT" "GMT+0" "GMT+12" "GMT-1" "GMT-10" "GMT0"))

(deftest directory-subdirectories-sorted-by-namestring
  (let ((dirs (directory (merge-pathnames "*/" (dirsort-root)))))
    (values (length dirs) (dirsort-sorted-p dirs)))
  4 t)
