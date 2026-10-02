;;; A :WILD-INFERIORS ("**") directory component matches zero or more
;;; directories (CLHS 19.2.2.4.3). DIRECTORY matched exactly one level, so
;;; (directory "build/**/*.html") missed build/index.html and found only files
;;; one directory further down. Found while checking the output of a static
;;; site generator's example build with such a pattern. The values below are
;;; what SBCL returns for the same tree.

(defun %dwi-root ()
  (let ((root (merge-pathnames "dwi-tree/" (regression-temp-dir))))
    (dolist (f '("top.html" "a/mid.html" "a/b/deep.html" "a/b/note.txt"))
      (let ((p (merge-pathnames f root)))
        (ensure-directories-exist p)
        (with-open-file (s p :direction :output :if-exists :supersede)
          (write-string "x" s))))
    root))

(defun %dwi-names (pattern)
  (let ((root (%dwi-root)))
    (sort (mapcar (lambda (p) (enough-namestring p root))
                  (directory (merge-pathnames pattern root)))
          #'string<)))

(deftest directory-wild-inferiors-zero-levels
  (list (%dwi-names "**/*.html")
        (%dwi-names "a/**/*.html")
        (%dwi-names "a/b/**/*.*")
        (%dwi-names "**/b/*.txt"))
  (("a/b/deep.html" "a/mid.html" "top.html")
   ("a/b/deep.html" "a/mid.html")
   ("a/b/deep.html" "a/b/note.txt")
   ("a/b/note.txt")))
