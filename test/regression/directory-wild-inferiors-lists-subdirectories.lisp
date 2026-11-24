;;; DIRECTORY with a wild directory component (** or *) and a wild name lists the
;;; subdirectories as well as the files, as the same name pattern does one level
;;; down.

(defun %dwi-rel (list base)
  (sort (mapcar (lambda (p) (subseq (namestring p) (length (namestring base)))) list) #'string<))

(deftest directory-wild-inferiors-lists-subdirectories.listing
  (let* ((base (pathname (concatenate 'string (string-right-trim "/" (namestring (regression-temp-dir))) "/dwi/"))))
    (ensure-directories-exist (merge-pathnames "a/b/c/x" base))
    (dolist (f '("a/b/g.txt" "a/h.txt"))
      (with-open-file (o (merge-pathnames f base) :direction :output :if-exists :supersede) (write-line "x" o)))
    (let ((a (truename (merge-pathnames "a/" base))))
      (flet ((rel (pat) (%dwi-rel (directory (merge-pathnames pat a)) (truename base))))
        (list (rel "*.*") (rel "**/*.*") (rel "**/*.txt") (rel "*/*.*") (rel "**/*")))))
  (("a/b/" "a/h.txt")
   ("a/b/" "a/b/c/" "a/b/g.txt" "a/h.txt")
   ("a/b/g.txt" "a/h.txt")
   ("a/b/c/" "a/b/g.txt")
   ("a/b/" "a/b/c/")))
