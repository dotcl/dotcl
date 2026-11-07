;;; (directory ".../sub/**/") lists sub/ itself and every directory below it (it listed only the
;;; direct children). A "*/" pattern, a "**/*.lisp" pattern and a plain directory are unchanged.

(defun %dwid-build (root)
  (ensure-directories-exist (format nil "~A/sub/deep/deeper/" root))
  (dolist (f '("sub/a.lisp" "sub/deep/b.lisp" "sub/deep/deeper/c.lisp"))
    (with-open-file (s (format nil "~A/~A" root f) :direction :output :if-exists :supersede)
      (write-string "x" s))))

(defun %dwid-names (root pattern)
  (let ((r (namestring (truename root))))
    (sort (mapcar (lambda (p) (subseq (namestring p) (length r)))
                  (directory (format nil "~A~A" r pattern)))
          #'string<)))

(deftest directory-wild-inferiors-dirs.listing
  (let ((root (format nil "~A/dwid-test" (regression-temp-dir))))
    (%dwid-build root)
    (list (%dwid-names root "sub/**/")
          (%dwid-names root "sub/*/")
          (%dwid-names root "sub/**/*.lisp")
          (%dwid-names root "sub/deep/")))
  (("sub/" "sub/deep/" "sub/deep/deeper/")
   ("sub/deep/")
   ("sub/a.lisp" "sub/deep/b.lisp" "sub/deep/deeper/c.lisp")
   ("sub/deep/")))
