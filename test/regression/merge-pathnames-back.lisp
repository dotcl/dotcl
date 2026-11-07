;;; :BACK in MERGE-PATHNAMES and in the namestring.
;;;
;;; CLHS MERGE-PATHNAMES: a relative directory is appended to the default's,
;;; and a :BACK that follows a directory name then goes away together with
;;; that name.  The merge used to keep it, and the namestring then dropped
;;; it, so (merge-pathnames #P"../" #P"/a/b/") named /a/b/ instead of /a/:
;;; ASDF's test harness finds its own tree this way and stopped at its first
;;; form.  :UP is left alone by the merge; it is resolved by the file system.

(defun mpb-dir (relative default)
  (pathname-directory
   (merge-pathnames (make-pathname :directory (cons :relative relative))
                    (make-pathname :directory (cons :absolute default)))))

(deftest merge-pathnames-back.folds-into-the-name-before-it
  (mpb-dir '(:back) '("mpb" "a" "b"))
  (:absolute "mpb" "a"))

(deftest merge-pathnames-back.then-descends
  (mpb-dir '(:back "c") '("mpb" "a" "b"))
  (:absolute "mpb" "a" "c"))

(deftest merge-pathnames-back.twice
  (mpb-dir '(:back :back "c") '("mpb" "a" "b"))
  (:absolute "mpb" "c"))

(deftest merge-pathnames-back.up-is-kept
  (mpb-dir '(:up "c") '("mpb" "a" "b"))
  (:absolute "mpb" "a" "b" :up "c"))

(deftest merge-pathnames-back.relative-default-without-names-keeps-it
  (pathname-directory
   (merge-pathnames (make-pathname :directory '(:relative :back))
                    (make-pathname :directory '(:relative))))
  (:relative :back))

;;; A :BACK that is still there names the parent, as :UP does.
(deftest merge-pathnames-back.namestring-writes-it
  (namestring (make-pathname :directory '(:absolute "mpb" "a" "b" :back)))
  "/mpb/a/b/../")

;;; Checked against SBCL 2.6.8.
(deftest merge-pathnames-back.with-name-into-string-default
  (namestring (merge-pathnames (make-pathname :directory '(:relative :back "a")
                                              :name "f")
                               "/x/y/z/"))
  "/x/y/a/f")

(deftest merge-pathnames-back.relative-default-folds
  (pathname-directory
   (merge-pathnames (make-pathname :directory '(:relative :back "a"))
                    (make-pathname :directory '(:relative "x" "y"))))
  (:relative "x" "a"))

(deftest merge-pathnames-back.relative-default-consumed
  (pathname-directory
   (merge-pathnames (make-pathname :directory '(:relative :back))
                    (make-pathname :directory '(:relative "x"))))
  (:relative))

(deftest merge-pathnames-back.after-wild-inferiors-folds-the-name-before-it
  (pathname-directory
   (merge-pathnames (make-pathname :directory '(:relative :back "a"))
                    (make-pathname :directory '(:relative "x" :wild-inferiors "y"))))
  (:relative "x" :wild-inferiors "a"))

(deftest merge-pathnames-back.absolute-argument-wins
  (pathname-directory
   (merge-pathnames (make-pathname :directory '(:absolute "q"))
                    (make-pathname :directory '(:absolute "x" "y"))))
  (:absolute "q"))

(deftest merge-pathnames-back.nil-directory-takes-the-default
  (pathname-directory
   (merge-pathnames (make-pathname :name "f")
                    (make-pathname :directory '(:absolute "x" "y"))))
  (:absolute "x" "y"))
