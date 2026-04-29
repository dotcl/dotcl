;;; pathname.lisp — ANSI tests for pathnames

;;; --- pathname ---

(deftest pathname.1
  (pathnamep (pathname "foo.lisp"))
  t)

(deftest pathname.2
  (pathnamep (pathname "/tmp/foo.lisp"))
  t)

;;; --- namestring ---

(deftest namestring.1
  (namestring (pathname "foo.lisp"))
  "foo.lisp")

(deftest namestring.2
  (namestring (pathname "/tmp/foo.lisp"))
  "/tmp/foo.lisp")

;;; --- pathname-name ---

(deftest pathname-name.1
  (pathname-name (pathname "foo.lisp"))
  "foo")

(deftest pathname-name.2
  (pathname-name (pathname "/tmp/bar.txt"))
  "bar")

;;; --- pathname-type ---

(deftest pathname-type.1
  (pathname-type (pathname "foo.lisp"))
  "lisp")

(deftest pathname-type.2
  (pathname-type (pathname "/tmp/bar.txt"))
  "txt")

;;; --- pathname-directory ---

(deftest pathname-directory.1
  (let ((d (pathname-directory (pathname "/tmp/foo.lisp"))))
    (and (consp d) (eq (car d) :absolute)))
  t)

(deftest pathname-directory.2
  (pathname-directory (pathname "foo.lisp"))
  nil)

(deftest pathname-directory.3
  (let ((d (pathname-directory (pathname "sub/foo.lisp"))))
    (and (consp d) (eq (car d) :relative)))
  t)

;;; --- make-pathname ---

(deftest make-pathname.1
  (namestring (make-pathname :name "foo" :type "lisp"))
  "foo.lisp")

(deftest make-pathname.2
  (pathname-name (make-pathname :name "bar" :type "txt"))
  "bar")

(deftest make-pathname.3
  (pathname-type (make-pathname :name "bar" :type "txt"))
  "txt")

;;; --- merge-pathnames ---

(deftest merge-pathnames.1
  (let ((p (merge-pathnames (pathname "foo.lisp") (pathname "/tmp/"))))
    (namestring p))
  "/tmp/foo.lisp")

;;; --- probe-file ---

(deftest probe-file.1
  ;; Nonexistent file
  (probe-file "/tmp/nonexistent-dotcl-test-file-xyz.lisp")
  nil)

;;; --- typep ---

(deftest typep-pathname.1
  (typep (pathname "foo.lisp") 'pathname)
  t)

(deftest typep-pathname.2
  (typep "foo.lisp" 'pathname)
  nil)

(do-tests-summary)
