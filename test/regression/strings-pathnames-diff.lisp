;;; Differences against SBCL 2.6.8 in string trimming and pathnames.

(deftest strings-pathnames-diff.trim-empty-bag-left
  (string-left-trim "" "  abc  ")
  "  abc  ")

(deftest strings-pathnames-diff.trim-empty-bag-right
  (string-right-trim "" "  abc  ")
  "  abc  ")

(deftest strings-pathnames-diff.trim-empty-bag-both
  (string-trim "" "  ")
  "  ")

(deftest strings-pathnames-diff.trim-empty-bag-tab
  (equal (string-trim "" (string #\Tab)) (string #\Tab))
  t)

(deftest strings-pathnames-diff.trim-empty-list-bag
  (string-trim '() "  ")
  "  ")

(setf (logical-pathname-translations "SPP-DIFF")
      '(("FOO;BAR.LISP" "/tmp/spp-diff/bar.lisp")
        ("FOO;*.LISP" "/tmp/spp-diff/*.lisp")
        ("**;*.*" "/tmp/spp-diff/**/*.*")))

(deftest strings-pathnames-diff.make-pathname-logical-uppercase
  (let ((p (make-pathname :host "SPP-DIFF" :directory '(:absolute "foo")
                          :name "bar" :type "lisp")))
    (list (pathname-host p) (pathname-directory p) (pathname-name p)
          (pathname-type p) (namestring p)))
  ("SPP-DIFF" (:absolute "FOO") "BAR" "LISP" "SPP-DIFF:FOO;BAR.LISP"))

(deftest strings-pathnames-diff.make-pathname-logical-version
  (namestring (make-pathname :host "SPP-DIFF" :directory '(:absolute "foo")
                             :name "bar" :type "lisp" :version :newest))
  "SPP-DIFF:FOO;BAR.LISP.NEWEST")

(deftest strings-pathnames-diff.merge-logical-uppercase
  (let ((p (merge-pathnames
            (make-pathname :host "SPP-DIFF" :directory '(:absolute "foo")
                           :name "bar" :type "lisp")
            (make-pathname :directory '(:absolute "tmp")
                           :name "base" :type "lisp"))))
    (list (pathname-directory p) (pathname-name p) (pathname-type p)
          (namestring p)))
  ((:absolute "FOO") "BAR" "LISP" "SPP-DIFF:FOO;BAR.LISP.NEWEST"))

(deftest strings-pathnames-diff.parse-namestring-logical-default
  (let ((p (parse-namestring "foo;bar.lisp" nil
                             (logical-pathname "SPP-DIFF:"))))
    (list (typep p 'logical-pathname) (namestring p)))
  (t "SPP-DIFF:FOO;BAR.LISP"))

(deftest strings-pathnames-diff.translate-logical-keeps-logical
  (let ((p (translate-pathname "SPP-DIFF:foo;bar.lisp"
                               "SPP-DIFF:foo;*.lisp"
                               "SPP-DIFF:baz;*.txt")))
    (list (typep p 'logical-pathname) (namestring p)))
  (t "SPP-DIFF:BAZ;BAR.TXT"))

(deftest strings-pathnames-diff.translate-logical-objects-keep-logical
  (let ((p (translate-pathname (logical-pathname "SPP-DIFF:foo;bar.lisp")
                               (logical-pathname "SPP-DIFF:foo;*.lisp")
                               (logical-pathname "SPP-DIFF:baz;*.txt"))))
    (list (typep p 'logical-pathname) (namestring p)))
  (t "SPP-DIFF:BAZ;BAR.TXT"))

(deftest strings-pathnames-diff.directory-namestring-logical
  (directory-namestring (logical-pathname "SPP-DIFF:foo;bar.lisp"))
  "FOO;")

(deftest strings-pathnames-diff.case-common-uppercase
  (namestring (make-pathname :name "foo" :type "txt" :case :common))
  "FOO.TXT")

(deftest strings-pathnames-diff.case-common-lowercase
  (namestring (make-pathname :name "Foo" :type "TXT" :case :common))
  "Foo.txt")

(deftest strings-pathnames-diff.case-common-directory
  (namestring (make-pathname :directory '(:absolute "TMP")
                             :name "FOO" :type "TXT" :case :common))
  "/tmp/foo.txt")

(deftest strings-pathnames-diff.base-char-stream-rejects-extended
  (signals-error
      (with-output-to-string (s nil :element-type 'base-char)
        (write-char (code-char 233) s))
    type-error)
  t)

(deftest strings-pathnames-diff.base-char-string-form-rejects-extended
  (let ((dst (make-array 0 :element-type 'base-char :adjustable t
                           :fill-pointer 0)))
    (signals-error
        (with-output-to-string (s dst :element-type 'base-char)
          (write-char (code-char 233) s))
      type-error))
  t)

(deftest strings-pathnames-diff.character-stream-accepts-extended
  (let ((out (with-output-to-string (s nil :element-type 'character)
               (write-char (code-char 233) s))))
    (and (= (length out) 1) (char= (char out 0) (code-char 233))))
  t)

(deftest strings-pathnames-diff.translate-unmatched-source-errors
  (signals-error (translate-pathname "foo.txt" "*.lisp" "*.txt")
    simple-error)
  t)

(deftest strings-pathnames-diff.enough-namestring-relative-default
  (let ((p (make-pathname :directory '(:relative "a") :name "foo"))
        (d (make-pathname :directory '(:relative "a"))))
    (list (enough-namestring p d)
          (equal (merge-pathnames (enough-namestring p d) d)
                 (merge-pathnames (parse-namestring p nil d) d))))
  ("a/foo" t))

(deftest strings-pathnames-diff.enough-namestring-absolute-default
  (let ((p (make-pathname :directory '(:absolute "tmp" "x") :name "foo"))
        (d (make-pathname :directory '(:absolute "tmp"))))
    (enough-namestring p d))
  "x/foo")

(deftest strings-pathnames-diff.make-pathname-unknown-keyword
  (signals-error (make-pathname :bogus 1) program-error)
  t)

(deftest strings-pathnames-diff.make-pathname-allow-other-keys
  (namestring (make-pathname :bogus 1 :allow-other-keys t))
  "")
