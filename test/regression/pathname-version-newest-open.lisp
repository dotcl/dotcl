;;; Parsed physical namestrings have version :NEWEST; OPEN's default follows it.

(defun pvno-file (name)
  (merge-pathnames name
                   (pathname (concatenate 'string (regression-temp-dir) "/"))))

(defun pvno-read (path)
  (with-open-file (s path :direction :input)
    (let ((buf (make-string 16)))
      (subseq buf 0 (read-sequence buf s)))))

(defun pvno-write-old (path)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-string "old-old" s)))

(defun pvno-try (path direction version)
  "Open PATH with VERSION and no :IF-EXISTS; return result and contents."
  (pvno-write-old path)
  (let* ((spec (make-pathname :version version :defaults path))
         (result (handler-case
                     (let ((s (open spec :direction direction)))
                       (unwind-protect
                            (progn
                              (when (member direction '(:output :io))
                                (write-string "x" s))
                              :opened)
                         (close s)))
                   (file-error () :file-error))))
    (list result (pvno-read path))))

(deftest pathname-version-newest-open.parsed
  (list (pathname-version (pathname "pvno-parse.txt"))
        (pathname-version (parse-namestring "pvno-parse.txt"))
        (pathname-version #P"pvno-parse.txt")
        (pathname-version (pathname ""))
        (pathname-version (parse-namestring "")))
  (:newest :newest :newest nil nil))

(deftest pathname-version-newest-open.directory-stays-nil
  (let ((dir (concatenate 'string (regression-temp-dir) "/")))
    (list (pathname-version (pathname dir))
          (pathname-version (parse-namestring dir))))
  (nil nil))

(deftest pathname-version-newest-open.default-output-truncates
  (let ((path (pvno-file "pvno-default-output.txt")))
    (pvno-write-old path)
    (with-open-file (s path :direction :output)
      (write-string "x" s))
    (pvno-read path))
  "x")

(deftest pathname-version-newest-open.default-io-truncates
  (let ((path (pvno-file "pvno-default-io.txt")))
    (pvno-write-old path)
    (with-open-file (s path :direction :io)
      (write-string "x" s))
    (pvno-read path))
  "x")

(deftest pathname-version-newest-open.newest-output
  (pvno-try (pvno-file "pvno-newest-output.txt") :output :newest)
  (:opened "x"))

(deftest pathname-version-newest-open.newest-io
  (pvno-try (pvno-file "pvno-newest-io.txt") :io :newest)
  (:opened "x"))

(deftest pathname-version-newest-open.nil-version-signals
  (pvno-try (pvno-file "pvno-nil.txt") :output nil)
  (:file-error "old-old"))

(deftest pathname-version-newest-open.number-version-signals
  (pvno-try (pvno-file "pvno-number.txt") :output 1)
  (:file-error "old-old"))

(deftest pathname-version-newest-open.unspecific-version-signals
  (pvno-try (pvno-file "pvno-unspecific.txt") :output :unspecific)
  (:file-error "old-old"))

(deftest pathname-version-newest-open.explicit-new-version-truncates
  (let* ((path (pvno-file "pvno-explicit-new-version.txt"))
         (spec (make-pathname :version nil :defaults path)))
    (pvno-write-old path)
    (with-open-file (s spec :direction :output :if-exists :new-version)
      (write-string "x" s))
    (pvno-read path))
  "x")
