;;; MAKE-PATHNAME's :DIRECTORY may be a string, which means (:ABSOLUTE string)
;;; (CLHS 19.2.2.4.3). It was stored as the bare string, which the namestring
;;; printer then ignored: (make-pathname :directory "/bin" :name "ls") printed
;;; as "ls". GrammaTech's cl-utils WHICH builds its search path this way.

(deftest make-pathname-directory-string.component
  (list (pathname-directory (make-pathname :directory "bin"))
        (pathname-directory (make-pathname :directory "/bin")))
  ((:absolute "bin") (:absolute "/bin")))

(deftest make-pathname-directory-string.namestring
  (namestring (make-pathname :directory "usr" :name "ls"))
  "/usr/ls")

(deftest make-pathname-directory-string.merge
  (pathname-directory (merge-pathnames "ls" (make-pathname :directory "bin")))
  (:absolute "bin"))

(deftest make-pathname-directory-string.list-unchanged
  (pathname-directory (make-pathname :directory '(:relative "a" "b")))
  (:relative "a" "b"))
