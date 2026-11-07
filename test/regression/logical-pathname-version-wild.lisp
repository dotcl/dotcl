;;; A logical pathname with a wild version prints the version as *, which reads back
;;; as a wild version (CLHS 19.3.1), not as the word WILD.

(setf (logical-pathname-translations "LPVW") '(("**;*.*.*" "/tmp/lpvw/**/*.*")))

(deftest logical-pathname-version-wild.print
  (namestring (make-pathname :host "LPVW" :directory '(:absolute "A") :name "B" :type "C"
                             :version :wild))
  "LPVW:A;B.C.*")

(deftest logical-pathname-version-wild.round-trip
  (let ((p (parse-namestring "LPVW:A;B.C.*")))
    (list (pathname-version p) (namestring p)
          (pathname-version (parse-namestring (namestring p)))))
  (:wild "LPVW:A;B.C.*" :wild))

(deftest logical-pathname-version-wild.newest-and-number
  (list (namestring (parse-namestring "LPVW:A;B.C.NEWEST"))
        (namestring (parse-namestring "LPVW:A;B.C.3")))
  ("LPVW:A;B.C.NEWEST" "LPVW:A;B.C.3"))
