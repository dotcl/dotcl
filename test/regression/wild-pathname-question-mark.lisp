;;; WILD-PATHNAME-P counts ? as a wildcard in the name, type and directory, as PATHNAME-MATCH-P and
;;; DIRECTORY already did. A pathname without * or ? is not wild.

(deftest wild-pathname-question-mark.detected
  (list (wild-pathname-p #p"/src/?.lisp")
        (wild-pathname-p #p"/src/?.lisp" :name)
        (wild-pathname-p #p"/src/a.l?p" :type)
        (wild-pathname-p #p"/s?c/a.lisp" :directory)
        (wild-pathname-p (parse-namestring "a?b")))
  (t t t t t))

(deftest wild-pathname-question-mark.not-wild
  (list (wild-pathname-p #p"/src/a.lisp")
        (wild-pathname-p #p"/src/a.lisp" :name)
        (wild-pathname-p #p"/src/?.lisp" :type)
        (wild-pathname-p #p"/src/*.lisp" :name)
        (wild-pathname-p #p"")
        (wild-pathname-p #p"/src/*.lisp"))
  (nil nil nil t nil t))
