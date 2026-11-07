;;; * ? [ in a string given to MAKE-PATHNAME are literal (the pathname is not wild and its namestring
;;; quotes them: "a\*b"), a backslash before them in a namestring quotes them on Unix, and the file
;;; system sees the plain character. A namestring with an unquoted * or ? stays a pattern, and a
;;; backslash that quotes nothing is still a directory separator (the Windows habit).
;;; Windows keeps \ as the separator: this file checks the Unix reading only.
#-windows
(progn
  (deftest pathname-quoting.make-pathname-is-literal
    (list (wild-pathname-p (make-pathname :name "a*b"))
          (namestring (make-pathname :name "a*b"))
          (namestring (make-pathname :name "a?b" :type "t[1]"))
          (wild-pathname-p (make-pathname :name :wild))
          (wild-pathname-p (make-pathname :name "plain" :type "lisp")))
    (nil "a\\*b" "a\\?b.t\\[1]" t nil))

  ;; The accessors answer the plain text: the name of "a\\*b" is a*b.
  (deftest pathname-quoting.accessors-unquote
    (list (pathname-name (make-pathname :name "a*b"))
          (pathname-name (parse-namestring "a\\*b"))
          (pathname-type (make-pathname :name "f" :type "t?"))
          (pathname-directory (make-pathname :directory '(:absolute "d[1]" "e")))
          (pathname-name (parse-namestring "*")))
    ("a*b" "a*b" "t?" (:absolute "d[1]" "e") :wild))

  (deftest pathname-quoting.namestring-quoting
    (list (wild-pathname-p (parse-namestring "a*b"))
          (wild-pathname-p (parse-namestring "a\\*b"))
          (wild-pathname-p (parse-namestring "a\\?b"))
          (namestring (parse-namestring "a\\*b"))
          (equal (parse-namestring "a\\*b") (make-pathname :name "a*b"))
          (wild-pathname-p (parse-namestring "*"))
          (wild-pathname-p (parse-namestring "\\*"))
          (wild-pathname-p (parse-namestring "")))
    (t nil nil "a\\*b" t t nil nil))

  (deftest pathname-quoting.matching
    (list (pathname-match-p "a*b" "a\\*b")
          (pathname-match-p "axb" "a\\*b")
          (pathname-match-p "a?b" "a\\?b")
          (pathname-match-p "axb" "a?b")
          (pathname-match-p "axb" "a*b")
          (pathname-match-p "a*b" "a*b"))
    (t nil t t t t))

  (deftest pathname-quoting.file-system
    (let* ((d (format nil "~A/pq-test" (regression-temp-dir))))
      (ensure-directories-exist (format nil "~A/" d))
      (dolist (n '("a\\*b" "axb" "a\\?b"))
        (with-open-file (s (format nil "~A/~A" d n) :direction :output :if-exists :supersede)
          (write-string "x" s)))
      (list (and (probe-file (format nil "~A/a\\*b" d)) t)
            (and (probe-file (make-pathname :directory (list :absolute (subseq d 1)) :name "a*b")) t)
            (length (directory (format nil "~A/a*b" d)))
            (length (directory (format nil "~A/a\\*b" d)))
            (length (directory (format nil "~A/a\\?b" d)))
            (and (probe-file (format nil "~A/nosuch" d)) t)
            (with-open-file (s (format nil "~A/a\\*b" d)) (read-line s))))
    (t t 3 1 1 nil "x"))

  (deftest pathname-quoting.backslash-still-separates
    (list (pathname-directory (parse-namestring "dir\\sub\\f.lisp"))
          (pathname-name (parse-namestring "dir\\sub\\f.lisp")))
    ((:relative "dir" "sub") "f")))

#-windows
(deftest pathname-quoting.backslash-edge-forms
  (list (pathname-directory (parse-namestring "a\\\\b\\f"))
        (pathname-directory (parse-namestring "d\\sub\\"))
        (pathname-name (parse-namestring "x\\[y"))
        (pathname-name (parse-namestring "x\\?y")))
  ((:relative "a" "b") (:relative "d" "sub") "x[y" "x?y"))
