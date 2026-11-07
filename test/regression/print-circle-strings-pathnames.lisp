;;; With *PRINT-CIRCLE* true, a string or pathname that occurs twice in the printed
;;; object is printed with a label (CLHS 22.1.3: *PRINT-CIRCLE* does not limit the
;;; objects), as SBCL does; reading it back gives one object. Equal but distinct
;;; strings are not shared and print as they were.

(deftest print-circle-strings-pathnames.string-shared
  (let ((*print-circle* t) (s "shared")) (prin1-to-string (list s s)))
  "(#1=\"shared\" #1#)")

(deftest print-circle-strings-pathnames.pathname-shared
  (let ((*print-circle* t) (p #p"/a/b")) (prin1-to-string (list p p)))
  "(#1=#P\"/a/b\" #1#)")

(deftest print-circle-strings-pathnames.vector-of-strings-and-three-uses
  (let ((*print-circle* t) (s "ab")) (prin1-to-string (vector s (list s) s)))
  "#(#1=\"ab\" (#1#) #1#)")

(deftest print-circle-strings-pathnames.princ
  (let ((*print-circle* t) (s "x")) (princ-to-string (list s s)))
  "(#1=x #1#)")

(deftest print-circle-strings-pathnames.read-back-is-eq
  (let* ((*print-circle* t) (s "shared")
         (back (let ((*read-eval* nil)) (read-from-string (prin1-to-string (list s s))))))
    (eq (first back) (second back)))
  t)

(deftest print-circle-strings-pathnames.distinct-strings-unchanged
  (let ((*print-circle* t)) (prin1-to-string (list (copy-seq "a") (copy-seq "a"))))
  "(\"a\" \"a\")")

(deftest print-circle-strings-pathnames.without-print-circle-unchanged
  (let ((*print-circle* nil) (s "shared")) (prin1-to-string (list s s)))
  "(\"shared\" \"shared\")")

(deftest print-circle-strings-pathnames.single-string
  (let ((*print-circle* t)) (prin1-to-string "alone"))
  "\"alone\"")
