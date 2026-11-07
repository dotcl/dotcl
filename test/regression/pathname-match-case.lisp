;;; PATHNAME-MATCH-P compares name, type and directory pieces with their case
;;; where file names differ by case (Unix-like systems, as in SBCL and ECL), and
;;; without it on Windows.

(defvar *pmc-windows* (and (member :windows dotcl:*host-features*) t))

(deftest pathname-match-case.name-and-type
  (list (and (pathname-match-p "FOO.TXT" "foo.txt") t)
        (and (pathname-match-p "foo.txt" "foo.txt") t)
        (and (pathname-match-p "foo.TXT" "foo.txt") t))
  #.(if (and (member :windows dotcl:*host-features*) t) '(t t t) '(nil t nil)))

(deftest pathname-match-case.directory
  (and (pathname-match-p "/A/b/c.txt" "/a/b/c.txt") t)
  #.(if (and (member :windows dotcl:*host-features*) t) 't 'nil))

(deftest pathname-match-case.pattern
  (list (and (pathname-match-p "Foo.txt" "F*.txt") t)
        (and (pathname-match-p "foo.txt" "F*.txt") t))
  #.(if (and (member :windows dotcl:*host-features*) t) '(t t) '(t nil)))
