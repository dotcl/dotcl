;;; Name, type and directory pieces that mix literal characters with * and ?
;;; are patterns for PATHNAME-MATCH-P and TRANSLATE-PATHNAME (SBCL and ECL do
;;; this); the text a * of the from-wildcard matched fills the * of the
;;; to-wildcard in order.

(deftest pathname-partial-wildcards.match
  (list (pathname-match-p "abc.txt" "a*.txt")
        (pathname-match-p "abc.txt" "a?c.txt")
        (pathname-match-p "abc.txt" "a?.txt")
        (pathname-match-p "xbc.txt" "a*.txt")
        (pathname-match-p "abc.txt" "*c.t*")
        (pathname-match-p "a.txt" "a*.txt"))
  (t t nil nil t t))

(deftest pathname-partial-wildcards.match-directory
  (list (pathname-match-p "/a/bq/c.txt" "/a/b*/c.txt")
        (pathname-match-p "/a/xq/c.txt" "/a/b*/c.txt"))
  (t nil))

(deftest pathname-partial-wildcards.translate-name
  (list (namestring (translate-pathname "abc.txt" "a*.txt" "x*.lisp"))
        (namestring (translate-pathname "abcd.txt" "a*d.txt" "<*>.l"))
        (namestring (translate-pathname "foo.txt" "*.txt" "bar.*")))
  ("xbc.lisp" "<bc>.l" "bar.txt"))

(deftest pathname-partial-wildcards.translate-directory
  (namestring (translate-pathname "/a/bq/c.txt" "/a/b*/*.txt" "/x/y*/z*.lisp"))
  "/x/yq/zc.lisp")

(deftest pathname-partial-wildcards.translate-no-match
  (handler-case (progn (translate-pathname "xbc.txt" "a*.txt" "y*.txt") :no-error)
    (error () :error))
  :error)
