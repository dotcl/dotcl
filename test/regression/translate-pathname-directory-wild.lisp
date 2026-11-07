;;; TRANSLATE-PATHNAME fills the wild pieces of the to-wildcard's directory with
;;; what the from-wildcard's wild pieces matched in the source, in order (SBCL
;;; does; the to-wildcard's * used to stay a * in the result).

(deftest translate-pathname-directory-wild.single
  (namestring (translate-pathname "/a/b/c.txt" "/a/*/*.txt" "/x/*/*.lisp"))
  "/x/b/c.lisp")

(deftest translate-pathname-directory-wild.inferiors
  (namestring (translate-pathname "/a/b/c/d.txt" "/a/**/d.txt" "/z/**/q.lisp"))
  "/z/b/c/q.lisp")

(deftest translate-pathname-directory-wild.both-in-order
  (namestring (translate-pathname "/a/b/c/d.txt" "/a/*/**/*.txt" "/x/**/*/y.lisp"))
  "/x/c/b/y.lisp")

(deftest translate-pathname-directory-wild.fixed-to-directory
  (namestring (translate-pathname "/a/b/c.txt" "/a/*/*.txt" "/x/y/*.lisp"))
  "/x/y/c.lisp")
