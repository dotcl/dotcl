;;; PATHNAME-MATCH-P and TRANSLATE-PATHNAME accept a pattern with several ** directory pieces:
;;; (pathname-match-p "/a/1/b/2/c.lisp" "/a/**/b/**/c.lisp") was NIL, and the translate signalled.
;;; A single ** and ordinary directories are unchanged.

(deftest pathname-several-wild-inferiors.match
  (list (pathname-match-p "/a/1/b/2/c.lisp" "/a/**/b/**/c.lisp")
        (pathname-match-p "/a/b/c.lisp" "/a/**/b/**/c.lisp")
        (pathname-match-p "/a/1/2/b/3/4/c.lisp" "/a/**/b/**/c.lisp")
        (pathname-match-p "/a/1/c.lisp" "/a/**/b/**/c.lisp")
        (pathname-match-p "/a/1/c.lisp" "/a/**/c.lisp")
        (pathname-match-p "/a/c.lisp" "/a/**/c.lisp")
        (pathname-match-p "/x/1/c.lisp" "/a/**/c.lisp")
        (pathname-match-p "/a/1/b/2/d.lisp" "/a/**/b/**/c.lisp"))
  (t t t nil t t nil nil))

(deftest pathname-several-wild-inferiors.translate
  (list (namestring (translate-pathname "/a/1/b/2/c.lisp" "/a/**/b/**/c.lisp" "/x/**/y/**/z.lisp"))
        (namestring (translate-pathname "/a/1/2/b/3/c.lisp" "/a/**/b/**/c.lisp" "/x/**/y/**/z.lisp"))
        (namestring (translate-pathname "/a/1/c.lisp" "/a/**/c.lisp" "/x/**/z.lisp")))
  ("/x/1/y/2/z.lisp" "/x/1/2/y/3/z.lisp" "/x/1/z.lisp"))
