;;; A directory component :UP (".." in a namestring) matches itself in PATHNAME-MATCH-P and so
;;; TRANSLATE-PATHNAME works across it.

(deftest pathname-match-up.same-up-component
  (list (pathname-match-p "../bb.txt" "../bb.txt")
        (and (pathname-match-p "../bb.txt" "../*.txt") t)
        (and (pathname-match-p "x/../bb.txt" "x/../*.txt") t)
        (pathname-match-p "x/bb.txt" "x/../*.txt")
        (pathname-match-p "../bb.txt" "x/*.txt")
        (and (pathname-match-p "/a/b/../c.txt" "/a/**/*.txt") t))
  (t t t nil nil t))

(deftest pathname-match-up.translate-across-up
  (namestring (translate-pathname "../bb.txt" "../*.txt" "../out/*.dat"))
  "../out/bb.dat")
