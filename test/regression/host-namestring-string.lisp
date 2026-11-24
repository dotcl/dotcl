;;; HOST-NAMESTRING always returns a string.

(deftest host-namestring-string.physical
  (list (host-namestring #p"/a/b/c.txt") (host-namestring #p"foo") (host-namestring #p"*.lisp"))
  ("" "" ""))

(deftest host-namestring-string.logical
  (progn
    (setf (logical-pathname-translations "HNS") '(("**;*.*.*" "/tmp/hns/**/*.*.*")))
    (list (host-namestring (logical-pathname "HNS:A;B.TXT"))))
  ("HNS"))
