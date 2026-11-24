;;; Logical pathnames: a namestring merged into logical defaults is read in the
;;; defaults' host (the result is a logical pathname, name and type upper-case),
;;; FILE-NAMESTRING of a logical pathname includes the version, and :CASE :COMMON
;;; leaves logical components alone (they are already in the common case).

(setf (logical-pathname-translations "lpmc") '(("**;*.*" "/tmp/lpmc/**/*.*")))

(deftest logical-pathname-merge.physical-into-logical
  (let ((m (merge-pathnames "c.txt" "lpmc:a;b;")))
    (list (typep m 'logical-pathname)
          (namestring m)
          (pathname-name m)
          (pathname-type m)))
  (t "LPMC:A;B;C.TXT.NEWEST" "C" "TXT"))

(deftest logical-pathname-merge.version-from-defaults
  (namestring (merge-pathnames (make-pathname :name "q") "lpmc:a;b;d.e.3"))
  "LPMC:A;B;Q.E.NEWEST")

(deftest logical-pathname-file-namestring.version
  (list (file-namestring "lpmc:a;b;c.txt.3")
        (file-namestring "lpmc:a;b;c.txt.newest")
        (file-namestring "lpmc:a;b;c.txt"))
  ("C.TXT.3" "C.TXT.NEWEST" "C.TXT"))

(deftest logical-pathname-accessor-case-common
  (list (pathname-name "lpmc:a;B;c" :case :common)
        (pathname-directory "lpmc:a;B;c" :case :common))
  ("C" (:absolute "A" "B")))
