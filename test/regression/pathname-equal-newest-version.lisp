;;; EQUAL (and EQUALP, and an EQUAL hash table) take a pathname whose version
;;; is NIL and one whose version is :NEWEST for the same file, as SBCL does.
;;; (pathname "wow/ok.txt") has version NIL and (merge-pathnames "ok.txt"
;;; "wow/") gets :NEWEST from the default version; they were not EQUAL, and
;;; Coalton's Eq on pathnames (and its sorted DIRECTORY-FILES comparison) saw
;;; two different files.

(deftest pathname-equal-newest-version.equal
  (let ((c (pathname "wow/ok.txt"))
        (d (merge-pathnames "ok.txt" "wow/")))
    (list (equal c d) (equalp c d)
          (equal (pathname "wow/ok/") (merge-pathnames "ok/" "wow/"))
          (equal (make-pathname :name "x" :version 3)
                 (make-pathname :name "x" :version :newest))))
  (t t t nil))

(deftest pathname-equal-newest-version.hash-table
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash (pathname "wow/ok.txt") h) 1)
    (list (gethash (merge-pathnames "ok.txt" "wow/") h)
          (progn (setf (gethash (merge-pathnames "ok.txt" "wow/") h) 2)
                 (hash-table-count h))))
  (1 1))
