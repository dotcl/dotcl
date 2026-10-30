;;; EQUAL, EQUALP and hash tables treat NIL and :NEWEST versions as the same.

(deftest pathname-equal-newest-version.equal
  (let ((c (make-pathname :directory '(:relative "wow") :name "ok" :type "txt"))
        (d (merge-pathnames "ok.txt" "wow/")))
    (list (equal c d) (equalp c d)
          (equal (pathname "wow/ok/") (merge-pathnames "ok/" "wow/"))
          (equal (make-pathname :name "x" :version 3)
                 (make-pathname :name "x" :version :newest))))
  (t t t nil))

(deftest pathname-equal-newest-version.hash-table
  (let ((h (make-hash-table :test 'equal))
        (nil-version (make-pathname :directory '(:relative "wow")
                                    :name "ok" :type "txt")))
    (setf (gethash nil-version h) 1)
    (list (gethash (merge-pathnames "ok.txt" "wow/") h)
          (progn (setf (gethash (merge-pathnames "ok.txt" "wow/") h) 2)
                 (hash-table-count h))))
  (1 1))
