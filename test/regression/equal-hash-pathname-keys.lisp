;;; An EQUAL hash table finds a pathname key from another pathname EQUAL to
;;; it. EQUAL compares pathnames by their components, but the table compared
;;; them by identity and hashed them by identity, so (gethash (pathname "/x/y.z")
;;; h) missed the entry stored under #p"/x/y.z".

(deftest equal-hash-pathname-keys.lookup
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash #p"/x/y.z" h) 1
          (gethash (make-pathname :name "a" :type "b") h) 2)
    (list (gethash (pathname "/x/y.z") h)
          (gethash (make-pathname :name "a" :type "b") h)
          (gethash (make-pathname :name "A" :type "b") h)
          (hash-table-count h)))
  (1 2 nil 2))

(deftest equal-hash-pathname-keys.no-duplicate-entries
  (let ((h (make-hash-table :test 'equal)))
    (dotimes (i 3) (setf (gethash (merge-pathnames "c.d" #p"/tmp/") h) i))
    (hash-table-count h))
  1)
