;;; CLHS 3.4.1.4: the first of repeated keyword arguments is the one used.

(deftest keyword-duplicates.open
  (let ((path (regression-temp-file "keyword-duplicates-open.txt")))
    (unwind-protect
        (let ((s (open path :direction :output :direction :input
                            :if-exists :supersede :if-exists :error)))
          (unwind-protect (list (output-stream-p s) (input-stream-p s))
            (close s)))
      (ignore-errors (delete-file path))))
  (t nil))

(deftest keyword-duplicates.make-pathname
  (list (pathname-name (make-pathname :name "a" :name "b"))
        (pathname-type (make-pathname :type "x" :type "y"))
        (pathname-host (make-pathname :host "h" :host "g")))
  ("a" "x" "h"))

(deftest keyword-duplicates.parse-namestring
  (pathname-name (parse-namestring "abc" nil #p"/tmp/" :start 1 :start 2))
  "bc")

(deftest keyword-duplicates.make-package
  (let ((name (symbol-name (gensym "KWDUP-PKG"))))
    (unwind-protect
        (package-nicknames (make-package name :nicknames (list "KWDUP-NICK-A")
                                              :nicknames (list "KWDUP-NICK-B")))
      (ignore-errors (delete-package name))))
  ("KWDUP-NICK-A"))

(deftest keyword-duplicates.make-hash-table
  (let ((h (make-hash-table :test 'equal :test 'eq)))
    (setf (gethash (copy-seq "k") h) 1)
    (list (multiple-value-list (gethash (copy-seq "k") h))
          (hash-table-count (make-hash-table :size 1 :size "bad"))))
  ((1 t) 0))

(deftest keyword-duplicates.string-upcase
  (list (string-upcase "abc" :start 1 :start "x")
        (string-upcase "abc" :end 2 :end "x"))
  ("aBC" "ABc"))

(deftest keyword-duplicates.string=
  (string= "xabc" "abc" :start1 1 :start1 0 :start2 0 :start2 1)
  t)

(deftest keyword-duplicates.adjust-array
  (aref (adjust-array (make-array 2 :initial-element 0) 3
                      :initial-element 7 :initial-element 9)
        2)
  7)

(deftest keyword-duplicates.sort
  (list (sort (list 3 1 2) #'< :key #'identity :key (lambda (x) (- x)))
        (sort (list 3 1 2) #'< :key #'identity :key "x"))
  ((1 2 3) (1 2 3)))

(deftest keyword-duplicates.make-condition
  (princ-to-string (make-condition 'simple-error
                                   :format-control "first"
                                   :format-control "second"))
  "first")
