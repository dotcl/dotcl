;;; MAKE-PATHNAME components given as strings that are not plain CHARACTER
;;; simple strings (base strings, fill-pointer strings) appear in the namestring.
;;; UIOP:STRCAT returns a base string, and cffi-grovel names its .c file with it.

(deftest make-pathname-base-string.name
  (let ((s (make-string 3 :element-type 'base-char :initial-element #\a)))
    (namestring (make-pathname :name s :type (coerce "c" 'simple-base-string))))
  "aaa.c")

(deftest make-pathname-base-string.fill-pointer
  (let ((f (make-array 3 :element-type 'character :fill-pointer 2
                         :initial-contents "xyz")))
    (list (namestring (make-pathname :name f :type "c"))
          (namestring (make-pathname :defaults f))))
  ("xy.c" "xy"))

(deftest make-pathname-base-string.directory
  (let ((b (coerce "abc" 'simple-base-string)))
    (list (namestring (make-pathname :directory (list :absolute b "d") :name "q"))
          (namestring (make-pathname :directory b))
          (namestring (merge-pathnames (make-pathname :name b) #p"/t/x.c"))))
  ("/abc/d/q" "/abc/" "/t/abc.c"))
