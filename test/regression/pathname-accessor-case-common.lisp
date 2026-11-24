;;; PATHNAME-NAME / PATHNAME-TYPE / PATHNAME-DIRECTORY honour :CASE :COMMON
;;; (all-upper becomes lower, all-lower becomes upper, mixed case is kept);
;;; :LOCAL and no :CASE return the component as stored. Expected values are
;;; what SBCL returns.

(deftest pathname-accessor-case-common.name
  (list (pathname-name "ABC" :case :common)
        (pathname-name "abc" :case :common)
        (pathname-name "AbC" :case :common)
        (pathname-name "ABC" :case :local)
        (pathname-name "ABC"))
  ("abc" "ABC" "AbC" "ABC" "ABC"))

(deftest pathname-accessor-case-common.type-directory
  (list (pathname-type "x.LISP" :case :common)
        (pathname-directory "/A/b/C" :case :common)
        (pathname-directory "a/b" :case :common)
        (pathname-directory "a/b" :case :local))
  ("lisp" (:absolute "a" "B") (:relative "A") (:relative "a")))
