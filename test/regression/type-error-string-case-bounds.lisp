;;; A :START / :END of STRING-UPCASE that is not an integer is a type-error (it was a program-error).

(defmacro %tes (form)
  `(handler-case ,form
     (type-error (e) (list (type-error-datum e) (type-error-expected-type e)))
     (error (e) (list :other (type-of e)))))

(deftest type-error-string-case-bounds.start
  (list (%tes (string-upcase "abc" :start 'a)) (%tes (string-downcase "abc" :start 1.5)))
  ((a unsigned-byte) (1.5 unsigned-byte)))

(deftest type-error-string-case-bounds.end
  (list (%tes (string-upcase "abc" :end 'a)) (%tes (string-capitalize "abc" :end 1.5)))
  ((a (or null unsigned-byte)) (1.5 (or null unsigned-byte))))

(deftest type-error-string-case-bounds.still-works
  (list (string-upcase "abc" :start 1) (string-upcase "abc" :end nil))
  ("aBC" "ABC"))
