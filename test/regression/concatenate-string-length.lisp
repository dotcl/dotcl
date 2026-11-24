;;; (concatenate '(string 3) "ab") signals a TYPE-ERROR: a result type that names a
;;; length gets exactly that length. The VECTOR T forms already did; the string
;;; and (vector character n) forms returned whatever was built.

(deftest concatenate-string-length.mismatch-signals
  (flet ((err (thunk) (handler-case (progn (funcall thunk) :no-error) (type-error () :type-error))))
    (list (err (lambda () (concatenate '(string 3) "ab")))
          (err (lambda () (concatenate '(string 3) "abcd")))
          (err (lambda () (concatenate '(simple-string 3) "ab")))
          (err (lambda () (concatenate '(vector character 2) "abc")))
          (err (lambda () (concatenate '(simple-array character (3)) "ab")))))
  (:type-error :type-error :type-error :type-error :type-error))

(deftest concatenate-string-length.match-ok
  (list (concatenate '(string 3) "ab" "c")
        (concatenate '(string *) "ab")
        (concatenate '(vector character 2) "a" "b")
        (concatenate 'string "ab" "cd"))
  ("abc" "ab" "ab" "abcd"))
