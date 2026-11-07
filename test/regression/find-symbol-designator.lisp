;;; FIND-SYMBOL wants a string; package designators NIL and T name packages.

(deftest find-symbol-non-string-name
  (handler-case (find-symbol 1) (type-error (e) (type-error-expected-type e)))
  string)

(deftest find-symbol-symbol-name
  (handler-case (find-symbol 'car) (type-error () :type-error))
  :type-error)

(deftest find-symbol-ordinary
  (multiple-value-list (find-symbol "CAR" "CL"))
  (car :external))

(deftest find-package-nil-and-t
  (list (find-package nil) (find-package t))
  (nil nil))

(deftest package-use-list-bad-designator
  (handler-case (package-use-list 5)
    (type-error (e) (type-error-datum e)))
  5)
