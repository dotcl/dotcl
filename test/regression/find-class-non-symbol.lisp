;;; FIND-CLASS of something that cannot name a class is a type-error against SYMBOL.

(deftest find-class-non-symbol.number
  (handler-case (find-class 5)
    (type-error (e) (list (type-error-datum e) (type-error-expected-type e))))
  (5 symbol))

(deftest find-class-non-symbol.cons-and-char
  (list (handler-case (find-class '(a b)) (type-error () :type-error))
        (handler-case (find-class #\a) (type-error () :type-error)))
  (:type-error :type-error))

(deftest find-class-non-symbol.symbols-still-work
  (list (class-name (find-class 'integer)) (find-class 'no-such-class-xyz nil))
  (integer nil))
