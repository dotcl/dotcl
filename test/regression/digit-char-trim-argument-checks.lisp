;;; Argument checks (SBCL: TYPE-ERROR): DIGIT-CHAR-P's radix is an integer from 2 to 36,
;;; DIGIT-CHAR's weight a non-negative integer, and the character bag of STRING-TRIM and
;;; its relatives a sequence. (digit-char-p #\5 37) used to be 5. Valid calls, including a
;;; weight past the radix (NIL), are unchanged.

(defun %dcta (thunk) (handler-case (funcall thunk) (type-error () :type-error)))

(deftest digit-char-trim-argument-checks.digit-char-p-radix
  (list (%dcta (lambda () (digit-char-p #\5 37)))
        (%dcta (lambda () (digit-char-p #\5 1)))
        (%dcta (lambda () (digit-char-p #\5 0)))
        (%dcta (lambda () (digit-char-p #\5 'a)))
        (digit-char-p #\5 36) (digit-char-p #\5 2) (digit-char-p #\z 36) (digit-char-p #\7)
        (digit-char-p #\a 16) (digit-char-p #\g 16))
  (:type-error :type-error :type-error :type-error 5 nil 35 7 10 nil))

(deftest digit-char-trim-argument-checks.digit-char-weight
  (list (%dcta (lambda () (digit-char -1 10)))
        (%dcta (lambda () (digit-char 1.5 10)))
        (%dcta (lambda () (digit-char 5 37)))
        (digit-char 11 10) (digit-char 5) (digit-char 35 36) (digit-char 0 2))
  (:type-error :type-error :type-error nil #\5 #\Z #\0))

(deftest digit-char-trim-argument-checks.string-trim-bag
  (list (%dcta (lambda () (string-trim 42 "abc")))
        (%dcta (lambda () (string-left-trim 42 "abc")))
        (%dcta (lambda () (string-right-trim 'a "abc")))
        (string-trim "a" "abc") (string-trim '(#\a #\c) "abc") (string-trim #(#\a) "abc")
        (string-trim nil "abc") (string-left-trim "a" "aab") (string-right-trim "b" "abb"))
  (:type-error :type-error :type-error "bc" "b" "bc" "abc" "b" "a"))
