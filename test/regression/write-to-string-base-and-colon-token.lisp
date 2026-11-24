;;; WRITE-TO-STRING checks its printer keywords as WRITE does, and a token that ends in a
;;; package marker is a reader error (CLHS 2.3.5). The neighbouring valid inputs still work.

(defun %wsb-datum (thunk)
  (handler-case (funcall thunk)
    (type-error (e) (list :type-error (type-error-datum e)))))

(defun %wsb-read (string)
  (handler-case (let ((x (read-from-string string)))
                  (list :read (if (symbolp x)
                                  (list (symbol-name x)
                                        (let ((p (symbol-package x))) (and p (package-name p))))
                                  x)))
    (reader-error () :reader-error)
    (end-of-file () :reader-error)))

(deftest write-to-string-base.rejected
  (list (%wsb-datum (lambda () (write-to-string 35 :base 10.0)))
        (%wsb-datum (lambda () (write-to-string 35 :base nil)))
        (%wsb-datum (lambda () (write-to-string 35 :base 'a)))
        (%wsb-datum (lambda () (write-to-string 35 :base 2/3)))
        (%wsb-datum (lambda () (write-to-string 35 :base 1)))
        (%wsb-datum (lambda () (write-to-string 35 :base 0)))
        (%wsb-datum (lambda () (write-to-string 35 :base -1)))
        (%wsb-datum (lambda () (write-to-string 35 :base 37)))
        (%wsb-datum (lambda () (write-to-string 35 :base "10"))))
  ((:type-error 10.0) (:type-error nil) (:type-error a) (:type-error 2/3)
   (:type-error 1) (:type-error 0) (:type-error -1) (:type-error 37) (:type-error "10")))

(deftest write-to-string-base.accepted-neighbours
  (list (write-to-string 35 :base 2)
        (write-to-string 35 :base 10)
        (write-to-string 35 :base 36)
        (write-to-string 35)
        (write-to-string 35 :base 16 :radix t)
        (write-to-string '(1 (2)) :level nil)
        (write-to-string '(1 (2)) :level 0)
        (write-to-string '(1 2) :length nil)
        (write-to-string '(1 2) :length 0)
        (write-to-string 'ab :case :downcase)
        (write-to-string 35 :base 10 :base 99))
  ("100011" "35" "Z" "35" "#x23" "(1 (2))" "#" "(1 2)" "(...)" "ab" "35"))

(deftest write-to-string-keywords.rejected
  (list (%wsb-datum (lambda () (write-to-string '(1) :level -1)))
        (%wsb-datum (lambda () (write-to-string '(1) :length 1.5)))
        (%wsb-datum (lambda () (write-to-string 'a :case :foo)))
        (%wsb-datum (lambda () (write-to-string 'a :case nil))))
  ((:type-error -1) (:type-error 1.5) (:type-error :foo) (:type-error nil)))

(deftest colon-token.rejected
  (list (%wsb-read ":") (%wsb-read "::") (%wsb-read ": x") (%wsb-read ":)")
        (%wsb-read "(:)") (%wsb-read "cl:") (%wsb-read "cl::") (%wsb-read "a:")
        (%wsb-read "cl: x") (%wsb-read "(a . :)"))
  (:reader-error :reader-error :reader-error :reader-error :reader-error
   :reader-error :reader-error :reader-error :reader-error :reader-error))

(deftest colon-token.accepted-neighbours
  (list (%wsb-read ":a") (%wsb-read "::a") (%wsb-read ":|a|") (%wsb-read ":||")
        (%wsb-read "|:|") (%wsb-read "\\:") (%wsb-read "cl:car") (%wsb-read "cl::car")
        (%wsb-read "#:a") (%wsb-read "#:") (%wsb-read ":1") (%wsb-read "a\\:"))
  ((:read ("A" "KEYWORD")) (:read ("A" "KEYWORD")) (:read ("a" "KEYWORD"))
   (:read ("" "KEYWORD")) (:read (":" "COMMON-LISP-USER")) (:read (":" "COMMON-LISP-USER"))
   (:read ("CAR" "COMMON-LISP")) (:read ("CAR" "COMMON-LISP"))
   (:read ("A" nil)) (:read ("" nil)) (:read ("1" "KEYWORD"))
   (:read ("A:" "COMMON-LISP-USER"))))
