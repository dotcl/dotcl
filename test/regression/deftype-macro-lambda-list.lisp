;;; DEFTYPE takes the lambda list of DEFMACRO (CLHS 3.4.4): &WHOLE (bound to the whole type
;;; specifier), &ENVIRONMENT and destructuring patterns were refused. Plain lambda lists
;;; (&optional, &rest, &key, defaults) work as they did.

(deftype dml-pair (&optional ((a b))) (list 'member a b))
(deftype dml-env (&environment env) (declare (ignore env)) 'integer)
(deftype dml-whole (&whole whole &optional (n 5)) (declare (ignore n)) (if (consp whole) 'integer 'string))
(deftype dml-nested ((lo hi)) `(integer ,lo ,hi))
(deftype dml-plain (&optional (n 5)) `(integer 0 ,n))
(deftype dml-rest (&rest r) `(member ,@r))
(deftype dml-key (&key (hi 10)) `(integer 0 ,hi))

(deftest deftype-macro-lambda-list.optional-destructuring
  (list (multiple-value-list (subtypep '(dml-pair (1 2)) '(member 1 2)))
        (typep 1 '(dml-pair (1 2))) (typep 3 '(dml-pair (1 2))))
  ((t t) t nil))

(deftest deftype-macro-lambda-list.required-destructuring
  (list (typep 5 '(dml-nested (1 10))) (typep 11 '(dml-nested (1 10))))
  (t nil))

(deftest deftype-macro-lambda-list.environment-and-whole
  (list (typep 1 'dml-env) (typep "a" 'dml-env)
        (typep 1 'dml-whole) (typep 1 '(dml-whole 3)))
  (t nil t t))

(deftest deftype-macro-lambda-list.plain-lists-unchanged
  (list (typep 3 'dml-plain) (typep 3 '(dml-plain 2)) (typep 2 '(dml-rest 1 2))
        (typep 3 '(dml-key :hi 5)) (typep 9 '(dml-key :hi 5)))
  (t nil t t nil))
