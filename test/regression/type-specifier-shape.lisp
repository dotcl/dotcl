;;; TYPEP and SUBTYPEP reject objects that cannot be type specifiers, bare
;;; compound-only names (AND OR MEMBER NOT EQL SATISFIES), and compound
;;; specifiers with a dotted argument list; the valid forms still work.

(defmacro %tss-err (form)
  `(handler-case (progn (eval ',form) :no-error) (error () :error)))

(deftest type-specifier-shape.typep-invalid
  (values (%tss-err (typep nil 0)) (%tss-err (typep nil "abc")) (%tss-err (typep nil #\a))
          (%tss-err (typep nil (vector 1 2))) (%tss-err (typep nil (make-hash-table)))
          (%tss-err (typep nil 'and)) (%tss-err (typep nil 'or)) (%tss-err (typep nil 'member))
          (%tss-err (typep nil 'not)) (%tss-err (typep nil 'eql))
          (%tss-err (typep nil '(and . foo))) (%tss-err (typep nil '(or . foo)))
          (%tss-err (typep nil '(member . foo))) (%tss-err (typep nil '(not))))
  :error :error :error :error :error :error :error :error :error :error :error :error :error :error)

(deftest type-specifier-shape.subtypep-invalid
  (values (%tss-err (subtypep 0 t)) (%tss-err (subtypep nil "x"))
          (%tss-err (subtypep 'and t)) (%tss-err (subtypep '(or . foo) t))
          (%tss-err (subtypep t '(not))))
  :error :error :error :error :error)

(deftest type-specifier-shape.valid
  (values (typep 1 'integer) (typep 1 '(or integer symbol)) (typep nil '(and))
          (typep nil '(or)) (typep 3 '(member 1 2 3)) (typep 3 '(eql 3)) (typep 3 '(not symbol))
          (typep 3 (find-class 'integer)) (typep nil '(member)) (typep 5 '(satisfies integerp))
          (subtypep 'integer 'number) (subtypep '(and) t) (subtypep '(or) 'integer)
          (subtypep '(member 1 2) 'integer) (subtypep '(values integer) '(values number)))
  t t t nil t t t t nil t t t t t t)

;;; Every valid compound specifier shape keeps working with TYPEP and SUBTYPEP.
(deftest type-specifier-shape.valid-compounds
  (values (typep 1 '(member 1 2 3 4)) (typep 5 '(member 1 2 3 4)) (typep 1 '(member))
          (typep 5 '(integer 0 10)) (typep 5 '(integer * 10)) (typep 5 '(integer 0)) (typep 5 '(integer))
          (typep 1.5 '(float 0.0 2.0)) (typep 1/2 '(rational 0 1)) (typep 1 '(real 0))
          (typep 1 '(or integer string)) (typep 1 '(and integer (not string)))
          (typep 1 '(and)) (typep 1 '(or)) (typep 1 '(and integer)) (typep 1 '(not string))
          (typep 1 '(eql 1)) (typep 1 '(satisfies oddp)) (typep 3 '(mod 4)) (typep 3 '(unsigned-byte 8))
          (typep 3 '(signed-byte 8)) (typep #c(1 2) '(complex integer))
          (typep #'car 'function)
          (typep (make-array 3) '(array t (3))) (typep (make-array 3) '(vector t 3))
          (typep (make-array 3) '(simple-vector 3)) (typep "ab" '(string 2)) (typep "ab" '(simple-string))
          (typep #*101 '(bit-vector 3)) (typep (cons 1 2) '(cons integer integer)) (typep (cons 1 2) '(cons)))
  t nil nil t t t t t t t t t t nil t t t t t t t t t t t t t t t t t)

(deftest type-specifier-shape.valid-compounds-subtypep
  (values (subtypep '(member 1 2 3 4) 'integer) (subtypep '(member) 'integer)
          (subtypep '(integer 0 10) '(integer 0)) (subtypep '(or integer string) 't)
          (subtypep '(and integer (not string)) 'integer) (subtypep '(eql 1) 'integer)
          (subtypep '(satisfies oddp) 't) (subtypep '(function (t) t) 'function)
          (subtypep '(array t (3)) 'array) (subtypep '(cons integer) 'cons)
          (subtypep '(values integer) '(values number)))
  t t t t t t t t t t t)

;;; The type a type-error reports is a specifier TYPEP accepts (PPRINT-NEWLINE
;;; used to report the bare symbol MEMBER).
(deftest type-specifier-shape.pprint-newline-expected-type
  (handler-case (pprint-newline 1)
    (type-error (e) (list (typep 1 (type-error-expected-type e))
                          (typep :fill (type-error-expected-type e)))))
  (nil t))
