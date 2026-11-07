;;; TYPEP signals for a CL compound type specifier whose numeric / length / dimension
;;; arguments are not valid, or whose type arguments are numbers, strings or lists
;;; that are no type; the valid forms keep working.

(defmacro %tsa-err (form)
  `(handler-case (progn ,form :no-error) (error () :error)))

(deftest type-specifier-arguments.invalid
  (values (%tsa-err (typep 1 '(integer 1.5)))
          (%tsa-err (typep 1 '(integer a 3)))
          (%tsa-err (typep 1 '(mod *)))
          (%tsa-err (typep 1 '(signed-byte 1.5)))
          (%tsa-err (typep 1 '(unsigned-byte (* *))))
          (%tsa-err (typep 1 '(float 1.5 symbol)))
          (%tsa-err (typep nil '(vector 5 1.5)))
          (%tsa-err (typep nil '(simple-array -1)))
          (%tsa-err (typep nil '(array 1.5 t)))
          (%tsa-err (typep "s" '(string t)))
          (%tsa-err (typep 1 '(cons integer 0)))
          (%tsa-err (funcall #'typep 1 (copy-list '(or integer 5))))
          (%tsa-err (funcall #'typep 1 (copy-list '(and integer (2 2)))))
          (%tsa-err (typep 1 'mod)))
  :error :error :error :error :error :error :error :error :error :error :error :error :error :error)

(deftest type-specifier-arguments.valid
  (values (typep 1 '(integer 0 10)) (typep 1 '(integer * 10)) (typep 5 '(mod 10)) (typep 3 '(signed-byte 8))
          (typep 1.5 '(float 0.0 2.0)) (typep 1/2 '(rational 0 1)) (typep 1 '(real (0) *))
          (typep "ab" '(string 2)) (typep (make-array 3) '(vector t 3)) (typep (make-array '(2 2)) '(array t (2 2)))
          (typep (make-array '(2 2)) '(array * (* 2))) (typep (make-array '(2 2)) '(array * 2)) (typep (make-array 3) '(simple-array t (3)))
          (typep 1 '(or integer symbol)) (typep nil '(cons integer)) (typep (cons 1 2) '(cons integer integer))
          (typep #*101 '(bit-vector 3)) (typep (make-array 3) '(simple-vector 3)) (typep nil '(function)))
  t t t t t t t t t t t t t t nil t t t nil)
