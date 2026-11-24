;;; More built-in type-errors name the expected type SBCL names (the NIL expected-type is gone).

(defmacro %tee2 (form)
  `(handler-case ,form
     (type-error (e) (list (type-error-datum e) (type-error-expected-type e)))
     (error (e) (list :other (type-of e)))))

(deftest type-error-sbcl2.format-destination
  (list (%tee2 (format 5 "x")))
  ((5 (or string boolean stream))))

(deftest type-error-sbcl2.stream-designator
  (list (%tee2 (princ 1 5)) (%tee2 (terpri 5)))
  ((5 (or stream boolean)) (5 (or stream boolean))))

(deftest type-error-sbcl2.parse-integer-radix
  (list (%tee2 (parse-integer "12" :radix 99)))
  ((99 (integer 2 36))))

(deftest type-error-sbcl2.assoc-entry
  (list (%tee2 (assoc 1 '(5))))
  ((5 list)))

(deftest type-error-sbcl2.type-specifier
  (list (%tee2 (typep 1 5)) (%tee2 (subtypep 5 'a)) (%tee2 (make-condition 5)))
  ((5 (or cons symbol class)) (5 (or cons symbol class)) (5 (or cons symbol class))))
