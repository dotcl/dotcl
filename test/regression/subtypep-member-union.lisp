;;; (MEMBER ...) on the left of SUBTYPEP is decided object by object, also
;;; against a union none of whose members covers it alone. Expected values are
;;; SBCL's.

(deftest subtypep-member-or-eql
  (list (multiple-value-list (subtypep '(member :x :y) '(or (eql :x) (eql :y))))
        (multiple-value-list (subtypep '(member :x :y) '(or (eql :x) (eql :z))))
        (multiple-value-list (subtypep '(or (eql :x) (eql :y)) '(member :x :y))))
  ((t t) (nil t) (t t)))

(deftest subtypep-member-mixed-union
  (list (multiple-value-list (subtypep '(member 1 :y) '(or integer (eql :y))))
        (multiple-value-list (subtypep '(member 1 :y) '(or (integer 2 3) (eql :y))))
        (multiple-value-list (subtypep '(member 1.0 2) '(or float (eql 2))))
        (multiple-value-list (subtypep '(member :x nil) '(or null (eql :x))))
        (multiple-value-list (subtypep '(member :x nil) '(or list (eql :x)))))
  ((t t) (nil t) (t t) (t t) (t t)))

(deftest subtypep-member-not-and
  (list (multiple-value-list (subtypep '(member :x :y) '(not (eql :z))))
        (multiple-value-list (subtypep '(member :x :y) '(not (eql :x))))
        (multiple-value-list (subtypep '(member :x :y) '(and keyword (or (eql :x) (eql :y))))))
  ((t t) (nil t) (t t)))

(deftest subtypep-member-satisfies
  (list (multiple-value-list (subtypep '(member :x) '(satisfies keywordp)))
        (multiple-value-list (subtypep '(member :x :y) '(or (eql :x) (satisfies keywordp)))))
  ((t t) (t t)))
