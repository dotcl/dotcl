;;; SUBTYPEP answers (NIL NIL) used to be given where the answer is T: a type that is the union of
;;; known parts (LIST, NUMBER, RATIONAL) is below what holds every part, (AND A (NOT B)) is below C
;;; when A is below (OR B C), and (OR X (NOT X)) is everything.

(defun %sc (a b) (multiple-value-list (subtypep a b)))

(deftest subtypep-cases.and-not
  (list (%sc '(and list (not null)) 'cons) (%sc '(and number (not complex)) 'real)
        (%sc '(and number (not real)) 'complex) (%sc '(and integer (not (eql 0))) 'integer)
        (%sc '(and integer (not integer)) nil))
  ((t t) (t t) (t t) (t t) (t t)))

(deftest subtypep-cases.complement-union
  (list (%sc t '(or integer (not integer))) (%sc '(or integer (not integer)) t)
        (%sc t '(or (not string) string)) (%sc t '(or integer symbol)))
  ((t t) (t t) (t t) (nil t)))

(deftest subtypep-cases.union-of-parts
  (list (%sc 'list '(or null cons)) (%sc 'number '(or real complex)) (%sc 'rational '(or integer ratio))
        (%sc 'list 'cons) (%sc 'number 'real))
  ((t t) (t t) (t t) (nil t) (nil t)))
