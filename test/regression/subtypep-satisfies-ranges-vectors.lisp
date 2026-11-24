;;; SUBTYPEP: (SATISFIES P) for a standard predicate is the type it tests, integer ranges
;;; that share no integer are disjoint, a cons type is below LIST, and a vector of anything
;;; is not a (VECTOR (UNSIGNED-BYTE 8)) (the compound element type was read as the wildcard).

(defmacro %sub (a b) `(multiple-value-list (subtypep ',a ',b)))

(deftest subtypep-satisfies-ranges-vectors.satisfies
  (list (%sub (satisfies integerp) integer) (%sub integer (satisfies integerp))
        (%sub (satisfies symbolp) symbol) (%sub (satisfies stringp) string)
        (%sub (and (satisfies integerp) (integer 0 5)) (integer 0 5))
        (%sub (satisfies evenp) integer))
  ((t t) (t t) (t t) (t t) (t t) (nil nil)))

(deftest subtypep-satisfies-ranges-vectors.ranges
  (list (%sub (and (integer 0 5) (integer 10 20)) nil)
        (%sub (integer 10 20) (not (integer 0 5)))
        (%sub (integer 3 9) (and (integer 0 100) (not (integer 10 20))))
        (%sub bignum (not (integer 0 5)))
        (%sub (integer 0 5) (integer 3 9))
        (%sub (and (integer 0 5) (integer 3 9)) (integer 3 5)))
  ((t t) (t t) (t t) (t t) (nil t) (t t)))

(deftest subtypep-satisfies-ranges-vectors.cons-null-keyword
  (list (%sub (cons t t) list) (%sub (cons integer) sequence) (%sub (cons integer integer) (and list (not null)))
        (%sub null (member nil)) (%sub keyword (not null)) (%sub keyword (not list))
        (%sub (not list) atom) (%sub list (cons t t)) (%sub cons (cons integer)))
  ((t t) (t t) (t t) (t t) (t t) (t t) (t t) (nil t) (nil t)))

(deftest subtypep-satisfies-ranges-vectors.vectors
  (list (%sub vector (vector (unsigned-byte 8))) (%sub vector (vector (integer 0 255)))
        (%sub vector (vector (member 1 2))) (%sub (array * (3)) (vector (unsigned-byte 8)))
        (%sub (vector (unsigned-byte 8)) vector) (%sub (vector t) (vector t))
        (%sub (vector (unsigned-byte 8) 3) (vector (unsigned-byte 8))))
  ((nil t) (nil t) (nil t) (nil t) (t t) (t t) (t t)))
