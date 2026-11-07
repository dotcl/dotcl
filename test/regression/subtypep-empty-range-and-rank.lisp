;;; SUBTYPEP: a numeric range with no member is empty (a subtype of everything),
;;; and an array type that fixes only the rank is not inside one that fixes a length.

(deftest subtypep-empty-range.integer
  (values (subtypep '(integer 256 10) 'bignum) (subtypep '(integer 256 10) nil)
          (subtypep '(integer (5) (6)) nil) (subtypep '(integer 5 5) nil) (subtypep '(integer 0 10) nil))
  t t t nil nil)

(deftest subtypep-empty-range.real-and-float
  (values (subtypep '(real 2.5 1) nil) (subtypep '(float 0.5 0) 'cons) (subtypep '(real (1) 1) nil)
          (subtypep '(real 1 1) nil) (subtypep '(float 0.0 1.0) nil))
  t t t nil nil)

(deftest subtypep-rank-vs-length.not-subtype
  (values (subtypep '(array character 1) '(vector * 3))
          (subtypep '(array * 2) '(array * (3 4)))
          (subtypep '(vector character) '(vector character 3)))
  nil nil nil)

(deftest subtypep-rank-vs-length.still-subtype
  (values (subtypep '(array * 2) '(array * (* *))) (subtypep '(vector * 3) '(array * 1))
          (subtypep '(array character (3)) '(array character 1)) (subtypep '(array * 1) '(vector * *)))
  t t t t)
