;;; (NOT (NOT X)) is X and (MEMBER) is NIL for SUBTYPEP.

(deftest subtypep-double-negation.both-ways
  (list (multiple-value-list (subtypep 'integer '(not (not integer))))
        (multiple-value-list (subtypep '(not (not integer)) 'integer))
        (multiple-value-list (subtypep '(signed-byte) '(not (not integer))))
        (multiple-value-list (subtypep 'string '(not (not string))))
        (multiple-value-list (subtypep 'fixnum '(not (not (not string))))))
  ((t t) (t t) (t t) (t t) (t t)))

(deftest subtypep-double-negation.not-subtype
  (list (multiple-value-list (subtypep 'string '(not (not integer))))
        (multiple-value-list (subtypep '(not (not integer)) 'string)))
  ((nil t) (nil t)))

(deftest subtypep-empty-member
  (list (multiple-value-list (subtypep '(member) 'integer))
        (multiple-value-list (subtypep 'integer '(member)))
        (multiple-value-list (subtypep '(simple-array bit (4)) '(member)))
        (multiple-value-list (subtypep '(member) '(member))))
  ((t t) (nil t) (nil t) (t t)))
