;;; SUBTYPEP decides a small finite range of integers (an INTEGER or MOD
;;; range, or an AND with one) by the integers it holds.

(deftest subtypep-small-range.mod-in-member
  (list (multiple-value-list (subtypep '(mod 3) '(member 0 1 2)))
        (multiple-value-list (subtypep '(integer 0 2) '(member 0 1 2 4 8)))
        (multiple-value-list (subtypep '(mod 3) '(member 0 1))))
  ((t t) (t t) (nil t)))

(deftest subtypep-small-range.in-union-of-ranges
  (list (multiple-value-list
         (subtypep '(and (integer 0 20) (not (eql 10)))
                   '(or (integer 0 9) (integer 11 20))))
        (multiple-value-list
         (subtypep '(and (integer 0 20) (not (member 10 11)))
                   '(or (integer 0 9) (integer 12 20))))
        (multiple-value-list
         (subtypep '(and (integer 0 20) (not (eql 10)))
                   '(or (integer 0 9) (integer 12 20)))))
  ((t t) (t t) (nil t)))

(deftest subtypep-small-range.unchanged
  (list (multiple-value-list (subtypep '(integer 0 5) '(integer 0 9)))
        (multiple-value-list (subtypep '(integer 0 5) 'fixnum))
        (multiple-value-list (subtypep '(integer 0 5) '(or (integer 0 2) (integer 3 5)))))
  ((t t) (t t) (t t)))

(deftest subtypep-small-range.wide-range-not-enumerated
  (list (multiple-value-list (subtypep '(signed-byte 64) '(eql 5)))
        (multiple-value-list (subtypep '(unsigned-byte 64) '(member 0 1))))
  ((nil t) (nil t)))
