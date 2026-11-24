;;; SUBTYPEP answers for RATIO against rational ranges, a range of rationals with no
;;; integer in it, and BOOLEAN. RATIO against (RATIONAL -1/2 1/2) used to answer T
;;; (the type was compared by its name only); the open range (RATIONAL (0) (1)) holds
;;; only ratios, and BOOLEAN is (MEMBER NIL T).

(defun %sr (a b) (multiple-value-list (subtypep a b)))

(deftest subtypep-ratio.not-within-a-narrow-range
  (list (%sr 'ratio '(rational -1/2 1/2)) (%sr 'ratio '(rational 0 1)) (%sr 'ratio '(rational 1/3 1/2))
        (%sr 'ratio '(rational -100 100)) (%sr 'ratio '(real -1/2 1/2)) (%sr 'ratio '(integer -1 1)))
  ((nil t) (nil t) (nil t) (nil t) (nil t) (nil t)))

(deftest subtypep-ratio.range-without-integers
  (list (%sr '(rational (0) (1)) 'ratio) (%sr '(rational (1/2) (3/2)) 'ratio) (%sr '(rational 0 1) 'ratio)
        (%sr '(rational 1/2 1) 'ratio) (%sr '(rational (0) (1)) 'rational) (%sr '(eql 1/2) 'ratio))
  ((t t) (nil t) (nil t) (nil t) (t t) (t t)))

(deftest subtypep-ratio.boolean
  (list (%sr 'boolean '(member nil t)) (%sr '(member nil t) 'boolean) (%sr 'boolean 'symbol)
        (%sr 'null 'boolean) (%sr 'boolean 'null) (%sr 'boolean 'integer))
  ((t t) (t t) (t t) (t t) (nil t) (nil t)))
