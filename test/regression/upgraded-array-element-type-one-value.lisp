;;; UPGRADED-ARRAY-ELEMENT-TYPE returns exactly one value.

(deftest upgraded-array-element-type.one-value
  (list (length (multiple-value-list (upgraded-array-element-type '(member))))
        (length (multiple-value-list (upgraded-array-element-type '(cons nil))))
        (length (multiple-value-list (upgraded-array-element-type 'bit))))
  (1 1 1))
