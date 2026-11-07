;;; A ratio is [sign] digits+ / digits+ (CLHS 2.3.1.1): the sign belongs to the numerator only.
;;; "6/-4" and "-6/-4" are symbols (SBCL), not -3/2 and 3/2. #x6/-4 is a reader error.

(deftest reader-ratio-denominator-sign.symbols
  (list (symbolp (read-from-string "6/-4")) (symbol-name (read-from-string "6/-4"))
        (symbolp (read-from-string "-6/-4")) (symbolp (read-from-string "6/+4")))
  (t "6/-4" t t))

(deftest reader-ratio-denominator-sign.valid-ratios
  (list (read-from-string "6/4") (read-from-string "-6/4") (read-from-string "+6/4")
        (read-from-string "#x-6/4") (read-from-string "#3r1/2") (let ((*read-base* 16)) (read-from-string "A/4")))
  (3/2 -3/2 3/2 -3/2 1/2 5/2))

(deftest reader-ratio-denominator-sign.radix-reader-errors
  (list (handler-case (read-from-string "#x6/-4") (reader-error () :reader-error))
        (handler-case (read-from-string "#3r1/-2") (reader-error () :reader-error)))
  (:reader-error :reader-error))
