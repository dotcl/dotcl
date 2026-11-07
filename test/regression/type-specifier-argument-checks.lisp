;;; Malformed type specifiers are errors, near-valid ones still work:
;;; an INTEGER with a third bound (also through a DEFTYPE expansion), SATISFIES with a
;;; non-symbol, UPGRADED-ARRAY-ELEMENT-TYPE of a name that is no type.

(deftype tsac-bad-int () '(integer 1 2 3))
(deftype tsac-ok-int () '(integer 1 2))

(defmacro %tsac-errors-p (form) `(handler-case (progn ,form nil) (error () t)))

(deftest type-specifier-argument-checks.integer-bounds
  (list (%tsac-errors-p (typep 1 '(integer 1 2 3)))
        (%tsac-errors-p (typep 1 'tsac-bad-int))
        (typep 1 'tsac-ok-int) (typep 3 'tsac-ok-int)
        (typep 1 '(integer 1)) (typep 1 '(integer * *)) (typep 5 '(integer (1) 5)))
  (t t t nil t t t))

(deftest type-specifier-argument-checks.satisfies
  (list (handler-case (typep 1 '(satisfies (lambda (x) x))) (type-error () :type-error))
        (handler-case (typep 1 '(satisfies 5)) (type-error () :type-error))
        (typep 1 '(satisfies integerp)) (typep "a" '(satisfies integerp)))
  (:type-error :type-error t nil))

(deftest type-specifier-argument-checks.upgraded-array-element-type
  (list (%tsac-errors-p (upgraded-array-element-type 'bogus-type-xyz))
        (upgraded-array-element-type 'character)
        (upgraded-array-element-type '(unsigned-byte 8))
        (upgraded-array-element-type 'integer)
        (upgraded-array-element-type 't))
  (t character (unsigned-byte 8) t t))
