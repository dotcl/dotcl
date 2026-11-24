;;; An empty string has no non-bit element, so it coerces to a bit vector.

(deftest coerce-empty-string-bit-vector.empty
  (list (coerce "" 'bit-vector) (coerce "" 'simple-bit-vector)
        (length (coerce "" 'bit-vector)) (bit-vector-p (coerce "" 'bit-vector)))
  (#* #* 0 t))

(deftest coerce-empty-string-bit-vector.nonempty-still-errors
  (handler-case (coerce "ab" 'bit-vector) (type-error () :type-error))
  :type-error)
