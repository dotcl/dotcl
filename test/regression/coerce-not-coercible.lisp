;;; COERCE signals a TYPE-ERROR when the object cannot be coerced to the
;;; requested type. A character or symbol is not a sequence, so it is not a
;;; string, and a rank-2 array is not a sequence either.

(defun %cnc (value type)
  (handler-case (coerce value type)
    (type-error () :type-error)))

(deftest coerce-not-coercible.character-or-symbol-to-string
  (list (%cnc #\a 'string)
        (%cnc #\a 'simple-string)
        (%cnc 'foo 'string)
        (%cnc 'foo 'base-string))
  (:type-error :type-error :type-error :type-error))

(deftest coerce-not-coercible.rank-two-array
  (let ((a (make-array '(2 3) :initial-element 0)))
    (list (%cnc a 'vector)
          (%cnc a 'list)
          (%cnc a 'simple-vector)
          (%cnc a 'bit-vector)))
  (:type-error :type-error :type-error :type-error))

(deftest coerce-not-coercible.valid-sequences-still-work
  (list (coerce (list #\a #\b) 'string)
        (coerce "abc" 'list)
        (coerce (vector 1 2) 'list)
        (coerce (make-array 2 :element-type 'character :initial-contents "ab")
                'string))
  ("ab" (#\a #\b #\c) (1 2) "ab"))
