;;; The array type names are disjoint where their element types or simplicity differ:
;;; SIMPLE-VECTOR (element type T) has nothing in common with STRING or BIT-VECTOR.

(defun %sad (a b) (multiple-value-list (subtypep a b)))

(deftest subtypep-array-names.not-string
  (list (%sad 'simple-vector '(not string)) (%sad 'bit-vector '(not string))
        (%sad 'simple-bit-vector '(not string)) (%sad 'simple-vector '(not base-string))
        (%sad 'string '(not simple-vector)) (%sad 'simple-vector '(and vector (not string)))
        (%sad 'bit-vector '(and vector (not string))))
  ((t t) (t t) (t t) (t t) (t t) (t t) (t t)))

(deftest subtypep-array-names.overlapping-unchanged
  (list (%sad 'vector '(not string)) (%sad 'string '(not vector)) (%sad 'simple-string '(not string))
        (%sad 'array '(not string)) (%sad 'bit-vector '(not simple-bit-vector))
        (%sad 'simple-vector '(not (vector t))) (%sad 'simple-string '(not simple-vector)))
  ((nil t) (nil t) (nil t) (nil t) (nil t) (nil t) (t t)))
