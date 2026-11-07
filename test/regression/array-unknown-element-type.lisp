;;; An element type that names no type at all is an error, not a general array.

(deftest array-unknown-element-type.misspelled
  (handler-case (progn (make-array 3 :element-type 'no-such-elt-type-aue) :no-error)
    (error () :error))
  :error)

(deftest array-unknown-element-type.known-class-still-general
  (progn (defstruct aue-struct a)
         (array-element-type (make-array 2 :element-type 'aue-struct)))
  t)

(deftest array-unknown-element-type.deftype-works
  (progn (deftype aue-octet () '(unsigned-byte 8))
         (array-element-type (make-array 2 :element-type 'aue-octet)))
  (unsigned-byte 8))
