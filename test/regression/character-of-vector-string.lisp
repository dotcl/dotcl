;;; CHARACTER accepts any string designator of length one. It accepted the
;;; literal string representation only, so a string made by MAKE-ARRAY (stored
;;; as a character vector) signalled "not a character designator", and so did
;;; the symbol T, which is not represented as an ordinary symbol. Found by
;;; ansi-test's random type propagation tests (RANDOM-TYPE-PROP.CHARACTER).

(deftest character-of-vector-string
  (list (character (make-array 1 :element-type 'character :initial-element #\b))
        (character (make-array 1 :element-type 'base-char :initial-element #\c))
        (character (make-array 3 :element-type 'character :initial-element #\d
                                 :fill-pointer 1))
        (character t)
        (character 'x)
        (handler-case (character (make-array 2 :element-type 'character :initial-element #\a))
          (type-error () :type-error)))
  (#\b #\c #\d #\T #\X :type-error))
