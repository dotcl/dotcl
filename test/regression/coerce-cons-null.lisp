;;; COERCE of a sequence to CONS or NULL makes the list (the list must be non-empty for CONS,
;;; empty for NULL), and of the symbol T to CHARACTER makes #\T.

(deftest coerce-cons-null.cons
  (list (coerce "abc" 'cons) (coerce #(1 2 3) 'cons) (coerce '(1 2) 'cons) (coerce #*101 'cons)
        (handler-case (coerce "" 'cons) (type-error () :type-error))
        (handler-case (coerce nil 'cons) (type-error () :type-error)))
  ((#\a #\b #\c) (1 2 3) (1 2) (1 0 1) :type-error :type-error))

(deftest coerce-cons-null.null
  (list (coerce "" 'null) (coerce #() 'null) (coerce nil 'null)
        (handler-case (coerce "a" 'null) (type-error () :type-error))
        (handler-case (coerce '(1) 'null) (type-error () :type-error)))
  (nil nil nil :type-error :type-error))

(deftest coerce-cons-null.character-designator
  (list (coerce 't 'character) (coerce 'a 'character) (coerce "b" 'character)
        (handler-case (coerce 'ab 'character) (type-error () :type-error)))
  (#\T #\A #\b :type-error))
