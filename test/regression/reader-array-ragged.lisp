;;; #nA with contents that are not a regular nest of sequences (the rows have
;;; different lengths) is a reader error; it used to pad the short rows with NIL.

(deftest reader-array-ragged.is-error
  (handler-case (progn (read-from-string "#2a((1 2) (3))") :no-error)
    (reader-error () :reader-error))
  :reader-error)

(deftest reader-array-ragged.regular-ok
  (array-dimensions (read-from-string "#2a((1 2) (3 4))"))
  (2 2))

(deftest reader-array-ragged.deep-is-error
  (handler-case (progn (read-from-string "#3a(((1 2)) ((3)))") :no-error)
    (reader-error () :reader-error))
  :reader-error)
