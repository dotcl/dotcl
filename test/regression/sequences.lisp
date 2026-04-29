;;; Sequence and list regression tests

;;; Basic list operations
(deftest seq-mapcar
  (mapcar #'1+ '(1 2 3))
  (2 3 4))

(deftest seq-mapcar-multi
  (mapcar #'+ '(1 2 3) '(10 20 30))
  (11 22 33))

(deftest seq-reduce
  (reduce #'+ '(1 2 3 4 5))
  15)

;;; MAP with vector result type
(deftest seq-map-to-vector
  (coerce (map 'vector #'1+ '(1 2 3)) 'list)
  (2 3 4))

;;; MAP with OR type — conflicting element types should signal type-error (D578)
(deftest seq-map-or-type-conflict
  (handler-case
      (map '(or (vector bit) (vector t)) #'identity '(1 0 1))
    (type-error () :type-error))
  :type-error)

;;; MAP with OR type — compatible element types should work
(deftest seq-map-or-type-same-elt
  (let ((result (map '(or (vector t 5) (vector t 3)) #'identity '(1 2 3))))
    (and (vectorp result) (equalp result #(1 2 3))))
  t)

;;; sort
(deftest seq-sort
  (sort (list 3 1 4 1 5) #'<)
  (1 1 3 4 5))

;;; find
(deftest seq-find
  (find 3 '(1 2 3 4 5))
  3)

;;; position
(deftest seq-position
  (position 3 '(1 2 3 4 5))
  2)

;;; remove
(deftest seq-remove
  (remove 3 '(1 2 3 4 3 5))
  (1 2 4 5))

;;; subseq
(deftest seq-subseq
  (subseq '(a b c d e) 1 4)
  (b c d))

;;; coerce list to vector
(deftest seq-coerce-to-vector
  (coerce (coerce '(1 2 3) 'vector) 'list)
  (1 2 3))

;;; coerce vector to list
(deftest seq-coerce-to-list
  (coerce #(1 2 3) 'list)
  (1 2 3))

;;; every / some / notany
(deftest seq-every
  (every #'numberp '(1 2 3))
  t)

(deftest seq-some
  (some #'oddp '(2 3 4))
  t)

(deftest seq-notany
  (notany #'stringp '(1 2 3))
  t)
