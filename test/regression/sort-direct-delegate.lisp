;;; SORT / STABLE-SORT have typed direct-call entries for (sort seq pred) and
;;; (sort seq pred :key f). The 4-arg entry decodes the keyword pair itself, so
;;; it must agree with the args-array path on :key, on :allow-other-keys, and on
;;; rejecting unknown keywords.

(deftest sort-direct.no-key
  (sort (list 3 1 2) #'<)
  (1 2 3))

(deftest sort-direct.key
  (sort (list (cons 3 'c) (cons 1 'a) (cons 2 'b)) #'< :key #'car)
  ((1 . a) (2 . b) (3 . c)))

(deftest sort-direct.key-symbol-designator
  (sort (list "bb" "a" "ccc") #'< :key 'length)
  ("a" "bb" "ccc"))

;;; STABLE-SORT shares the entries; ordering of equal keys is not asserted here
;;; (see the separate stability work).
(deftest sort-direct.stable-sort-key
  (stable-sort (list (cons 2 'b) (cons 0 'c) (cons 1 'a)) #'< :key #'car)
  ((0 . c) (1 . a) (2 . b)))

(deftest sort-direct.vector
  (coerce (sort (vector 3 1 2) #'< :key #'identity) 'list)
  (1 2 3))

(deftest sort-direct.allow-other-keys-alone
  (sort (list 3 1 2) #'< :allow-other-keys t)
  (1 2 3))

(deftest sort-direct.unknown-keyword-errors
  (handler-case (progn (sort (list 3 1 2) #'< :bogus t) :no-error)
    (error () :error))
  :error)

(deftest sort-direct.unknown-keyword-allowed-with-aok
  (sort (list 3 1 2) #'< :bogus t :allow-other-keys t)
  (1 2 3))

;;; APPLY takes the args-array path — it must produce the same answer.
(deftest sort-direct.apply-matches-direct
  (equal (sort (list 3 1 2) #'< :key #'identity)
         (apply #'sort (list (list 3 1 2) #'< :key #'identity)))
  t)
