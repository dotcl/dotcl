;;; TYPE-ERRORs signalled for a wrong argument carry an expected-type.

(defmacro tee-expected (form)
  `(handler-case (progn ,form nil)
     (type-error (e) (type-error-expected-type e))))

(deftest tee.bit-and
  (tee-expected (bit-and 5 5))
  (array bit))

(deftest tee.rational
  (tee-expected (rational 'a))
  real)

(deftest tee.numerator
  (tee-expected (numerator 1.5))
  rational)

(deftest tee.char-compare
  (tee-expected (char< #\a 1))
  character)

(deftest tee.subseq-start
  (tee-expected (subseq "abc" 5))
  (integer 0 3))

(deftest tee.sort-predicate
  (tee-expected (sort (list 1 2) 5))
  (or function symbol))

(deftest tee.vector-pop
  (not (null (tee-expected (vector-pop 5))))
  t)

(deftest tee.symbol-function
  (not (null (tee-expected (symbol-function 5))))
  t)

(deftest tee.apropos
  (tee-expected (apropos-list 5))
  (or string symbol character))

(deftest-emitting-only tee.slot-value
  (not (null (tee-expected (slot-value 5 'a))))
  t)

(deftest-emitting-only tee.add-method
  (not (null (tee-expected (add-method 5 5))))
  t)

(deftest tee.elt-index
  (tee-expected (elt "a" 5))
  (integer 0 0))

(deftest tee.subseq-bad-start
  (tee-expected (subseq "abc" 'a))
  integer)
