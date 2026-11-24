;;; A non-integer :START1 / :START2 of SEARCH and MISMATCH is a type-error that names the value
;;; (it used to name NIL and FIXNUM).

(defmacro %tek (form)
  `(handler-case ,form
     (type-error (e) (list (type-error-datum e) (type-error-expected-type e)))
     (error (e) (list :other (type-of e)))))

(deftest type-error-index-keyword.search
  (list (%tek (search "ab" "ab" :start1 'a)) (%tek (search "ab" "ab" :start2 1.5)))
  ((a unsigned-byte) (1.5 unsigned-byte)))

(deftest type-error-index-keyword.mismatch
  (list (%tek (mismatch "ab" "ab" :start1 'a)) (%tek (mismatch "ab" "ab" :start2 "x")))
  ((a unsigned-byte) ("x" unsigned-byte)))

(deftest type-error-index-keyword.still-works
  (list (search "b" "abc" :start2 1) (mismatch "abc" "abd" :start1 1 :start2 1))
  (1 2))
