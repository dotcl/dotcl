;;; The string comparison family has 4-arg direct entries for a single bounds
;;; keyword pair — (string= name prefix :end1 n), which is how prefix checks are
;;; written and by far the most common keyworded shape. The entry decodes the pair
;;; itself, so it must agree with the args-array path in every case; anything it
;;; does not handle (:allow-other-keys, a non-integer bound, an unknown keyword,
;;; an out-of-range bound) has to fall back to that path rather than answer.
;;;
;;; Each check below compares the fixed-arity call against APPLY of the same call,
;;; which takes the args-array path.

(defmacro scd-same (form fn a b kw val)
  `(deftest ,form
     (equal ,(list fn a b kw val) (apply ,(list 'function fn) ,a ,b (list ,kw ,val)))
     t))

(scd-same scd-string=-end1        string= "abcdef" "abcxyz" :end1 3)
(scd-same scd-string=-end1-ne     string= "abcdef" "abcxyz" :end1 4)
(scd-same scd-string=-start1      string= "xxabc" "abc" :start1 2)
(scd-same scd-string=-start2      string= "abc" "xxabc" :start2 2)
(scd-same scd-string/=            string/= "abcdef" "abcxyz" :end1 4)
(scd-same scd-string<             string< "abc" "abd" :end1 3)
(scd-same scd-string>             string> "abd" "abc" :end1 3)
(scd-same scd-string<=            string<= "abc" "abc" :end1 2)
(scd-same scd-string>=            string>= "abc" "abc" :end1 2)
(scd-same scd-string-equal        string-equal "ABCdef" "abcxyz" :end1 3)
(scd-same scd-string-not-equal    string-not-equal "ABCdef" "abcxyz" :end1 4)
(scd-same scd-string-lessp        string-lessp "ABC" "abd" :end1 3)
(scd-same scd-string-greaterp     string-greaterp "ABD" "abc" :end1 3)
(scd-same scd-string-not-greaterp string-not-greaterp "ABC" "abc" :end1 3)
(scd-same scd-string-not-lessp    string-not-lessp "ABC" "abc" :end1 3)

;;; Values, not just agreement, for the shape this was built for.
(deftest scd-prefix-check-true
  (string= "__LABELFN_x" "__LABELFN_" :end1 10)
  t)

(deftest scd-prefix-check-false
  (string= "__OTHER___x" "__LABELFN_" :end1 10)
  nil)

(deftest scd-mismatch-index
  (string/= "abcdef" "abcxyz" :end1 5)
  3)

;;; Symbols and characters are string designators here too.
(deftest scd-symbol-designator
  (string= 'abc "ABCDEF" :end2 3)
  t)

;;; Shapes the 4-arg entry must hand to the general path.
(deftest scd-allow-other-keys-alone
  (string= "abc" "abc" :allow-other-keys t)
  t)

(deftest scd-nil-bound
  (string= "abc" "abc" :end1 nil)
  t)

(deftest scd-unknown-keyword-errors
  (handler-case (progn (string= "abc" "abc" :bogus 1) :no-error)
    (error () :error))
  :error)

;;; An out-of-range bound is not validated by this implementation (ANSI wants a
;;; bounding index); whatever it does, both paths must do the same thing, which is
;;; what the 4-arg entry guarantees by declining and falling back.
(deftest scd-out-of-range-bound-agrees-with-array-path
  (equal (handler-case (string= "abc" "abc" :end1 99) (error () :error))
         (handler-case (apply #'string= "abc" "abc" (list :end1 99)) (error () :error)))
  t)

(deftest scd-two-arg-unaffected
  (list (string= "abc" "abc") (string= "abc" "abd"))
  (t nil))

;;; Bounding indices are now validated (CLHS: 0 <= start <= end <= length). They
;;; used to be taken as given, so a too-large :end quietly compared the whole
;;; string — and (subseq '(1 2 3) 0 99) built a list padded with NILs.
(defmacro scd-errors (name form)
  `(deftest ,name (handler-case (progn ,form :no-error) (type-error () :type-error)) :type-error))

(scd-errors scd-end1-too-large   (string= "abc" "abc" :end1 99))
(scd-errors scd-start1-too-large (string= "abc" "abc" :start1 5))
(scd-errors scd-start-after-end  (string= "abc" "abc" :start1 2 :end1 1))
(scd-errors scd-negative-bound   (string= "abc" "abc" :end1 -1))
(scd-errors scd-end2-too-large   (string-equal "abc" "abc" :end2 99))
(scd-errors scd-subseq-string    (subseq "abc" 0 99))
(scd-errors scd-subseq-list      (subseq (list 1 2 3) 0 99))
(scd-errors scd-subseq-vector    (subseq (vector 1 2 3) 0 99))
(scd-errors scd-subseq-start     (subseq (list 1 2 3) 9))

;;; Valid bounds — including the empty range and the full range — still work.
(deftest scd-bounds-still-ok
  (list (string= "abc" "abd" :end1 2 :end2 2)
        (string= "abc" "abc" :start1 3 :start2 3)
        (subseq "abc" 3)
        (subseq (list 1 2 3) 1 3))
  (t t "" (2 3)))

;;; The sequence functions validate :start/:end the same way.
(scd-errors scd-position-end   (position 1 (list 1 2 3) :end 99))
(scd-errors scd-find-start     (find 1 (list 1 2 3) :start 99))
(scd-errors scd-count-end      (count 1 (list 1 2 3) :end 99))
(scd-errors scd-remove-list    (remove 1 (list 1 2 3) :end 99))
(scd-errors scd-remove-vector  (remove 1 (vector 1 2 3) :end 99))
(scd-errors scd-substitute     (substitute 9 1 (list 1 2 3) :end 99))
(scd-errors scd-remove-dups    (remove-duplicates (list 1 2 3) :end 99))
(scd-errors scd-position-if    (position-if #'oddp (list 1 2 3) :end 99))

;;; Valid ranges on sequences keep working, including the empty one.
(deftest scd-sequence-bounds-still-ok
  (list (position 3 (list 1 2 3) :start 1)
        (count 1 (list 1 2 3) :end 1)
        (remove 1 (list 1 2 3) :end 1)
        (find 1 (list 1 2 3) :start 3 :end 3)
        (subseq (list 1 2 3) 3))
  (2 1 (2 3) nil nil))

;;; fill / search / reduce / replace take bounding indices too.
(scd-errors scd-fill-list      (fill (list 1 2 3) 0 :end 99))
(scd-errors scd-fill-vector    (fill (vector 1 2) 0 :end 9))
(scd-errors scd-search-end2    (search "a" "abc" :end2 99))
(scd-errors scd-search-start1  (search "a" "abc" :start1 9))
(scd-errors scd-reduce-list    (reduce #'+ (list 1 2 3) :end 99))
(scd-errors scd-reduce-vector  (reduce #'+ (vector 1 2) :end 9))
(scd-errors scd-replace-start1 (replace (list 1 2 3) (list 9) :start1 99))
(scd-errors scd-replace-end1   (replace (list 1 2 3) (list 9) :end1 99))

(deftest scd-fill-search-reduce-replace-ok
  (list (fill (list 1 2 3) 0 :start 1)
        (search "b" "abc")
        (reduce #'+ (list 1 2 3) :start 1)
        (replace (list 1 2 3) (list 9 9) :start1 1 :end1 2))
  ((1 0 0) 1 5 (1 9 3)))
