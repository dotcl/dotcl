;;; REMOVE has typed direct-call entries for the 4-arg (one keyword pair) and
;;; 6-arg (two keyword pairs) shapes, next to the existing 2-arg one. They must
;;; agree with the args-array path in every keyword combination — the direct
;;; entries run the shared keyword parser over their own small array, so a
;;; mistake there would silently change :count / :start / :from-end handling
;;; only for calls compiled at that exact arity.

(deftest remove-direct.test-string=
  (remove "a" (list "a" "b" "A") :test #'string=)
  ("b" "A"))

(deftest remove-direct.test-string-equal
  (remove "a" (list "a" "b" "A") :test #'string-equal)
  ("b"))

(deftest remove-direct.test-not
  (remove 1 (list 1 2 1 3) :test-not #'eql)
  (1 1))

(deftest remove-direct.key
  (remove 1 (list (cons 1 'a) (cons 2 'b) (cons 1 'c)) :key #'car)
  ((2 . b)))

(deftest remove-direct.count
  (remove 3 (list 1 2 3 4 3) :count 1)
  (1 2 4 3))

(deftest remove-direct.count-from-end
  (remove 3 (list 1 2 3 4 3) :count 1 :from-end t)
  (1 2 3 4))

(deftest remove-direct.start-end
  (remove 1 (list 1 1 1 1) :start 1 :end 3)
  (1 1))

(deftest remove-direct.key-and-test
  (remove "B" (list (cons 1 "a") (cons 2 "b")) :key #'cdr :test #'string-equal)
  ((1 . "a")))

(deftest remove-direct.key-and-count
  (remove 1 (list (cons 1 'a) (cons 1 'b) (cons 1 'c)) :key #'car :count 2)
  ((1 . c)))

(deftest remove-direct.string-sequence
  (remove #\a "banana" :count 2)
  "bnna")

(deftest remove-direct.vector-sequence
  (coerce (remove 3 (vector 1 2 3 4 3) :test #'=) 'list)
  (1 2 4))

;;; The 2-arg entry and the general args-array path must stay consistent with
;;; the new entries: same call built at run time through APPLY (no direct call).
(deftest remove-direct.apply-matches-direct
  (equal (remove 1 (list (cons 1 'a) (cons 2 'b)) :key #'car)
         (apply #'remove 1 (list (list (cons 1 'a) (cons 2 'b)) :key #'car)))
  t)

(deftest remove-direct.no-keywords
  (remove 2 (list 1 2 3 2))
  (1 3))

;;; An odd keyword tail is still a program error at the direct arity.
(deftest remove-direct.odd-keyword-tail-errors
  (handler-case (progn (remove 1 (list 1 2) :test) :no-error)
    (error () :error))
  :error)
