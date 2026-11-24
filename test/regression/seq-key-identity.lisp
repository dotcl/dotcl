;;; :KEY #'IDENTITY (given as the function, a symbol or a variable) is no key: every sequence function
;;; answers as it does without :KEY, with :TEST given as a function variable too, on strings, vectors
;;; and lists; any other key still applies.

(defvar *skid-key* #'identity)
(defvar *skid-test* #'eql)

(deftest seq-key-identity.answers
  (let ((s "a,b,,c") (v (vector 1 2 3 2 1)) (l (list 1 2 3 2 1)))
    (list (position #\, s :key #'identity) (position #\, s :key 'identity :start 2)
          (position #\, s :key *skid-key* :test *skid-test* :start 2)
          (position #\, s :key *skid-key* :from-end t)
          (position 2 v :key #'identity :test #'eql) (position 2 l :key *skid-key* :start 2)
          (find #\b s :key #'identity) (find 3 v :key *skid-key*) (find 9 l :key #'identity)
          (count #\, s :key #'identity) (count 2 v :key *skid-key* :test *skid-test*)
          (remove 2 l :key #'identity) (remove #\, s :key 'identity)
          (coerce (substitute 9 2 v :key #'identity) 'list)
          (member 3 l :key #'identity) (assoc 2 '((1 . a) (2 . b)) :key #'identity)
          (position 3 v :key #'1+) (position -1 l :key #'-) (find 4 l :key #'1+)
          (position 2 v :key (lambda (x) (values x 'ignored)))))
  (1 3 3 4 1 3 #\b 3 nil 3 2 (1 3 1) "abc" (1 9 3 9 1)
    (3 2 1) (2 . b) 1 0 3 1))
