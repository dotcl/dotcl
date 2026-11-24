;;; :END1 / :END2 of SEARCH, MISMATCH and REPLACE is an integer or NIL; any other value is a type-error
;;; and is not ignored.

(defmacro %semr (form) `(handler-case (progn ,form :no-error) (type-error () :type-error)))

(deftest search-mismatch-replace-end-argument.bad
  (list (%semr (search "a" "abc" :end2 1.5))
        (%semr (search "a" "abc" :end1 "1"))
        (%semr (mismatch "abc" "abd" :end1 1.5))
        (%semr (mismatch "abc" "abd" :end2 #\a))
        (%semr (replace (list 1 2 3) '(a b) :end1 1.5))
        (%semr (replace (list 1 2 3) '(a b) :end2 "1"))
        (%semr (search "a" "abc" :end2 (expt 2 70))))
  (:type-error :type-error :type-error :type-error :type-error :type-error :type-error))

(deftest search-mismatch-replace-end-argument.good
  (list (search "b" "abc" :end2 nil) (search "b" "abc" :end2 2) (mismatch "abc" "abd" :end1 nil :end2 nil)
        (mismatch "abc" "abd" :end1 2) (replace (list 1 2 3) '(a b) :end1 nil)
        (replace (list 1 2 3) '(a b) :end1 1))
  (1 1 2 2 (a b 3) (a 2 3)))
