;;; A :START / :END keyword of a string comparison that is not an integer (or NIL
;;; for :END) is a TYPE-ERROR, like the bounds of SUBSEQ and SEARCH.

(defmacro %scb (form)
  `(handler-case (progn ,form :no-error) (type-error () :type-error) (condition () :other)))

(deftest string-cmp-bound-type-error.float-start1
  (%scb (string= "abc" "abc" :start1 0.0)) :type-error)
(deftest string-cmp-bound-type-error.symbol-end1
  (%scb (string= "abc" "abc" :end1 'x)) :type-error)
(deftest string-cmp-bound-type-error.float-start2
  (%scb (string-equal "abc" "abc" :start2 0.5)) :type-error)
(deftest string-cmp-bound-type-error.string-end2
  (%scb (string< "abc" "abd" :end2 "a")) :type-error)
(deftest string-cmp-bound-type-error.valid-bounds-unchanged
  (list (string= "xabd" "abcy" :start1 1 :end2 3)
        (string= "abc" "abc" :end1 nil :start2 nil))
  (nil t))
