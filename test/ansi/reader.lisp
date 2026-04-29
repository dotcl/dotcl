;;; reader.lisp — Tests for reader feature expressions (#+ #-)
;;; Note: These tests are READ by SBCL (cross-compiler), so we use features
;;; that behave identically on both SBCL and dotcl:
;;;   #+common-lisp → matches on both
;;;   #+no-such-feature-xyz → matches on neither
;;;   #+(or ...) / #+(and ...) / #+(not ...) → compound expressions

;;; --- simple #+ with matching feature ---

(deftest sharp-plus.match.1
  (list 1 #+common-lisp 2 3)
  (1 2 3))

;;; --- simple #+ with non-matching feature ---

(deftest sharp-plus.nomatch.1
  (list 1 #+no-such-feature-xyz 2 3)
  (1 3))

;;; --- simple #- ---

(deftest sharp-minus.match.1
  (list 1 #-common-lisp 2 3)
  (1 3))

(deftest sharp-minus.nomatch.1
  (list 1 #-no-such-feature-xyz 2 3)
  (1 2 3))

;;; --- compound OR ---

(deftest sharp-plus.or.match.1
  (list 1 #+(or common-lisp no-such-feature-xyz) 2 3)
  (1 2 3))

(deftest sharp-plus.or.nomatch.1
  (list 1 #+(or no-such-feature-aaa no-such-feature-bbb) 2 3)
  (1 3))

;;; --- compound AND ---

(deftest sharp-plus.and.match.1
  (list 1 #+(and common-lisp) 2 3)
  (1 2 3))

(deftest sharp-plus.and.nomatch.1
  (list 1 #+(and common-lisp no-such-feature-xyz) 2 3)
  (1 3))

;;; --- compound NOT ---

(deftest sharp-plus.not.match.1
  (list 1 #+(not no-such-feature-xyz) 2 3)
  (1 2 3))

(deftest sharp-plus.not.nomatch.1
  (list 1 #+(not common-lisp) 2 3)
  (1 3))

;;; --- nested compound ---

(deftest sharp-plus.nested.1
  (list 1 #+(or (and common-lisp) no-such-feature-xyz) 2 3)
  (1 2 3))

(deftest sharp-plus.nested.2
  (list 1 #+(and (not no-such-feature-xyz) common-lisp) 2 3)
  (1 2 3))

(deftest sharp-plus.nested.3
  (list 1 #+(and (not common-lisp) no-such-feature-xyz) 2 3)
  (1 3))

;;; --- #- with compound ---

(deftest sharp-minus.or.nomatch.1
  (list 1 #-(or no-such-feature-aaa no-such-feature-bbb) 2 3)
  (1 2 3))

(deftest sharp-minus.or.match.1
  (list 1 #-(or common-lisp no-such-feature-xyz) 2 3)
  (1 3))

;;; --- consecutive skips (the fatal bug scenario) ---

(deftest sharp-plus.consecutive-skip.1
  (list
    #+no-such-feature-aaa :a
    #+no-such-feature-bbb :b
    :end)
  (:end))

(deftest sharp-plus.consecutive-skip.2
  (list
    #+no-such-feature-aaa :a
    #+no-such-feature-bbb :b
    #+no-such-feature-ccc :c
    :d)
  (:d))

(deftest sharp-plus.consecutive-skip.3
  (list
    #+common-lisp :cl
    #+no-such-feature-aaa :a
    #+no-such-feature-bbb :b
    :end)
  (:cl :end))

;;; --- skip form is a list ---

(deftest sharp-plus.skip-list.1
  (list 1 #+no-such-feature-xyz (this should be skipped) 3)
  (1 3))

;;; --- skip form is a string ---

(deftest sharp-plus.skip-string.1
  (list 1 #+no-such-feature-xyz "skipped" 3)
  (1 3))

;;; --- empty OR / AND ---

(deftest sharp-plus.or-empty.1
  (list 1 #+(or) 2 3)
  (1 3))

(deftest sharp-plus.and-empty.1
  (list 1 #+(and) 2 3)
  (1 2 3))

(do-tests-summary)
