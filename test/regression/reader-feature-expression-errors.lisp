;;; A malformed feature expression after #+ or #- is a reader error (CLHS
;;; 24.1.2.1: a symbol, (and ...), (or ...) or (not x)); it used to read as
;;; false or true. As in SBCL only the parts that evaluation reaches are checked,
;;; so an operator of another implementation behind a feature test that is false
;;; is fine. Well-formed expressions, and any expression inside a form that
;;; *READ-SUPPRESS* skips, are unchanged.

(defun %fee-read (string)
  (handler-case (values (read-from-string string))
    (reader-error () :reader-error)))

(deftest reader-feature-expression-errors.malformed
  (mapcar #'%fee-read '("#+(not) x y" "#+(not a b) x y" "#+(foo a) x y"
                        "#+ 5 x y" "#+(and . a) x y" "#-(not) x y"
                        "#+(or a (not)) x y"))
  (:reader-error :reader-error :reader-error :reader-error :reader-error
   :reader-error :reader-error))

(deftest reader-feature-expression-errors.well-formed
  (mapcar #'%fee-read '("#+(and) x y" "#+(or) x y" "#+(not (or)) x y"
                        "#+(and common-lisp (not no-such-feature)) x y"
                        "#-no-such-feature x y"))
  (x y x x x))

(deftest reader-feature-expression-errors.not-reached
  (list (%fee-read "#+(and no-such-feature (version>= 11)) x y")
        (%fee-read "#+(or common-lisp (not)) x y"))
  (y x))

(deftest reader-feature-expression-errors.skipped-form
  (%fee-read "#+(or) (#+(bad) 1 2) z")
  z)

;;; The empty list is the symbol NIL, not a malformed expression: it is never a
;;; feature, so #+() skips the next form and #-() keeps it, like #+nil.
(deftest reader-feature-expression-errors.empty-list
  (mapcar #'%fee-read '("#+() x y" "#-() x y" "#+ () x y" "(a #+() b c)"
                        "#+(not ()) x y" "#-(not ()) x y"
                        "#+(or () nil) x y" "#+(or () common-lisp) x y"
                        "#+(and) x y" "#+(and ()) x y"))
  (y x y (a c) x y y x x y))

(deftest reader-feature-expression-errors.empty-list-malformed
  (mapcar #'%fee-read '("#+(not) x y" "#+(foo bar) x y" "#+ 5 x y"
                        "#+(not () ()) x y"))
  (:reader-error :reader-error :reader-error :reader-error))
