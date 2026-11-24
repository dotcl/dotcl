;;; Two unrelated standard classes, or two unrelated condition classes, can have a common
;;; subclass, so an intersection of them is not known to be empty. Structure classes have
;;; single inheritance, and a standard, a condition and a structure class never share a
;;; subclass. Expected values are SBCL's.

(define-condition suc-ca () ())
(define-condition suc-cb () ())
(defclass suc-sa () ())
(defclass suc-sb () ())
(defstruct suc-s1)
(defstruct suc-s2)

(deftest subtypep-unrelated-classes.condition
  (multiple-value-list (subtypep '(and suc-ca suc-cb) nil))
  (nil nil))

(deftest subtypep-unrelated-classes.standard
  (multiple-value-list (subtypep '(and suc-sa suc-sb) nil))
  (nil nil))

(deftest subtypep-unrelated-classes.double-negation
  (multiple-value-list (subtypep '(and suc-sa (not (not suc-sb))) nil))
  (nil nil))

(deftest subtypep-unrelated-classes.pass-shape
  (multiple-value-list (subtypep '(and suc-ca (not (and suc-ca (not suc-cb)))) nil))
  (nil nil))

(deftest subtypep-unrelated-classes.structures
  (multiple-value-list (subtypep '(and suc-s1 suc-s2) nil))
  (t t))

(deftest subtypep-unrelated-classes.families
  (mapcar (lambda (ty) (car (multiple-value-list (subtypep ty nil))))
          '((and suc-ca suc-sa) (and suc-s1 suc-sa) (and suc-s1 suc-ca)))
  (t t t))
