;;; A bare * used as a type specifier, as an unsupplied &optional parameter of DEFTYPE
;;; gives, is the type T at the top level and inside AND, OR and NOT. The * of
;;; compound specifiers (cons, array, integer bounds) keeps its meaning.

(deftype tbs-non-nil (&optional type)
  (if type `(and (not null) ,type) '(not null)))
(deftype tbs-wild (&optional (x '*)) `(and (not null) ,x))

(deftest type-bare-star.top-level
  (list (typep 1 '*) (typep nil '*))
  (t t))

(deftest type-bare-star.and
  (list (typep 1 '(and integer *)) (typep 'a '(and integer *)) (typep nil '(and (not null) *)))
  (t nil nil))

(deftest type-bare-star.or-not
  (list (typep 1 '(or * integer)) (typep 1 '(or integer *)) (typep 1 '(not *)))
  (t t nil))

(deftest type-bare-star.deftype-default
  (list (typep 1 'tbs-non-nil) (typep nil 'tbs-non-nil) (typep 1 '(tbs-non-nil integer))
        (typep :a '(tbs-non-nil integer)) (typep 1 'tbs-wild) (typep nil 'tbs-wild))
  (t nil t nil t nil))

(deftest type-bare-star.subtypep
  (list (multiple-value-list (subtypep 'integer '*))
        (multiple-value-list (subtypep '(and integer *) 'integer)))
  ((t t) (t t)))

(defun tbs-declared (x) (declare (type (and integer *) x)) x)

(deftest type-bare-star.declared
  (list (tbs-declared 4)
        (handler-case (tbs-declared 'a) (type-error () :type-error)))
  (4 :type-error))

(deftest type-bare-star.compound-star-unchanged
  (list (typep (list 1) '(cons * *)) (typep (list 'a) '(cons integer *))
        (typep #(1 2) '(vector * 2)) (typep #(1 2) '(vector * 3))
        (typep 1 '(integer * 5)) (typep 9 '(integer * 5)) (typep -1 '(integer 0 *)))
  (t nil t nil t nil nil))

(deftest type-bare-star.ordinary-types
  (list (typep 1 '(and integer (not null))) (typep 'a '(member a b)) (typep 1 '(or string symbol))
        (typep 1 '(and)) (typep 1 '(or)))
  (t t nil t nil))
