;;; DEFINE-CONDITION requires each parent to name a condition type.

(deftest define-condition-parent.standard-object
  (handler-case
      (progn (define-condition bad-condition-parent-standard-object (standard-object) ())
             :defined)
    (error () :definition-error))
  :definition-error)

(deftest define-condition-parent.structure-object
  (handler-case
      (progn (define-condition bad-condition-parent-structure-object (structure-object) ())
             :defined)
    (error () :definition-error))
  :definition-error)

(deftest define-condition-parent.undefined
  (handler-case
      (progn (define-condition bad-condition-parent-missing (no-such-condition-parent) ())
             :defined)
    (error () :definition-error))
  :definition-error)

(deftest define-condition-parent.not-registered
  (list (find-class 'bad-condition-parent-standard-object nil)
        (find-class 'bad-condition-parent-missing nil))
  (nil nil))

(deftest define-condition-parent.valid
  (progn
    (define-condition good-condition-parent-child (simple-error) ())
    (typep (make-condition 'good-condition-parent-child) 'condition))
  t)

(deftest define-condition-parent.default
  (progn
    (define-condition default-condition-parent () ())
    (typep (make-condition 'default-condition-parent) 'condition))
  t)

;;; A reader conditional with no branch for this implementation leaves NIL.
(deftest define-condition-parent.nil
  (handler-case
      (progn (define-condition bad-condition-parent-nil (nil) ())
             :defined)
    (error () :definition-error))
  :definition-error)
