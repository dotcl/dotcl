;;; (SUBTYPEP 'a 'b) with constant specifiers is folded when both name only built-in
;;; types; a type or class defined after the function was compiled is not frozen.

(defun %stl-builtin () (multiple-value-list (subtypep '(integer 0 100) 'fixnum)))
(defun %stl-later-deftype () (multiple-value-list (subtypep 'stl-later-type 'integer)))
(defun %stl-later-class () (multiple-value-list (subtypep 'stl-later-class 'standard-object)))

(deftype stl-later-type () '(integer 0 5))
(defclass stl-later-class () ())

(deftest subtypep-constant-late-definition.builtin
  (%stl-builtin)
  (t t))

(deftest subtypep-constant-late-definition.later-deftype
  (%stl-later-deftype)
  (t t))

(deftest subtypep-constant-late-definition.later-class
  (%stl-later-class)
  (t t))
