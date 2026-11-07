;;; SUBTYPEP tells function signatures apart: arguments contravariant, results covariant
;;; (CLHS 4.2.2; a first value of T has to be true). A signature used to be flattened to
;;; the atom FUNCTION, so (function (integer) integer) was a subtype of
;;; (function (string) string). The atom and compound-with-* forms are unchanged.

(defun %sfs (a b) (first (multiple-value-list (subtypep a b))))

(deftest subtypep-function-signature.incompatible
  (list (%sfs '(function (integer) integer) '(function (string) string))
        (%sfs 'function '(function (integer) integer))
        (%sfs 'compiled-function '(function (integer) integer))
        (%sfs '(function (integer) number) '(function (number) integer))
        (%sfs '(values integer) '(values string)))
  (nil nil nil nil nil))

(deftest subtypep-function-signature.compatible
  (list (%sfs '(function (integer) integer) '(function (integer) integer))
        (%sfs '(function (number) integer) '(function (integer) number))
        (%sfs '(function (integer) integer) '(function (integer) t))
        (%sfs '(function (integer) integer) '(function * integer))
        (%sfs '(function (integer) (values integer integer)) '(function (integer) integer))
        (%sfs '(values integer) '(values number)))
  (t t t t t t))

(deftest subtypep-function-signature.the-atom
  (list (%sfs '(function (integer) integer) 'function)
        (%sfs '(function * *) 'function)
        (%sfs 'function '(function * *))
        (%sfs 'function 'function)
        (%sfs '(function (integer) integer) 't)
        (%sfs '(or (function (integer) integer) null) 'function)
        (%sfs 'null '(or (function (integer) integer) null)))
  (t t t t t nil t))
