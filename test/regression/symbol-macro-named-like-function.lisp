;;; A symbol-macro named like a function, whose expansion calls that function more than once, is
;;; compiled in finite time: the operator of a call is not a variable reference to expand.

(defstruct smnf-rec a)
(defun smnf-twice (x) (* 2 x))

(defun %smnf-accessor (x)
  (symbol-macrolet ((smnf-rec-a (if (typep x 'structure-object)
                                    (smnf-rec-a x)
                                    (if (listp x) (smnf-rec-a x) 'unbound))))
    (list smnf-rec-a)))

(defun %smnf-function (x)
  (symbol-macrolet ((smnf-twice (list (smnf-twice x) (smnf-twice x))))
    smnf-twice))

(deftest symbol-macro-named-like-function.accessor
  (%smnf-accessor (make-smnf-rec :a 7))
  (7))

(deftest symbol-macro-named-like-function.function
  (%smnf-function 4)
  (8 8))

(deftest symbol-macro-named-like-function.variable-still-expands
  (let ((x 3))
    (symbol-macrolet ((smnf-twice (smnf-twice x)))
      (list smnf-twice (smnf-twice 5))))
  (6 10))
