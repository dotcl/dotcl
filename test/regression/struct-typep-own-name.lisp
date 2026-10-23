;;; TYPEP of a structure instance against its own type name answers before the
;;; type test is looked up; parents, other types and non-structures are unchanged.

(defstruct %stn-base a)
(defstruct (%stn-kid (:include %stn-base)) b)
(defstruct %stn-other)
(deftype %stn-alias () '%stn-kid)

(defun %stn-branch (x ty)
  (if (typep x ty) :yes :no))

(defun %stn-fused (x)
  (list (if (typep x '%stn-kid) 1 0) (if (typep x '%stn-base) 1 0)
        (if (typep x '%stn-other) 1 0)))

(deftest struct-typep-own-name.values
  (let ((k (make-%stn-kid)) (b (make-%stn-base)))
    (list (%stn-fused k) (%stn-fused b) (%stn-fused 3)
          (mapcar (lambda (ty) (%stn-branch k ty))
                  '(%stn-kid %stn-base %stn-alias structure-object t %stn-other symbol))
          (typep b '%stn-kid) (typep 'x '%stn-kid)))
  ((1 1 0) (0 1 0) (0 0 0) (:yes :yes :yes :yes :yes :no :no) nil nil))
