;;; TYPECASE and TYPEP of a compile-time structure name are compiled as an
;;; ISINST plus the type-name/:INCLUDE chain, not as a runtime TYPEP call.

(setf dotcl:*save-sil* t)

(defstruct tsi-base a)
(defstruct (tsi-child (:include tsi-base)) b)
(defstruct tsi-other c)

(defun %tsi-typep (o) (typep o 'tsi-child))

(defun %tsi-typecase (o)
  (typecase o
    (tsi-other :other)
    (tsi-base :base)
    (t :t)))

(defun %tsi-etype (o)
  (etypecase o
    (tsi-child :child)
    (tsi-base :base)))

(deftest typecase-struct-inline.values
  (let ((base (make-tsi-base))
        (child (make-tsi-child))
        (other (make-tsi-other)))
    (list (list (typep base 'tsi-base)
                (typep child 'tsi-base)
                (typep child 'tsi-child)
                (typep base 'tsi-child)
                (typep other 'tsi-base)
                (typep 42 'tsi-base))
          (list (%tsi-typecase base)
                (%tsi-typecase child)
                (%tsi-typecase other)
                (%tsi-typecase 42))
          (list (%tsi-etype base)
                (%tsi-etype child))))
  ((t t t nil nil nil) (:base :base :other :t) (:base :child)))

(defun %tsi-sil-inline-p (fn)
  (let ((sil (%flat-text (dotcl:function-sil fn))))
    (list (not (search "Runtime.IsTrueTypep" sil))
          (not (search "Runtime.Typep" sil))
          (and (search "ISINST" sil) t)
          (and (search "get_StructParent" sil) t))))

(deftest-emitting-only typecase-struct-inline.sil
  (list (%tsi-sil-inline-p #'%tsi-typep)
        (%tsi-sil-inline-p #'%tsi-typecase)
        (%tsi-sil-inline-p #'%tsi-etype))
  ((t t t t) (t t t t) (t t t t)))
