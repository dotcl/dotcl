;;; VALIDATE-SUPERCLASS accepts the CLASS-PROTOTYPE of a metaclass as its class argument and judges
;;; it by the metaclass it is an instance of (values from SBCL 2.6.8). compatible-metaclasses'
;;; :VALIDATE-AS option calls it that way.
(defclass vsmp-plain () ())

(deftest validate-superclass-metaclass-prototype.default
  (let ((p (dotcl-mop:class-prototype (find-class 'standard-class))))
    (list (dotcl-mop:validate-superclass p (find-class 'vsmp-plain))
          (dotcl-mop:validate-superclass p (find-class 'standard-object))
          (dotcl-mop:validate-superclass p (find-class 'standard-class))))
  (t t t))

(deftest validate-superclass-metaclass-prototype.same-metaclass
  (let ((p (dotcl-mop:class-prototype (find-class 'standard-class))))
    (list (dotcl-mop:validate-superclass (find-class 'vsmp-plain) (find-class 'vsmp-plain))
          (not (null (dotcl-mop:validate-superclass p p)))))
  (t t))
