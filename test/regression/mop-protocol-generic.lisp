;;; AMOP's introspection functions (class-slots, slot-definition-name, method-specializers, ...) are
;;; generic functions: a method on a metaclass adds to them instead of replacing the function, and
;;; the default behaviour is unchanged.

(defclass mpg-meta (standard-class) ())
(defmethod dotcl-mop:validate-superclass ((c mpg-meta) (s standard-class)) t)
(defclass mpg-plain () ((a :initarg :a) (b)))
(defclass mpg-special () ((c)) (:metaclass mpg-meta))

(defmethod dotcl-mop:class-direct-subclasses ((c mpg-meta)) (list :custom))

(defgeneric mpg-g (x))
(defmethod mpg-g ((x integer)) x)

(deftest mop-protocol-generic.are-generic-functions
  (mapcar (lambda (f) (typep (fdefinition f) 'generic-function))
          '(dotcl-mop:class-slots dotcl-mop:class-direct-slots dotcl-mop:class-direct-superclasses
            dotcl-mop:class-precedence-list dotcl-mop:slot-definition-name dotcl-mop:slot-definition-type
            dotcl-mop:method-specializers dotcl-mop:generic-function-methods dotcl-mop:generic-function-name))
  (t t t t t t t t t))

(deftest mop-protocol-generic.default-behaviour
  (let ((slots (dotcl-mop:class-slots (find-class 'mpg-plain)))
        (m (first (dotcl-mop:generic-function-methods #'mpg-g))))
    (mpg-g 1)
    (list (mapcar #'dotcl-mop:slot-definition-name slots)
          (mapcar #'class-name (dotcl-mop:method-specializers m))
          (dotcl-mop:generic-function-name #'mpg-g)
          (mapcar #'class-name (dotcl-mop:class-direct-superclasses (find-class 'mpg-plain)))))
  ((a b) (integer) mpg-g (standard-object)))

(deftest mop-protocol-generic.metaclass-method-applies
  (list (dotcl-mop:class-direct-subclasses (find-class 'mpg-special))
        (listp (dotcl-mop:class-direct-subclasses (find-class 'mpg-plain)))
        (length (dotcl-mop:generic-function-methods #'dotcl-mop:class-direct-subclasses)))
  ((:custom) t 2))
