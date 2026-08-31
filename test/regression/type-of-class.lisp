;;; (type-of <class object>) is the metaclass name.
;;;
;;; TYPE-OF had no branch for a class metaobject, so every class answered T.
;;; Not false -- everything is of type T -- but the least useful true answer,
;;; and it made TYPE-OF disagree with CLASS-OF about the same object. CLASS-OF
;;; already works the metaclass out, including :metaclass and the built-in and
;;; structure cases, so TYPE-OF asks it.

(defclass toc-plain () ())

(defclass toc-meta (standard-class) ())

(defmethod dotcl-mop:validate-superclass ((class toc-meta) (super standard-class))
  t)

(defclass toc-with-meta () () (:metaclass toc-meta))

(defstruct toc-point x y)

(deftest type-of-class-standard-object
  (type-of (find-class 'standard-object))
  standard-class)

(deftest type-of-class-user-class
  (type-of (find-class 'toc-plain))
  standard-class)

(deftest type-of-class-custom-metaclass
  (type-of (find-class 'toc-with-meta))
  toc-meta)

(deftest type-of-class-built-in
  (type-of (find-class 'integer))
  built-in-class)

(deftest type-of-class-structure
  (type-of (find-class 'toc-point))
  structure-class)

;;; A metaclass is itself an instance of STANDARD-CLASS.
(deftest type-of-class-the-metaclass
  (type-of (find-class 'standard-class))
  standard-class)

;;; CLHS 4.4: TYPE-OF answers a type specifier the object is of.
(deftest type-of-class-satisfies-typep
  (every (lambda (class) (typep class (type-of class)))
         (list (find-class 'standard-object)
               (find-class 'toc-plain)
               (find-class 'toc-with-meta)
               (find-class 'integer)
               (find-class 'toc-point)
               (find-class 'standard-class)))
  t)

;;; TYPE-OF and CLASS-OF now name the same thing.
(deftest type-of-class-agrees-with-class-of
  (every (lambda (class)
           (eq (find-class (type-of class)) (class-of class)))
         (list (find-class 'toc-plain)
               (find-class 'integer)
               (find-class 'toc-point)))
  t)

;;; Instances are unaffected: the class, not the metaclass.
(deftest type-of-instance-unchanged
  (type-of (make-instance 'toc-plain))
  toc-plain)
