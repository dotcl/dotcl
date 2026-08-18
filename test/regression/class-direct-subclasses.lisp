;;; CLASS-DIRECT-SUBCLASSES answers the same thing through either symbol.
;;;
;;; Bug: the CL-package registration was a stub returning NIL for every class,
;;; while DOTCL-MOP had the real one (a registry scan). Whether you got the
;;; truth depended on which symbol you reached -- and nothing said so. The same
;;; split in GENERIC-FUNCTION-LAMBDA-LIST returned NIL only on Linux, because
;;; there the stub happened to be registered last.

(defclass cds-root () ())
(defclass cds-left (cds-root) ())
(defclass cds-right (cds-root) ())
(defclass cds-leaf (cds-left) ())
(defclass cds-child-holder () ((s :initform 1)))

(defun %cds-names (class)
  (sort (mapcar #'class-name (class-direct-subclasses class)) #'string< :key #'string))

(deftest class-direct-subclasses.direct-children
  (%cds-names (find-class 'cds-root))
  (cds-left cds-right))

;; Only direct ones: the grandchild belongs to CDS-LEFT, not CDS-ROOT.
(deftest class-direct-subclasses.grandchild-not-included
  (%cds-names (find-class 'cds-left))
  (cds-leaf))

(deftest class-direct-subclasses.leaf-has-none
  (class-direct-subclasses (find-class 'cds-leaf))
  nil)

;; The DOTCL-MOP symbol is the same implementation, not a second one.
(deftest class-direct-subclasses.mop-symbol-agrees
  (equal (class-direct-subclasses (find-class 'cds-root))
         (dotcl-mop:class-direct-subclasses (find-class 'cds-root)))
  t)

(deftest class-direct-subclasses.not-a-class-is-an-error
  (handler-case (progn (class-direct-subclasses 42) :no-error)
    (type-error () :type-error)
    (error (e) (list :other (type-of e))))
  :type-error)

;;; METHOD-GENERIC-FUNCTION / METHOD-LAMBDA-LIST had the same split: the CL-side
;;; registration returned NIL for every method ("not tracked in dotcl", which
;;; was false -- ADD-METHOD has always set LispMethod.Owner), while DOTCL-MOP
;;; returned the real thing.

(defgeneric cds-gf (x y))
(defmethod cds-gf ((x cds-root) y) (list x y))

(defun %cds-method ()
  (first (dotcl-mop:generic-function-methods #'cds-gf)))

(deftest class-direct-subclasses.method-generic-function
  (eq (method-generic-function (%cds-method)) #'cds-gf)
  t)

(deftest class-direct-subclasses.method-generic-function-agrees
  (eq (method-generic-function (%cds-method))
      (dotcl-mop:method-generic-function (%cds-method)))
  t)

(deftest class-direct-subclasses.method-lambda-list-arity
  (length (method-lambda-list (%cds-method)))
  2)

(deftest class-direct-subclasses.method-introspection-type-errors
  (list (handler-case (progn (method-generic-function 42) :no-error)
          (type-error () :type-error) (error () :other))
        (handler-case (progn (method-lambda-list 42) :no-error)
          (type-error () :type-error) (error () :other)))
  (:type-error :type-error))

;;; The rest of the class-side MOP accessors, same treatment: one implementation
;;; behind both symbols, and a non-class argument is a TYPE-ERROR rather than a
;;; quiet NIL.

(defun %cds-both (fn-plain fn-mop arg)
  (list (handler-case (funcall fn-plain arg) (error (e) (list :err (type-of e))))
        (handler-case (funcall fn-mop arg) (error (e) (list :err (type-of e))))))

(deftest class-direct-subclasses.class-accessors-reject-non-class
  (mapcar (lambda (pair)
            (destructuring-bind (plain mop) pair
              (let ((r (%cds-both plain mop 42)))
                (and (equal (first r) (second r))
                     (eq (first (first r)) :err)
                     t))))
          (list (list #'class-direct-slots #'dotcl-mop:class-direct-slots)
                (list #'class-slots #'dotcl-mop:class-slots)
                (list #'class-direct-superclasses #'dotcl-mop:class-direct-superclasses)
                (list #'class-precedence-list #'dotcl-mop:class-precedence-list)
                (list #'class-finalized-p #'dotcl-mop:class-finalized-p)
                (list #'class-prototype #'dotcl-mop:class-prototype)))
  (t t t t t t))

(deftest class-direct-subclasses.class-accessors-agree
  (let ((c (find-class 'cds-child-holder)))
    (list (equal (mapcar #'class-name (class-precedence-list c))
                 (mapcar #'class-name (dotcl-mop:class-precedence-list c)))
          (eq (class-prototype c) (dotcl-mop:class-prototype c))
          (eq (class-finalized-p c) (dotcl-mop:class-finalized-p c))
          (= (length (class-slots c)) (length (dotcl-mop:class-slots c)))))
  (t t t t))
