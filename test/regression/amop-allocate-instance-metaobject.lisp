;;; ALLOCATE-INSTANCE on a metaobject class returns a metaobject.
;;;
;;; ALLOCATE-INSTANCE dispatches on (class-of cls), which is STANDARD-CLASS for every
;;; user-defined method or generic function class, so a method specialized on
;;; STANDARD-METHOD is never applicable and the default allocator ran. It handed back a
;;; plain instance: CLASS-OF answered the right class, but the object was not a method,
;;; so ADD-METHOD rejected it and the INITIALIZE-INSTANCE primary for METHOD passed it
;;; straight through without filling any slot. MAKE-INSTANCE already allocated the right
;;; C# object; ALLOCATE-INSTANCE now shares that allocator.

(defclass aaim-method (standard-method)
  ((tag :initarg :tag :accessor aaim-tag :initform :untagged)))

(defstruct aaim-point x y)

(defclass aaim-plain () ((a :initarg :a :initform 1)))

;;; The allocated object is a method, not just something whose CLASS-OF says so.

(deftest amop-allocate-instance.method-p
  (typep (allocate-instance (find-class 'aaim-method)) 'method)
  t)

(deftest amop-allocate-instance.class-of
  (class-name (class-of (allocate-instance (find-class 'aaim-method))))
  aaim-method)

(deftest amop-allocate-instance.standard-method
  (class-name (class-of (allocate-instance (find-class 'standard-method))))
  standard-method)

;;; Being a method is what lets INITIALIZE-INSTANCE fill the slots the class adds.

(deftest amop-allocate-instance.initialize-fills-slots
  (let ((m (allocate-instance (find-class 'aaim-method))))
    (initialize-instance m :tag :filled)
    (aaim-tag m))
  :filled)

;;; ALLOCATE-INSTANCE must not run initforms; INITIALIZE-INSTANCE does that.

(deftest amop-allocate-instance.no-initform-before-initialize
  (slot-boundp (allocate-instance (find-class 'aaim-plain)) 'a)
  nil)

;;; A generic function class allocates a funcallable object.

(deftest amop-allocate-instance.generic-function
  (let ((g (allocate-instance (find-class 'standard-generic-function))))
    (list (class-name (class-of g)) (functionp g)))
  (standard-generic-function t))

;;; An allocated method can be initialized and added to a generic function.

(defgeneric aaim-echo (x))

(deftest amop-allocate-instance.add-method
  (let ((m (allocate-instance (find-class 'standard-method))))
    (initialize-instance m :qualifiers '() :lambda-list '(x)
                           :specializers (list (find-class 't))
                           :function (lambda (args next)
                                       (declare (ignore next))
                                       (list :ok (car args))))
    (add-method #'aaim-echo m)
    (aaim-echo 42))
  (:ok 42))

;;; The structure-class and ordinary-class paths are untouched.

(deftest amop-allocate-instance.structure-class
  (let ((s (allocate-instance (find-class 'aaim-point))))
    (list (aaim-point-p s) (equalp s (allocate-instance (find-class 'aaim-point)))))
  (t t))

(deftest amop-allocate-instance.ordinary-class
  (class-name (class-of (allocate-instance (find-class 'aaim-plain))))
  aaim-plain)
