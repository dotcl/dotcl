;;; A method of the program on MAKE-INSTANCE, specialised on a metaclass, runs
;;; for MAKE-INSTANCE of a class of that metaclass whichever way the class is
;;; given: the class object, its name through FUNCALL or a call with the name
;;; quoted. AMOP has the method on SYMBOL call MAKE-INSTANCE on the class. A
;;; class without such a method keeps the direct path.

(defpackage :mio (:use :cl :dotcl-mop))
(in-package :mio)

(defvar *log* nil)
(defclass meta (standard-class) ())
(defmethod validate-superclass ((c meta) (s standard-class)) t)
(defclass thing () ((a :initarg :a :initform 1 :reader thing-a)) (:metaclass meta))
(defclass plain () ((a :initarg :a :initform 1 :reader plain-a)))
(defclass eql-target () ((a :initarg :a :initform 1)))

(defmethod make-instance ((c meta) &rest initargs)
  (push (list (class-name c) initargs) *log*)
  (apply #'call-next-method c :a 7 initargs))

(defmethod make-instance ((c (eql (find-class 'eql-target))) &rest initargs)
  (push (list :eql initargs) *log*)
  (call-next-method))

(defun logged (thunk)
  (setq *log* nil)
  (let ((r (funcall thunk)))
    (list r (reverse *log*))))
(in-package :cl-user)

(deftest make-instance-override.class-object
  (mio::logged (lambda () (mio::thing-a (make-instance (find-class 'mio::thing) :a 2))))
  (7 ((mio::thing (:a 2)))))

(deftest make-instance-override.quoted-name
  (mio::logged (lambda () (mio::thing-a (make-instance 'mio::thing))))
  (7 ((mio::thing nil))))

(deftest make-instance-override.funcall-name
  (mio::logged (lambda () (mio::thing-a (funcall 'make-instance 'mio::thing :a 3))))
  (7 ((mio::thing (:a 3)))))

(deftest make-instance-override.name-in-variable
  (let ((name 'mio::thing))
    (mio::logged (lambda () (mio::thing-a (make-instance name)))))
  (7 ((mio::thing nil))))

(deftest make-instance-override.plain-class-untouched
  (mio::logged (lambda () (mio::plain-a (make-instance 'mio::plain :a 5))))
  (5 nil))

(deftest make-instance-override.eql-specializer
  (mio::logged (lambda () (typep (make-instance 'mio::eql-target) 'mio::eql-target)))
  (t ((:eql nil))))

(deftest make-instance-override.plain-class-after-warmup
  (progn
    (dotimes (i 5) (make-instance 'mio::plain :a i))
    (mio::logged (lambda () (mio::plain-a (make-instance 'mio::plain :a 9)))))
  (9 nil))
