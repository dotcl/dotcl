;;; A class of one's own whose name is a symbol spelled like a COMMON-LISP or
;;; MOP class (CLASS, METHOD, CONDITION, ...) in another package is an ordinary
;;; class. MAKE-INSTANCE recognized the system classes by the name's string, so
;;; an instance of a library's own CLASS type (Coalton's type-inference example
;;; defines one) was taken for a class metaobject and failed in
;;; VALIDATE-SUPERCLASS.

(defpackage :ucns (:use) (:shadow #:class #:method #:condition #:generic-function
                                  #:slot-definition))

(defclass ucns::class () ((ucns::a :initarg :a :reader ucns::a)))
(defclass ucns::class/class (ucns::class) ())
(defclass ucns::method () ((ucns::a :initarg :a :reader ucns::a)))
(defclass ucns::condition () ((ucns::a :initarg :a :reader ucns::a)))
(defclass ucns::generic-function () ((ucns::a :initarg :a :reader ucns::a)))
(defclass ucns::slot-definition () ((ucns::a :initarg :a :reader ucns::a)))

(deftest user-class-named-like-system-class.make-instance
  (mapcar (lambda (c)
            (handler-case (let ((o (make-instance c :a 7)))
                            (list (eq (type-of o) c) (ucns::a o) (typep o 'condition)))
              (error (e) (princ-to-string e))))
          '(ucns::class ucns::class/class ucns::method ucns::condition
            ucns::generic-function ucns::slot-definition))
  ((t 7 nil) (t 7 nil) (t 7 nil) (t 7 nil) (t 7 nil) (t 7 nil)))

;; The system classes are still recognized.
(deftest user-class-named-like-system-class.system-classes
  (list (typep (make-instance 'standard-class :name 'ucns-made) 'class)
        (typep (make-instance 'standard-generic-function) 'generic-function))
  (t t))
