;;; DEFMETHOD adds its method through the generic function ADD-METHOD (AMOP), so
;;; a method of the program on ADD-METHOD, specialised on the class of the
;;; generic function, runs for it. Other generic functions add their methods as
;;; before.

(defpackage :amo (:use :cl :dotcl-mop))
(in-package :amo)

(defvar *log* nil)
(defclass logging-gf (standard-generic-function) () (:metaclass funcallable-standard-class))
(defmethod add-method ((gf logging-gf) method)
  (push (list :add (mapcar #'class-name (method-specializers method))) *log*)
  (call-next-method))

(defgeneric lg (x) (:generic-function-class logging-gf))
(defgeneric plain-gf (x))

(defun run (thunk)
  (setq *log* nil)
  (funcall thunk)
  (reverse *log*))
(in-package :cl-user)

(deftest add-method-override.defmethod-runs-it
  (amo::run (lambda ()
              (eval '(defmethod amo::lg ((x integer)) (list :int x)))
              (eval '(defmethod amo::lg ((x string)) (list :str x)))))
  ((:add (integer)) (:add (string))))

(deftest add-method-override.method-works
  (list (amo::lg 1) (amo::lg "s"))
  ((:int 1) (:str "s")))

(deftest add-method-override.other-generic-functions
  (amo::run (lambda () (eval '(defmethod amo::plain-gf ((x integer)) x))))
  nil)

(deftest add-method-override.redefinition-replaces
  (progn
    (amo::run (lambda () (eval '(defmethod amo::lg ((x integer)) (list :int2 x)))))
    (list (amo::lg 1)
          (length (remove-if-not (lambda (m) (equal (mapcar #'class-name (dotcl-mop:method-specializers m)) '(integer)))
                                 (dotcl-mop:generic-function-methods #'amo::lg)))))
  ((:int2 1) 1))

(deftest-compiled-only add-method-override.compiled-defmethod
  (progn
    (amo::run (lambda ()))
    (let ((log (amo::run (lambda ()
                           (funcall (compile nil '(lambda ()
                                                    (defmethod amo::lg ((x symbol)) (list :sym x)))))))))
      (list log (amo::lg 'a))))
  (((:add (symbol))) (:sym a)))
