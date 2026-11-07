;;; Redefining a class with DEFCLASS removes the readers and writers the new
;;; definition no longer asks for (CLHS 4.3.6; SBCL signals NO-APPLICABLE-METHOD).
;;; Accessors it still asks for keep working, and a later definition can bring one back.

(defclass drda-c () ((a :reader drda-a :writer drda-set-a :initarg :a)
                     (b :accessor drda-b :initarg :b)))

(defun %drda-error-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error (c) (if (string= (symbol-name (type-of c)) "NO-APPLICABLE-METHOD-ERROR")
                   :no-applicable-method
                   (type-of c)))))

(deftest defclass-redefine-drops-accessors.before-redefinition
  (let ((o (make-instance 'drda-c :a 1 :b 2)))
    (list (drda-a o) (drda-b o) (progn (setf (drda-b o) 5) (drda-b o))))
  (1 2 5))

(deftest defclass-redefine-drops-accessors.dropped-and-kept
  (let ((o (make-instance 'drda-c :a 1 :b 2)))
    (eval '(defclass drda-c () ((a :initarg :a) (b :reader drda-b :initarg :b))))
    (list (%drda-error-p (lambda () (drda-a o)))
          (%drda-error-p (lambda () (drda-set-a 5 o)))
          (drda-b o)
          (%drda-error-p (lambda () (setf (drda-b o) 3)))
          (length (dotcl-mop:generic-function-methods #'drda-a))))
  (:no-applicable-method :no-applicable-method 2 :no-applicable-method 0))

(deftest defclass-redefine-drops-accessors.restored-later
  (progn
    (eval '(defclass drda-c () ((a :reader drda-a :initarg :a) (b :accessor drda-b :initarg :b))))
    (let ((o (make-instance 'drda-c :a 7 :b 8)))
      (list (drda-a o) (progn (setf (drda-b o) 9) (drda-b o)))))
  (7 9))

(deftest defclass-redefine-drops-accessors.same-definition-again
  (progn
    (eval '(defclass drda-c () ((a :reader drda-a :initarg :a) (b :accessor drda-b :initarg :b))))
    (eval '(defclass drda-c () ((a :reader drda-a :initarg :a) (b :accessor drda-b :initarg :b))))
    (let ((o (make-instance 'drda-c :a 1 :b 2)))
      (list (drda-a o) (drda-b o) (length (dotcl-mop:generic-function-methods #'drda-a)))))
  (1 2 1))

(define-condition drda-e (error) ((x :reader drda-e-x :initarg :x)))

(deftest defclass-redefine-drops-accessors.condition
  (progn
    (eval '(define-condition drda-e (error) ((x :initarg :x))))
    (%drda-error-p (lambda () (drda-e-x (make-condition 'drda-e :x 1)))))
  :no-applicable-method)
