;;; TYPEP and type declarations naming an ordinary CLOS class: an instance of the
;;; class or of a subclass is a member, anything else is not, and a changed
;;; hierarchy or a redefined class is seen on the next test.

(defclass tsc-a () ())
(defclass tsc-b (tsc-a) ())
(defclass tsc-c () ())
(defstruct tsc-s x)

(defun tsc-decl (p) (declare (type tsc-a p)) :ok)

(deftest typep-standard-class.members
  (let (r)
    (dotimes (i 3)
      (setq r (list (typep (make-instance 'tsc-a) 'tsc-a)
                    (typep (make-instance 'tsc-b) 'tsc-a)
                    (typep (make-instance 'tsc-a) 'tsc-b)
                    (typep (make-instance 'tsc-c) 'tsc-a)
                    (typep 1 'tsc-a) (typep nil 'tsc-a) (typep "x" 'tsc-a)
                    (typep (make-tsc-s) 'tsc-a))))
    r)
  (t t nil nil nil nil nil nil))

(deftest typep-standard-class.declaration
  (let (r)
    (dotimes (i 3)
      (setq r (list (tsc-decl (make-instance 'tsc-b))
                    (handler-case (tsc-decl (make-instance 'tsc-c)) (type-error () :type-error)))))
    r)
  (:ok :type-error))

(defclass tsc-d () ())
(deftest typep-standard-class.redefined
  (let ((before (typep (make-instance 'tsc-d) 'tsc-d))
        (old (make-instance 'tsc-d)))
    (eval '(defclass tsc-d () ((x :initform 1))))
    (list before (typep (make-instance 'tsc-d) 'tsc-d) (typep old 'tsc-d)))
  (t t t))

(defclass tsc-e () ())
(defclass tsc-f () ())
(deftest typep-standard-class.change-class
  (let ((o (make-instance 'tsc-e)))
    (dotimes (i 3) (typep o 'tsc-e))
    (change-class o 'tsc-f)
    (list (typep o 'tsc-e) (typep o 'tsc-f)))
  (nil t))
