;;; DEFGENERIC and ENSURE-GENERIC-FUNCTION go through the generic function
;;; ENSURE-GENERIC-FUNCTION-USING-CLASS (AMOP): a method of the program on it
;;; runs for a function that is made and for one that is redefined, and
;;; CALL-NEXT-METHOD reaches the default behaviour.

(defvar *egf-log* nil)

(defmacro with-egf-method ((qualifier spec &body body) &body forms)
  `(let ((m (eval '(defmethod dotcl-mop:ensure-generic-function-using-class
                       ,qualifier (,spec name &rest args &key &allow-other-keys)
                     (declare (ignorable name args))
                     ,@body))))
     (unwind-protect (progn ,@forms)
       (remove-method #'dotcl-mop:ensure-generic-function-using-class m))))

(deftest mop-egf-using-class.defgeneric-new-and-redefined
  (progn
    (setf *egf-log* nil)
    (with-egf-method (:before (gf null) (push (list :new name) *egf-log*))
      (with-egf-method (:before (gf standard-generic-function)
                               (push (list :old name) *egf-log*))
        (eval '(defgeneric egfuc-a (x)))
        (eval '(defgeneric egfuc-a (x) (:documentation "again")))))
    (reverse *egf-log*))
  ((:new egfuc-a) (:old egfuc-a)))

(deftest mop-egf-using-class.ensure-generic-function
  (progn
    (setf *egf-log* nil)
    (with-egf-method (:before (gf null) (push (list :new name) *egf-log*))
      (ensure-generic-function 'egfuc-b :lambda-list '(x)))
    (list (reverse *egf-log*) (typep (fdefinition 'egfuc-b) 'generic-function)))
  (((:new egfuc-b)) t))

(deftest mop-egf-using-class.around-sees-the-function
  (with-egf-method (:around (gf null) (let ((r (call-next-method))) (push (type-of r) *egf-log*) r))
    (setf *egf-log* nil)
    (eval '(defgeneric egfuc-c (x)))
    (eval '(defmethod egfuc-c ((x integer)) :int))
    (list (reverse *egf-log*) (egfuc-c 1)))
  ((standard-generic-function) :int))

(deftest mop-egf-using-class.no-method-no-call
  (progn
    (setf *egf-log* nil)
    (eval '(defgeneric egfuc-d (x)))
    (eval '(defmethod egfuc-d ((x integer)) :int))
    (list *egf-log* (egfuc-d 1)))
  (nil :int))

(deftest mop-egf-using-class.specialised-elsewhere-is-not-called
  (progn
    (setf *egf-log* nil)
    (with-egf-method (:before (gf (eql 5)) (push name *egf-log*))
      (eval '(defgeneric egfuc-e (x))))
    *egf-log*)
  nil)
