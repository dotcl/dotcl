;;; DEFGENERIC's :METHOD-CLASS option (and ENSURE-GENERIC-FUNCTION's) names the class
;;; DEFMETHOD makes the generic function's methods; it used to be ignored. Methods of
;;; a function without the option are still standard methods.

(defclass dmc-method (standard-method) ())

(defgeneric dmc-g (x) (:method-class dmc-method))
(defmethod dmc-g ((x integer)) :int)
(defgeneric dmc-plain (x))
(defmethod dmc-plain ((x integer)) :int)

(deftest defgeneric-method-class.option-applies
  (let ((m (find-method #'dmc-g '() (list (find-class 'integer)))))
    (list (typep m 'dmc-method) (class-name (class-of m)) (dmc-g 1)))
  (t dmc-method :int))

(deftest defgeneric-method-class.later-methods-too
  (progn
    (eval '(defmethod dmc-g ((x string)) :str))
    (list (typep (find-method #'dmc-g '() (list (find-class 'string))) 'dmc-method)
          (dmc-g "a")))
  (t :str))

(deftest defgeneric-method-class.no-option-is-standard
  (let ((m (find-method #'dmc-plain '() (list (find-class 'integer)))))
    (list (typep m 'dmc-method) (class-name (class-of m)) (dmc-plain 1)))
  (nil standard-method :int))

(deftest defgeneric-method-class.ensure-generic-function
  (progn
    (ensure-generic-function 'dmc-e :lambda-list '(x) :method-class (find-class 'dmc-method))
    (eval '(defmethod dmc-e ((x integer)) :e))
    (list (typep (find-method #'dmc-e '() (list (find-class 'integer))) 'dmc-method)
          (dmc-e 1)))
  (t :e))
