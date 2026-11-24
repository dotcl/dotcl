;;; DEFCLASS with a malformed name, superclass list, slot specification or class
;;; option signals a PROGRAM-ERROR; the valid shapes still work.

(defmacro %dcs-pe (form)
  `(handler-case (progn (eval ',form) :no-error)
     (program-error () :program-error)
     (error () :other-error)))

(deftest defclass-shape.invalid
  (list (%dcs-pe (defclass %dcs-a1))
        (%dcs-pe (defclass %dcs-a2 ()))
        (%dcs-pe (defclass %dcs-a3 (1) ()))
        (%dcs-pe (defclass %dcs-a4 () (1)))
        (%dcs-pe (defclass %dcs-a5 () ((s :initarg))))
        (%dcs-pe (defclass %dcs-a6 () ((s :type))))
        (%dcs-pe (defclass %dcs-a7 () ((s :documentation 1))))
        (%dcs-pe (defclass %dcs-a8 () ((s . 1))))
        (%dcs-pe (defclass %dcs-a9 () () (:metaclass)))
        (%dcs-pe (defclass %dcs-a10 () () (:metaclass 1)))
        (%dcs-pe (defclass %dcs-a11 () () (:metaclass standard-class) (:metaclass standard-class)))
        (%dcs-pe (defclass %dcs-a12 () () (:default-initargs :a)))
        (%dcs-pe (defclass %dcs-a13 () () (:documentation)))
        (%dcs-pe (defclass %dcs-a14 () () (:documentation 1)))
        (%dcs-pe (defclass %dcs-a15 () () nil))
        (%dcs-pe (defclass %dcs-a16 () () 1)))
  (:program-error :program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error :program-error :program-error
   :program-error :program-error :program-error :program-error))

(deftest defclass-shape.valid
  (list (class-name (eval '(defclass %dcs-b1 () ())))
        (class-name (eval '(defclass %dcs-b2 (%dcs-b1) ((s :initarg :s :documentation "d")) (:documentation "x"))))
        (class-name (eval '(defclass %dcs-b3 () () (:default-initargs :a 1) (:metaclass standard-class)))))
  (%dcs-b1 %dcs-b2 %dcs-b3))
