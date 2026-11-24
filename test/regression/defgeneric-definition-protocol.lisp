;;; DEFGENERIC redefinition keeps the methods DEFMETHOD made, an applicable method with an
;;; invalid qualifier is an error, and the STANDARD method combination takes no options.

(defclass dgdp-c () ())

(deftest defgeneric-definition-protocol.redefinition-with-class-keeps-methods
  (progn
    (eval '(defgeneric dgdp-g (x)))
    (eval '(defmethod dgdp-g ((x dgdp-c)) :c))
    (eval '(defgeneric dgdp-g (x) (:generic-function-class standard-generic-function)))
    (handler-case (dgdp-g (make-instance 'dgdp-c)) (error () :lost)))
  :c)

(deftest defgeneric-definition-protocol.redefinition-without-class-keeps-methods
  (progn
    (eval '(defgeneric dgdp-h (x)))
    (eval '(defmethod dgdp-h ((x dgdp-c)) :c))
    (eval '(defgeneric dgdp-h (x)))
    (handler-case (dgdp-h (make-instance 'dgdp-c)) (error () :lost)))
  :c)

(deftest defgeneric-definition-protocol.invalid-qualifier-signals
  (progn
    (eval '(defgeneric dgdp-q (x)))
    (eval '(defmethod dgdp-q ((x integer)) :ok))
    (eval '(defmethod dgdp-q bogus ((x integer)) :bogus))
    (list (handler-case (dgdp-q 1) (error () :error))
          (handler-case (dgdp-q "a") (error () :no-applicable-method))))
  (:error :no-applicable-method))

(deftest defgeneric-definition-protocol.valid-qualifiers-unchanged
  (progn
    (eval '(defgeneric dgdp-v (x)))
    (eval '(defmethod dgdp-v ((x integer)) (list :primary)))
    (eval '(defmethod dgdp-v :around ((x integer)) (cons :around (call-next-method))))
    (eval '(defmethod dgdp-v :before ((x integer)) nil))
    (eval '(defmethod dgdp-v :after ((x integer)) nil))
    (dgdp-v 1))
  (:around :primary))

(deftest defgeneric-definition-protocol.standard-combination-takes-no-options
  (list (handler-case (progn (eval '(defgeneric dgdp-m (x) (:method-combination standard :most-specific-last)))
                             :accepted)
          (error () :rejected))
        (handler-case (progn (eval '(defgeneric dgdp-n (x) (:method-combination standard)))
                             :accepted)
          (error () :rejected))
        (handler-case (progn (eval '(defgeneric dgdp-o (x) (:method-combination +) (:method + ((x integer)) x)))
                             :accepted)
          (error () :rejected)))
  (:rejected :accepted :accepted))
