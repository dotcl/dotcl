;;; ENSURE-GENERIC-FUNCTION honours :GENERIC-FUNCTION-CLASS (a class name or a
;;; class) when it creates the generic function, and refuses to change the
;;; class of an existing one instead of silently keeping the old class.

(defclass egfc-gf (standard-generic-function) ()
  (:metaclass dotcl-mop:funcallable-standard-class))

(deftest ensure-gf-class.by-name
  (let ((g (ensure-generic-function 'egfc-a :lambda-list '(x)
                                    :generic-function-class 'egfc-gf)))
    (list (class-name (class-of g))
          (dotcl-mop:generic-function-name g)
          (eq g (fdefinition 'egfc-a))))
  (egfc-gf egfc-a t))

(deftest ensure-gf-class.by-class-object-dispatches
  (let ((g (ensure-generic-function 'egfc-b :lambda-list '(x &key y)
                                    :generic-function-class (find-class 'egfc-gf))))
    (defmethod egfc-b ((x integer) &key y) (list x y))
    (list (class-name (class-of g))
          (dotcl-mop:generic-function-lambda-list g)
          (egfc-b 1 :y 2)))
  (egfc-gf (x &key y) (1 2)))

(deftest ensure-gf-class.setf-name
  (let ((g (ensure-generic-function '(setf egfc-c) :lambda-list '(v x)
                                    :generic-function-class 'egfc-gf)))
    (list (class-name (class-of g))
          (dotcl-mop:generic-function-name g)))
  (egfc-gf (setf egfc-c)))

(deftest ensure-gf-class.standard-class-unchanged
  (class-name (class-of (ensure-generic-function
                         'egfc-d :lambda-list '(x)
                         :generic-function-class 'standard-generic-function)))
  standard-generic-function)

;; Same class as the existing one: accepted, the generic function is kept.
(deftest ensure-gf-class.existing-same-class
  (let ((old #'egfc-a))
    (eq old (ensure-generic-function 'egfc-a :generic-function-class 'egfc-gf)))
  t)

;; A different class for an existing generic function is an error (as in SBCL),
;; and the generic function keeps its class and methods.
(defgeneric egfc-e (x))
(defmethod egfc-e (x) (list :m x))

(deftest ensure-gf-class.existing-other-class-errors
  (list (handler-case
            (progn (ensure-generic-function 'egfc-e :generic-function-class 'egfc-gf)
                   :no-error)
          (error () :error))
        (class-name (class-of #'egfc-e))
        (egfc-e 1))
  (:error standard-generic-function (:m 1)))
