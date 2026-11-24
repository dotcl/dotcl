;;; CALL-NEXT-METHOD / NEXT-METHOD-P in lambdas that run on the spot and in lambdas
;;; that outlive the call.

(defvar *%cc-log* nil)
(defgeneric %cc-a (x))
(defmethod %cc-a (x) (list :base x))
(defmethod %cc-a ((x integer)) (list :int (call-next-method)))
(defmethod %cc-a :around ((x integer))
  (multiple-value-call (lambda (&optional v) (list :mv v (call-next-method))) (values 1)))

(defgeneric %cc-b (x))
(defmethod %cc-b (x) (list :base x))
(defmethod %cc-b ((x integer))
  (funcall (lambda (y) (list :funcall y (call-next-method (+ x 100)))) 5))

(defgeneric %cc-c (x))
(defmethod %cc-c (x) (list :base x))
(defmethod %cc-c ((x integer))
  ((lambda (y) (list :direct y (next-method-p) (call-next-method))) 9))

;; the closure escapes: it is called after the method returned
(defgeneric %cc-d (x))
(defmethod %cc-d (x) (list :base x))
(defmethod %cc-d ((x integer)) (lambda () (call-next-method)))

;; a nested dispatch of another generic function before the call
(defgeneric %cc-e (x))
(defmethod %cc-e (x) (list :base-e x))
(defmethod %cc-e ((x integer))
  (multiple-value-call
      (lambda (&optional inner) (list :e inner (call-next-method) (next-method-p)))
    (%cc-a x)))

;; changed arguments, and NEXT-METHOD-P with no next method
(defgeneric %cc-f (x))
(defmethod %cc-f ((x integer)) (list :int x (next-method-p)))
(defmethod %cc-f ((x number)) (list :num x (next-method-p) (call-next-method 77)))
(defmethod %cc-f ((x t)) (list :t x))
(defmethod %cc-f :around ((x number))
  (funcall (lambda () (list :around (call-next-method) (call-next-method 3)))))

(defgeneric %cc-g (x))
(defmethod %cc-g ((x integer)) (funcall (lambda () (call-next-method))))

(deftest clos-cnm-capture.mv-call-lambda
  (%cc-a 3)
  (:mv 1 (:int (:base 3))))

(deftest clos-cnm-capture.funcall-lambda-with-args
  (%cc-b 3)
  (:funcall 5 (:base 103)))

(deftest clos-cnm-capture.operator-position-lambda
  (%cc-c 4)
  (:direct 9 t (:base 4)))

(deftest clos-cnm-capture.escaping-closure
  (funcall (%cc-d 6))
  (:base 6))

(deftest clos-cnm-capture.nested-dispatch-before-call
  (%cc-e 2)
  (:e (:mv 1 (:int (:base 2))) (:base-e 2) t))

(deftest clos-cnm-capture.around-changed-arguments
  (%cc-f 5)
  (:around (:int 5 t) (:int 3 t)))

(deftest clos-cnm-capture.no-next-method-signals
  (handler-case (%cc-g 1) (error () :error))
  :error)
