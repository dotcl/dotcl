;;; Quoted data in a method body that looks like #'CALL-NEXT-METHOD is data, and a local function named
;;; CALL-NEXT-METHOD (undefined by CLHS 11.1.2.1.2) is not a compile error. The expected value of the
;;; first is SBCL's.

(defgeneric cqf-1 (x))
(defmethod cqf-1 ((x integer)) (quote (#'call-next-method)))
(defmethod cqf-1 ((x (eql 1)))
  (list (call-next-method) (quote (function call-next-method)) '(call-next-method)))

(deftest defmethod-cnm-quote-flet.quote
  (cqf-1 1)
  ((#'call-next-method) #'call-next-method (call-next-method)))

(defgeneric cqf-2 (x))
(defmethod cqf-2 ((x integer)) (list :base x))
(defmethod cqf-2 ((x (eql 1)))
  (flet ((call-next-method () :local)) (list :flet (call-next-method))))

(deftest defmethod-cnm-quote-flet.flet-compiles
  (let ((r (cqf-2 1))) (and (consp r) (eq (car r) :flet)))
  t)

;;; A variable named CALL-NEXT-METHOD is a variable (the function and variable namespaces are separate):
;;; the binding forms below introduce one, and the method still reaches the next method through
;;; #'CALL-NEXT-METHOD. Expected values are SBCL's.
(defclass cqv-1 () ())
(defclass cqv-3 (cqv-1) ())
(defmacro cqv-def (name &body body)
  `(progn (defgeneric ,name (a &rest r))
          (defmethod ,name ((a cqv-1) &rest r) (list :base r))
          (defmethod ,name :around ((a cqv-3) &rest r) ,@body)))
(cqv-def cqv-let (let ((call-next-method 5)) (list call-next-method (apply #'call-next-method a r))))
(cqv-def cqv-let* (let* ((call-next-method 1) (x (list call-next-method))) (list x (apply #'call-next-method a r))))
(cqv-def cqv-flet-param (flet ((f (call-next-method) (list call-next-method))) (list (f 5) (apply #'call-next-method a r))))
(cqv-def cqv-mvb (multiple-value-bind (call-next-method) (values 8) (list call-next-method (apply #'call-next-method a r))))
(cqv-def cqv-dbind (destructuring-bind (call-next-method) '(7) (list call-next-method (apply #'call-next-method a r))))
(cqv-def cqv-lambda (funcall (lambda (call-next-method) (list call-next-method (apply #'call-next-method a r))) 9))
(cqv-def cqv-do (do ((call-next-method 0 (1+ call-next-method))) ((> call-next-method 1) (apply #'call-next-method a r))))

(deftest defmethod-cnm-quote-flet.variable-named-cnm
  (let ((o (make-instance 'cqv-3)))
    (list (cqv-let o 1 2) (cqv-let* o 1) (cqv-flet-param o 1) (cqv-mvb o 1) (cqv-dbind o 1) (cqv-lambda o 1)
          (cqv-do o 1)))
  ((5 (:base (1 2))) ((1) (:base (1))) ((5) (:base (1))) (8 (:base (1))) (7 (:base (1))) (9 (:base (1)))
   (:base (1))))
