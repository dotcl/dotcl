;;; (:constructor nil) together with another :constructor option is an error at DEFSTRUCT
;;; time (SBCL): the options contradict each other. Each alone is fine, as is every
;;; combination of named constructors.

(defun %dcn (form)
  (handler-case (progn (eval form) :ok) (program-error () :program-error)))

(deftest defstruct-constructor-nil-combination.contradiction
  (list (%dcn '(defstruct (dcn-a (:constructor nil) (:constructor make-dcn-ax)) x))
        (%dcn '(defstruct (dcn-b (:constructor make-dcn-bx) (:constructor nil)) x))
        (%dcn '(defstruct (dcn-c (:constructor nil) (:constructor)) x)))
  (:program-error :program-error :program-error))

(deftest defstruct-constructor-nil-combination.valid
  (list (%dcn '(defstruct (dcn-d (:constructor nil)) x))
        (%dcn '(defstruct (dcn-e (:constructor make-dcn-e1) (:constructor make-dcn-e2 (x))) x))
        (%dcn '(defstruct (dcn-f (:constructor nil) (:copier nil)) x))
        (%dcn '(defstruct dcn-g x)))
  (:ok :ok :ok :ok))

(deftest defstruct-constructor-nil-combination.valid-ones-work
  (progn (eval '(defstruct (dcn-h (:constructor make-dcn-h1) (:constructor make-dcn-h2 (x))) x))
         (list (dcn-h-x (make-dcn-h1 :x 1)) (dcn-h-x (make-dcn-h2 2))))
  (1 2))
