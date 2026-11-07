;;; A typed structure may include only a structure of the same representation: a vector
;;; structure cannot include a list one, nor the reverse (SBCL: error). The same kinds
;;; of structure still include each other.

(defstruct (dit-l (:type list)) a)
(defstruct (dit-v (:type vector)) a)

(defun %dit (form)
  (handler-case (progn (eval form) :ok) (error () :error)))

(deftest defstruct-include-type-mismatch.mismatch
  (list (%dit '(defstruct (dit-c1 (:type vector) (:include dit-l)) b))
        (%dit '(defstruct (dit-c2 (:type list) (:include dit-v)) b)))
  (:error :error))

(deftest defstruct-include-type-mismatch.same-kind
  (progn
    (eval '(defstruct (dit-c3 (:type list) (:include dit-l)) b))
    (eval '(defstruct (dit-c4 (:type vector) (:include dit-v)) b))
    (list (dit-c3-b (make-dit-c3 :a 1 :b 2)) (dit-c4-a (make-dit-c4 :a 1 :b 2))))
  (2 1))
