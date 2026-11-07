;;; The :ARGUMENTS lambda list of a long-form DEFINE-METHOD-COMBINATION is laid over
;;; the generic function's lambda list section by section: &rest / &key reach what
;;; follows the generic function's required and optional parameters, not whatever
;;; follows the same count of positions in the :arguments list.

(define-method-combination mca-rest ()
  ((primary () :required t))
  (:arguments arg &rest more)
  `(list :arg ,arg :more ,more
         :result (call-method ,(first primary) ,(rest primary))))

(defgeneric mca-g (x y) (:method-combination mca-rest))
(defmethod mca-g (x y) (list x y))

(defgeneric mca-g-rest (x y &rest z) (:method-combination mca-rest))
(defmethod mca-g-rest (x y &rest z) (list x y z))

(define-method-combination mca-opt ()
  ((primary () :required t))
  (:arguments a &optional b &key k)
  `(list :a ,a :b ,b :k ,k (call-method ,(first primary))))

(defgeneric mca-g-opt (x &optional y &key k) (:method-combination mca-opt))
(defmethod mca-g-opt (x &optional y &key k) (list x y k))

(deftest method-combination-arguments.rest-is-empty-for-required-only
  (multiple-value-list (mca-g 1 2))
  ((:arg 1 :more nil :result (1 2))))

(deftest method-combination-arguments.rest-follows-generic-function-rest
  (mca-g-rest 1 2 3 4)
  (:arg 1 :more (3 4) :result (1 2 (3 4))))

(deftest method-combination-arguments.optional-and-key
  (list (mca-g-opt 1 2 :k 3) (mca-g-opt 1))
  ((:a 1 :b 2 :k 3 (1 2 3)) (:a 1 :b nil :k nil (1 nil nil))))
