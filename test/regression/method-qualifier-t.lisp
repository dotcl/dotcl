;;; Regression: a method qualifier that is T (or a number) was dropped. CLHS
;;; DEFMETHOD allows any non-list atom as a qualifier, but the runtime kept only
;;; Symbol objects, and T is not one here, so (defmethod f t (...)) became an
;;; unqualified method. generic-cl's SUBTYPE method combination qualifies its
;;; fallback method with T and failed to load ("No method for type T").

(define-method-combination mq-subtype ()
  ((methods * :required t))
  (:arguments type)
  (labels ((type-qualifier (method) (first (method-qualifiers method)))
           (make-if (method else)
             `(if (subtypep ,type ',(type-qualifier method))
                  (call-method ,method)
                  ,else)))
    (reduce #'make-if
            (stable-sort (copy-list methods) #'subtypep :key #'type-qualifier)
            :from-end t
            :initial-value `(error "No method for type ~a" ,type))))

(defgeneric mq-dispatch (type x) (:method-combination mq-subtype))
(defmethod mq-dispatch list ((type t) x) (list :list x))
(defmethod mq-dispatch t ((type t) x) (list :t x))

(deftest method-qualifier-t-kept
  (sort (mapcar (lambda (m) (princ-to-string (method-qualifiers m)))
                (copy-list (generic-function-methods #'mq-dispatch)))
        #'string<)
  ("(LIST)" "(T)"))

(deftest method-qualifier-t-dispatch
  (list (mq-dispatch 'list 1) (mq-dispatch 't 2) (mq-dispatch 'integer 3))
  ((:list 1) (:t 2) (:t 3)))

(deftest method-qualifier-t-find-method
  (not (null (find-method #'mq-dispatch '(t) (list (find-class 't) (find-class 't)))))
  t)

(defgeneric mq-num (x) (:method-combination mq-subtype))
(defmethod mq-num 1 (x) x)

(deftest method-qualifier-number-kept
  (mapcar #'method-qualifiers (generic-function-methods #'mq-num))
  ((1)))
