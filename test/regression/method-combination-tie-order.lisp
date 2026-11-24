;;; Methods of equal specificity that differ only in their qualifiers reach a
;;; user method combination in definition order (deliberate; docs/deviations.md).

(define-method-combination mc-tie-order ()
    ((methods * :required t))
  `(quote ,(mapcar (lambda (m) (first (method-qualifiers m))) methods)))

(defgeneric mc-tie-order-gf (x)
  (:method-combination mc-tie-order))
(defmethod mc-tie-order-gf first (x) x)
(defmethod mc-tie-order-gf second (x) x)
(defmethod mc-tie-order-gf third (x) x)

(deftest method-combination-tie-order.definition-order
  (mc-tie-order-gf 1)
  (first second third))

(defgeneric mc-tie-order-gf2 (x)
  (:method-combination mc-tie-order))
(defmethod mc-tie-order-gf2 third ((x number)) x)
(defmethod mc-tie-order-gf2 first (x) x)
(defmethod mc-tie-order-gf2 second ((x integer)) x)

(deftest method-combination-tie-order.specificity-first
  (mc-tie-order-gf2 1)
  (second third first))
