;;; A specialized vector class sorts ARRAY before SEQUENCE, so a method on ARRAY is
;;; more specific than one on SEQUENCE for a simple vector (and a bit vector).

(defgeneric vcp-kind (x))
(defmethod vcp-kind ((x sequence)) :sequence)
(defmethod vcp-kind ((x array)) :array)

(deftest vector-class-precedence.dispatch
  (list (vcp-kind #(1 2)) (vcp-kind (make-array 2 :element-type 'bit))
        (vcp-kind (make-array 2 :adjustable t)) (vcp-kind '(1 2)))
  (:array :array :array :sequence))

(deftest vector-class-precedence.cpl
  (let ((names (mapcar #'class-name (class-precedence-list (find-class 'simple-vector)))))
    (and (< (position 'vector names) (position 'simple-array names))
         (< (position 'array names) (position 'sequence names))
         (eq (car (last names)) t)))
  t)
