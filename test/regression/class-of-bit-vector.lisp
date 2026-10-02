;;; CLASS-OF a bit vector is BIT-VECTOR (SIMPLE-BIT-VECTOR when simple), not
;;; VECTOR, so a method specialized on BIT-VECTOR is applicable to it and is
;;; more specific than one on VECTOR or SEQUENCE. The select library (used by
;;; data-frame) has exactly this pair of methods for a bit mask versus a vector
;;; of indices, and got the index method for a mask.

(defgeneric %cobv-kind (x))
(defmethod %cobv-kind ((x sequence)) :sequence)
(defmethod %cobv-kind ((x vector)) :vector)
(defmethod %cobv-kind ((x bit-vector)) :bit-vector)

(deftest class-of-bit-vector.simple
  (class-name (class-of (make-array 4 :element-type 'bit)))
  simple-bit-vector)

(deftest class-of-bit-vector.literal
  (class-name (class-of #*0101))
  simple-bit-vector)

(deftest class-of-bit-vector.adjustable
  (let ((c (class-of (make-array 4 :element-type 'bit :adjustable t))))
    (list (class-name c) (subtypep c 'bit-vector)))
  (bit-vector t))

(deftest class-of-bit-vector.dispatch
  (list (%cobv-kind (make-array 3 :element-type 'bit))
        (%cobv-kind (make-array 3 :element-type 'bit :fill-pointer 1))
        (%cobv-kind (vector 1 0 1))
        (%cobv-kind (list 1 0)))
  (:bit-vector :bit-vector :vector :sequence))

(deftest class-of-bit-vector.typep-agrees
  (let ((v (make-array 2 :element-type 'bit)))
    (typep v (class-of v)))
  t)
