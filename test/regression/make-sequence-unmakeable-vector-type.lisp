;;; MAKE-SEQUENCE of a vector type the vector it builds is not of: a type error.

(deftest make-sequence-unmakeable.not-simple
  (list (handler-case (make-sequence '(and vector (not simple-array)) 3) (type-error () :type-error))
        (handler-case (make-sequence '(and vector (not simple-array)) 0) (type-error () :type-error)))
  (:type-error :type-error))

(deftest make-sequence-unmakeable.unchanged
  (list (equalp (make-sequence 'vector 2) #(nil nil)) (equalp (make-sequence '(vector t 2) 2) #(nil nil))
        (make-sequence 'list 2 :initial-element 1))
  (t t (1 1)))

;; The class of an adjustable vector with a fill pointer is a vector class the
;; simple vector made here stands for (ansi-test MAKE-SEQUENCE.51 and .56).
(deftest make-sequence-unmakeable.class-of-vector
  (let* ((vec (make-array 1 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 1 :initial-element 1))
         (vec2 (make-sequence (class-of vec) 1 :initial-element 1)))
    (list (equalp vec vec2) (vectorp vec2)))
  (t t))
