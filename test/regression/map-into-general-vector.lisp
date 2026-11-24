;;; MAP-INTO of a general vector into a general vector stores into the element array directly.

(deftest map-into-general-vector.basic
  (let* ((d (make-array 5 :initial-element 0)) (s (vector 1 2 3 4 5)) (r (map-into d #'1+ s)))
    (list (eq r d) (coerce d 'list) (coerce s 'list)))
  (t (2 3 4 5 6) (1 2 3 4 5)))

(deftest map-into-general-vector.lengths
  (list (coerce (map-into (make-array 3 :initial-element 0) #'1+ (vector 1 2 3 4 5)) 'list)
        (coerce (map-into (make-array 5 :initial-element 0) #'1+ (vector 1 2)) 'list)
        (coerce (map-into (vector) #'1+ (vector 1 2)) 'list))
  ((2 3 4) (2 3 0 0 0) nil))

(deftest map-into-general-vector.same-vector-and-fill-pointer
  (let ((v (vector 1 2 3)) (f (make-array 5 :initial-element 0 :fill-pointer 2)))
    (map-into v #'* v v)
    (map-into f #'1+ (vector 10 20 30 40))
    (list (coerce v 'list) (length f) (coerce f 'list)))
  ((1 4 9) 4 (11 21 31 41)))

(deftest map-into-general-vector.other-shapes
  (list (coerce (map-into (make-array 3 :element-type 'fixnum :initial-element 0) #'1+ (vector 1 2 3)) 'list)
        (coerce (map-into (make-array 3 :initial-element 0) #'1+ (make-array 3 :element-type 'fixnum :initial-contents '(7 8 9))) 'list)
        (map-into (list 0 0 0) #'1+ (vector 1 2 3))
        (coerce (map-into (make-array 3 :initial-element 0) #'char-code "abc") 'list))
  ((2 3 4) (8 9 10) (2 3 4) (97 98 99)))

(deftest map-into-general-vector.error-in-function
  (let ((d (make-array 3 :initial-element 0)))
    (list (handler-case (map-into d (lambda (x) (if (= x 2) (error "boom") x)) (vector 1 2 3)) (error () :error))
          (coerce d 'list)))
  (:error (1 0 0)))
