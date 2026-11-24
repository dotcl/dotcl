;;; regression: skip-under-interp
;;; MAP-INTO stores the first value of what FUNCTION returns: ROUND returns two values and only the
;;; quotient is an element. A specialized array refused the second value's carrier ("cannot store")
;;; and a general vector kept it.

(deftest map-into.round-into-fixnum-array
  (let ((a (make-array 4 :element-type '(integer 0 #.most-positive-fixnum) :initial-element 3)))
    (map-into a (lambda (x) (round (* (/ x 3) 255))) a)
    (coerce a 'list))
  (255 255 255 255))

(deftest map-into.round-into-general-vector
  (let ((a (make-array 3 :initial-element 7)))
    (map-into a (lambda (x) (round (/ x 2))) a)
    (coerce a 'list))
  (4 4 4))

(deftest map-into.round-into-list
  (let ((l (list 1 2 3)))
    (map-into l (lambda (x) (round (* x 3 1/2))) l)
    l)
  (2 3 4))

(deftest map-into.floor-from-list-into-vector
  (let ((a (make-array 3 :initial-element 0)))
    (map-into a (lambda (x) (floor x 2)) (list 5 6 7))
    (coerce a 'list))
  (2 3 3))

(deftest map-into.values-in-the-middle-of-two-sources
  (let ((a (make-array 3 :initial-element 0)))
    (map-into a (lambda (x y) (values (+ x y) :ignored)) #(1 2 3) '(10 20 30))
    (coerce a 'list))
  (11 22 33))
