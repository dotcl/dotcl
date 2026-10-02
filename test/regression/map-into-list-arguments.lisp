;;; MAP-INTO walks list arguments and a list result with a cursor. It used to
;;; index them with NTHCDR / ELT, which made MAP-INTO quadratic in the length
;;; of a list (a 200000-element list did not finish in minutes).

(deftest map-into-mixed-sequences
  (list (map-into (list 1 2 3) #'+ '(10 20 30 40) #(1 1))
        (coerce (map-into (make-array 3) #'1+ '(1 2 3 4)) 'list)
        (map-into (list 0 0 0 0) (let ((i 0)) (lambda () (incf i))))
        (let ((v (make-array 5 :fill-pointer 1 :initial-element 0)))
          (list (coerce (map-into v #'identity "abc") 'list) (fill-pointer v)))
        (map-into (make-string 3) #'char-upcase "abcdef")
        (coerce (map-into (vector 1 2 3) #'+ (vector 10 20) '(1 2 3)) 'list)
        (map-into (list 1 2) 'list)
        (map-into nil #'identity '(1)))
  ((11 21 3) (2 3 4) (1 2 3 4) ((#\a #\b #\c) 3) "ABC" (11 22 3) (nil nil) nil))

(deftest map-into-long-lists
  (let* ((l (make-list 200000 :initial-element 1))
         (v (make-array 200000)))
    (map-into v #'1+ l)
    (map-into l #'1+ v)
    (reduce #'+ l))
  600000)
