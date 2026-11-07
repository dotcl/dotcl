;;; Argument checks (SBCL signals TYPE-ERROR): the result array of BIT-AND and its relatives
;;; has the dimensions of the arguments; VECTOR-PUSH-EXTEND's extension is a positive
;;; integer; :DISPLACED-INDEX-OFFSET is a non-negative integer; RANDOM's second argument
;;; is a random state. The valid forms are unchanged.

(defun %arc (thunk) (handler-case (funcall thunk) (type-error () :type-error)))

(deftest array-random-argument-checks.bit-op-result-dimensions
  (list (%arc (lambda () (bit-and #*101 #*100 (make-array 2 :element-type 'bit))))
        (%arc (lambda () (bit-not #*101 (make-array 4 :element-type 'bit))))
        (bit-and #*101 #*100 (make-array 3 :element-type 'bit))
        (bit-and #*101 #*100 t)
        (bit-not #*101 (make-array 3 :element-type 'bit))
        (bit-ior #*101 #*010))
  (:type-error :type-error #*100 #*100 #*010 #*111))

(deftest array-random-argument-checks.vector-push-extend-extension
  (flet ((try (ext)
           (let ((v (make-array 1 :adjustable t :fill-pointer 0)))
             (%arc (lambda () (if ext (vector-push-extend 1 v ext) (vector-push-extend 1 v)) (length v))))))
    (list (try 0) (try -1) (try 1.5) (try nil) (try 3) (try nil)))
  (:type-error :type-error :type-error 1 1 1))

(deftest array-random-argument-checks.displaced-index-offset
  (let ((base (make-array 6 :initial-element 0)))
    (list (%arc (lambda () (make-array 4 :displaced-to base :displaced-index-offset 1.5)))
          (%arc (lambda () (make-array 4 :displaced-to base :displaced-index-offset -1)))
          (length (make-array 4 :displaced-to base :displaced-index-offset 1))
          (length (make-array 4 :displaced-to base :displaced-index-offset 2))))
  (:type-error :type-error 4 4))

(deftest array-random-argument-checks.random-state
  (list (%arc (lambda () (random 10 "state")))
        (%arc (lambda () (random 10 nil)))
        (< -1 (random 10 (make-random-state)) 10)
        (< -1 (random 10) 10)
        (< -1 (random 10 *random-state*) 10)
        (< -1d0 (random 1d0 (make-random-state t)) 1d0))
  (:type-error :type-error t t t t))
