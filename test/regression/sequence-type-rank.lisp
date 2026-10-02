;;; In an ARRAY or SIMPLE-ARRAY type specifier the dimension spec may be an
;;; integer, and then it is the RANK: (simple-array (unsigned-byte 4) 1) is
;;; any one-dimensional array of that element type. CONCATENATE, COERCE and
;;; MAKE-SEQUENCE read that integer as a length, so 3bz's
;;;   (deftype code-table-type () '(simple-array (unsigned-byte 4) 1))
;;;   (concatenate 'code-table-type (make-array 144 ...) ...)
;;; failed with "result has 288 elements, type requires 1".

(deftype seq-rank-code-table () '(simple-array (unsigned-byte 4) 1))

(deftest sequence-type-rank.concatenate
  (let ((v (concatenate 'seq-rank-code-table
                        (make-array 3 :initial-element 8)
                        (make-array 2 :initial-element 9))))
    (list (length v) (coerce v 'list) (typep v 'seq-rank-code-table)))
  (5 (8 8 8 9 9) t))

(deftest sequence-type-rank.coerce
  (let ((v (coerce (make-array 32 :initial-element 5) 'seq-rank-code-table)))
    (list (length v) (aref v 31)))
  (32 5))

(deftest sequence-type-rank.make-sequence
  (list (length (make-sequence 'seq-rank-code-table 7))
        (length (make-sequence '(simple-array t (3)) 3))
        (length (make-sequence '(simple-vector 4) 4)))
  (7 3 4))

(deftest sequence-type-rank.length-still-checked
  (mapcar (lambda (thunk)
            (handler-case (progn (funcall thunk) :no-error)
              (type-error () :type-error)))
          (list (lambda () (concatenate '(simple-array t (3)) #(1 2)))
                (lambda () (concatenate '(vector t 2) #(1 2 3)))
                (lambda () (coerce '(1 2) '(simple-array t (3))))
                (lambda () (make-sequence '(simple-array t (3)) 2))
                (lambda () (make-sequence '(simple-vector 3) 2))))
  (:type-error :type-error :type-error :type-error :type-error))

(deftest sequence-type-rank.other-ranks-are-not-sequences
  (mapcar (lambda (thunk)
            (handler-case (progn (funcall thunk) :no-error)
              (type-error () :type-error)))
          (list (lambda () (concatenate '(simple-array t 2) #(1 2)))
                (lambda () (make-sequence '(array t (2 2)) 4))))
  (:type-error :type-error))
