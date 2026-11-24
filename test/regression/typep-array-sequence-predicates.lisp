;;; (TYPEP x 'SEQUENCE / 'ARRAY / 'BIT-VECTOR) with a constant type is a direct predicate. It must answer
;;; as the general TYPEP does for every kind of object.

(defun %tap-objects ()
  (list nil '(1 2) '(1 . 2) #() #(1 2) "str" (make-string 3) (make-array 3 :element-type 'bit)
        (make-array '(2 2)) (make-array 3 :adjustable t :fill-pointer 1) #*101 (make-array '(2 2) :element-type 'bit)
        1 1.5 'a :k #\a (lambda (x) x) (make-hash-table) (make-string-output-stream) (list nil) (vector)
        (make-array 0 :element-type 'character) (make-array 4 :element-type '(unsigned-byte 8))))

(defun %tap-compare (type-form)
  (let ((fn (compile nil `(lambda (x) (typep x ',type-form)))))
    (loop for o in (%tap-objects)
          collect (eq (and (funcall fn o) t) (and (typep o (identity type-form)) t)))))

(deftest-compiled-only typep-predicates.sequence
  (every #'identity (%tap-compare 'sequence))
  t)

(deftest-compiled-only typep-predicates.array
  (every #'identity (%tap-compare 'array))
  t)

(deftest-compiled-only typep-predicates.bit-vector
  (every #'identity (%tap-compare 'bit-vector))
  t)

(deftest-compiled-only typep-predicates.known-answers
  (list (typep '(1) 'sequence) (typep nil 'sequence) (typep #(1) 'sequence) (typep "a" 'sequence)
        (typep 1 'sequence) (typep (make-array '(2 2)) 'sequence)
        (typep (make-array '(2 2)) 'array) (typep "a" 'array) (typep 1 'array)
        (typep #*10 'bit-vector) (typep #(1) 'bit-vector) (typep (make-array '(2 2) :element-type 'bit) 'bit-vector))
  (t t t t nil nil t t nil t nil nil))
