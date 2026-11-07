;;; AREF of a string with an index the compiler holds as a raw integer reads the
;;; character directly; out-of-range, fill-pointer and non-simple strings still go
;;; through the general path with its errors.

(defun %asl-sum (str n)
  (let ((s 0)) (declare (fixnum s n))
    (dotimes (i n s) (incf s (char-code (aref str i))))))

(defun %asl-ref (str n) (declare (fixnum n)) (aref str (1+ n)))

(deftest aref-string-long-index.values
  (list (%asl-sum "abc" 3)
        (%asl-sum (make-array 3 :element-type 'character :initial-contents "xyz" :adjustable t) 3)
        (%asl-sum (make-array 5 :element-type 'character :initial-contents "xyzab" :fill-pointer 3) 3)
        (%asl-ref "hello" 1))
  (294 363 363 #\l))

(deftest aref-string-long-index.errors
  (list (handler-case (%asl-ref "abc" 2) (error () :error))
        (handler-case (%asl-ref "abc" -5) (error () :error))
        (handler-case (%asl-sum "ab" 3) (error () :error))
        (handler-case (%asl-sum (make-array 5 :element-type 'character :initial-contents "xyzab" :fill-pointer 3) 6)
          (error () :error)))
  (:error :error :error :error))
