;;; A negative or non-integer :END is a type-error whose datum is the value itself and whose
;;; expected type is (OR NULL UNSIGNED-BYTE), as SBCL reports it.

(defmacro %tee3 (form)
  `(handler-case ,form
     (type-error (e) (list (type-error-datum e) (type-error-expected-type e)))
     (error (e) (list :other (type-of e)))))

(deftest type-error-end-index.negative
  (list (%tee3 (subseq "abcde" 1 -1)) (%tee3 (position 1 "abcde" :end -1))
        (%tee3 (search "ab" "abcde" :end2 -1)) (%tee3 (string-upcase "abc" :end -1))
        (%tee3 (remove 1 (list 1 2) :end -3)))
  ((-1 (or null unsigned-byte)) (-1 (or null unsigned-byte)) (-1 (or null unsigned-byte))
   (-1 (or null unsigned-byte)) (-3 (or null unsigned-byte))))

(deftest type-error-end-index.not-an-integer
  (list (%tee3 (subseq "abcde" 1 'a)) (%tee3 (position 1 "abcde" :end 1.5))
        (%tee3 (count 1 "abcde" :end 'a)))
  ((a (or null unsigned-byte)) (1.5 (or null unsigned-byte)) (a (or null unsigned-byte))))

(deftest type-error-end-index.too-large-keeps-pair
  (%tee3 (subseq "abc" 1 10))
  ((1 . 10) (cons (integer 0 3) (integer 1 3))))
