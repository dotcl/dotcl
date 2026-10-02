;;; SXHASH of a number other than a fixnum (a bignum, a float, a ratio, a
;;; complex) was 0 for all of them. That is a legal hash but a useless one: a
;;; hash table built on SXHASH (Coalton's hash maps and hash tables are) put
;;; every bignum key in one bucket, and inserting 80000 such keys took 24
;;; minutes. They hash by value now; numbers that are EQUAL still hash alike.

(defun %sxn-distinct (keys)
  (length (remove-duplicates (mapcar #'sxhash keys))))

(deftest sxhash-numbers-by-value.spread
  (list (%sxn-distinct (loop for i below 200 collect (+ (ash 1 64) i)))
        (%sxn-distinct (loop for i below 200 collect (+ 0.5d0 i)))
        (%sxn-distinct (loop for i below 200 collect (+ 0.5f0 i)))
        (%sxn-distinct (loop for i from 1 to 200 collect (/ 1 (1+ i))))
        (%sxn-distinct (loop for i from 1 to 200 collect (complex i 1))))
  (200 200 200 200 200))

(deftest sxhash-numbers-by-value.equal-numbers-hash-alike
  (list (= (sxhash (expt 2 100)) (sxhash (* (expt 2 50) (expt 2 50))))
        (= (sxhash 1.5d0) (sxhash (/ 3d0 2)))
        (= (sxhash 2/3) (sxhash (/ 4 6)))
        (= (sxhash #c(1 2)) (sxhash (complex 1 2)))
        (typep (sxhash (expt 2 100)) '(and fixnum unsigned-byte)))
  (t t t t t))
