;;; EQ on fixnums is true for equal values whatever their size, as in implementations
;;; where they are immediate objects; bignums, floats and ratios stay distinct objects.
;;; Characters are the same object only in the ASCII range.

(defun %eqfc-num (n) (+ n (parse-integer "0")))
(defun %eqfc-big (n) (+ n (parse-integer "0")))

(deftest eq-fixnum-char.fixnum-boundaries
  (mapcar (lambda (n) (eq (%eqfc-num n) (%eqfc-num n)))
          (list -129 -128 0 255 65535 65536 100000 1000000
                most-positive-fixnum most-negative-fixnum))
  (t t t t t t t t t t))

(deftest eq-fixnum-char.fixnum-computed
  (list (eq 100000 (+ 99999 (parse-integer "1")))
        (eq 65536 (+ 65535 (parse-integer "1")))
        (eq 1 2)
        (eq 100000 100001))
  (t t nil nil))

(deftest eq-fixnum-char.bignum-not-eq
  (let ((a (%eqfc-big (expt 10 30))) (b (%eqfc-big (expt 10 30))))
    (list (eq a b) (eql a b) (eq a a)))
  (nil t t))

(deftest eq-fixnum-char.char-ascii
  (list (eq (code-char (+ 97 (parse-integer "0"))) #\a)
        (eq #\a #\b)
        (eq 65 #\A))
  (t nil nil))

(deftest eq-fixnum-char.hash-table
  (let ((h (make-hash-table :test 'eq)))
    (setf (gethash (%eqfc-num 100000) h) :a
          (gethash (%eqfc-num -500000) h) :b
          (gethash #\c h) :c)
    (list (gethash (+ 99999 (parse-integer "1")) h)
          (gethash (%eqfc-num -500000) h)
          (gethash (code-char (+ 98 (parse-integer "1"))) h)
          (gethash 100001 h)
          (hash-table-count h)))
  (:a :b :c nil 3))

(deftest eq-fixnum-char.member-assoc
  (list (member (%eqfc-num 100000) '(1 100000 3) :test #'eq)
        (assoc (%eqfc-num 70000) '((1 . a) (70000 . b)) :test 'eq)
        (member (%eqfc-num 100000) '(1 100001) :test #'eq)
        (getf '(100000 :x) (%eqfc-num 100000))
        (find (%eqfc-num 100000) '(5 100000) :test #'eq))
  ((100000 3) (70000 . b) nil :x 100000))

;; A literal that is never a number is compared by identity: same answers in value and branch position.
(defun %eqfc-kw-v (x) (eq x :k))
(defun %eqfc-kw-v2 (x) (eq :k x))
(defun %eqfc-kw-b (x) (if (eq x :k) 1 2))
(defun %eqfc-t-v (x) (eq x t))
(defun %eqfc-t-b (x) (if (eq t x) 1 2))
(defun %eqfc-q-v (x) (eq x 'foo))
(defun %eqfc-q-b (x) (if (eq 'foo x) 1 2))
(defun %eqfc-nil-v (x) (eq nil x))
(defun %eqfc-eql-kw (x) (if (eql x :k) 1 2))

(deftest eq-fixnum-char.literal-nonnumber
  (list (%eqfc-kw-v :k) (%eqfc-kw-v 100000) (%eqfc-kw-v2 :k) (%eqfc-kw-v2 nil)
        (%eqfc-kw-b :k) (%eqfc-kw-b 'k)
        (%eqfc-t-v t) (%eqfc-t-v 100000) (%eqfc-t-b t) (%eqfc-t-b nil)
        (%eqfc-q-v 'foo) (%eqfc-q-v 7) (%eqfc-q-b 'foo) (%eqfc-q-b "foo")
        (%eqfc-nil-v nil) (%eqfc-nil-v 0) (%eqfc-eql-kw :k) (%eqfc-eql-kw 3))
  (t nil t nil 1 2 t nil 1 2 t nil 1 2 t nil 1 2))
