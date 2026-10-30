;;; Shapes drawn by the random form generator: fixnum arithmetic next to the
;;; fixnum boundary that overflows into bignums, and multiple values carried
;;; through BLOCK / RETURN-FROM / LABELS / UNWIND-PROTECT / CATCH.

(defun %rfm-edge (x y)
  (declare (type fixnum x y))
  (list (+ x y) (* x 3) (- x) (abs x) (ash x 1)
        (floor x 3) (if (> (+ x y) 4611686018427387903) 1 0)))

(defun %rfm-half (h1 h2)
  (declare (type (integer -2305843009213693951 2305843009213693951) h1 h2))
  (list (the fixnum (+ h1 h2)) (the fixnum (- h1 h2)) (the (signed-byte 63) (* h1 2))))

(defun %rfm-grow (x y n)
  (let ((acc x))
    (declare (type integer acc))
    (dotimes (i n) (setq acc (+ acc (ash acc -1) y)))
    acc))

(defun %rfm-mv (x y z)
  (let ((u 0))
    (declare (type (integer 0 1000) u))
    (list
     (multiple-value-bind (a b c)
         (block blk
           (unwind-protect (return-from blk (values x y z))
             (setq u (+ u 1))))
       (+ a b c))
     (or (nth-value 2 (block blk
                        (unwind-protect (return-from blk (values x y z))
                          (incf u))))
         0)
     (labels ((g (n)
                (declare (type (integer 0 10) n))
                (if (<= n 0)
                    (values x y z)
                    (multiple-value-bind (a b c) (g (1- n))
                      (values b c (+ a n))))))
       (multiple-value-call #'+ (g 3)))
     (multiple-value-bind (a b)
         (catch 'tag (unwind-protect (throw 'tag (values x y)) (incf u)))
       (+ a b))
     (multiple-value-bind (a b c)
         (multiple-value-prog1 (values x y z) (incf u) (unwind-protect (values 9) (incf u)))
       (+ a b c))
     (multiple-value-call #'+ (values x y) (block blk (return-from blk (values y z))) (values))
     u)))

(deftest random-forms-mv-fixnum-shapes
  (list (%rfm-edge 4611686018427387903 4611686018427387903)
        (%rfm-edge -4611686018427387904 -1)
        (%rfm-half 2305843009213693951 -2305843009213693951)
        (%rfm-grow 4611686018427387903 5 70)
        (%rfm-mv 1 2 3))
  ((9223372036854775806 13835058055282163709 -4611686018427387903
    4611686018427387903 9223372036854775806 1537228672809129301 1)
   (-4611686018427387905 -13835058055282163712 4611686018427387904
    4611686018427387904 -9223372036854775808 -1537228672809129302 0)
   (0 4611686018427387902 4611686018427387902)
   9777951191379850022552067010646
   (6 3 12 3 6 8 5)))
