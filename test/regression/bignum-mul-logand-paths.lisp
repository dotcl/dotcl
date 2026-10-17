;;; Integer * and LOGAND across the fixnum / bignum boundary, the small-bignum
;;; multiply sizes (up to 256 bits per operand) and every sign combination.

(defun bml-pow2 (k) (ash 1 k))

(deftest bignum-mul.identity-sizes
  (let ((bad '()))
    (dolist (j '(1 31 32 33 62 63 64 65 96 127 128 129 160 191 192 193 255 256 257 300 512))
      (dolist (k '(1 32 63 64 65 127 128 129 192 255 256 257 400))
        (let* ((a (1- (bml-pow2 j))) (b (1+ (bml-pow2 k)))
               (want (+ (bml-pow2 (+ j k)) (bml-pow2 j) (- (bml-pow2 k)) -1)))
          (dolist (sa '(1 -1))
            (dolist (sb '(1 -1))
              (let ((got (* (* sa a) (* sb b)))
                    (got2 (funcall #'* (* sb b) (* sa a))))
                (unless (and (= got (* sa sb want)) (= got2 got))
                  (push (list j k sa sb) bad))))))))
    bad)
  nil)

(deftest bignum-mul.all-ones-carries
  (let ((bad '()))
    (dolist (j '(33 64 65 100 128 129 192 200 255 256))
      (dolist (k '(33 64 65 127 128 160 192 256))
        (let ((a (1- (bml-pow2 j))) (b (1- (bml-pow2 k))))
          (unless (= (* a b) (+ (bml-pow2 (+ j k)) (- (bml-pow2 j)) (- (bml-pow2 k)) 1))
            (push (list j k) bad)))))
    bad)
  nil)

(deftest bignum-mul.word-sizes-to-2048
  (let ((bad '()))
    (dolist (j '(257 300 383 384 385 448 512 513 521 600 640 768 900 1000 1023 1024 1025 1100 1536 2047 2048 2049))
      (dolist (k '(64 65 129 256 257 384 521 768 1023 1024 1025 2048 2049))
        (let* ((a (1- (bml-pow2 j))) (b (1+ (bml-pow2 k)))
               (want (+ (bml-pow2 (+ j k)) (bml-pow2 j) (- (bml-pow2 k)) -1))
               (c (1- (bml-pow2 k)))
               (want2 (+ (bml-pow2 (+ j k)) (- (bml-pow2 j)) (- (bml-pow2 k)) 1)))
          (dolist (sa '(1 -1))
            (dolist (sb '(1 -1))
              (unless (and (= (* (* sa a) (* sb b)) (* sa sb want))
                           (= (* (* sb c) (* sa a)) (* sa sb want2)))
                (push (list j k sa sb) bad)))))))
    bad)
  nil)

(deftest bignum-mul.fixnum-boundary
  (let ((lo (- (bml-pow2 63))) (hi (1- (bml-pow2 63))))
    (list (* lo -1) (* lo lo) (* hi hi) (* lo hi)
          (* (bml-pow2 64) 0) (typep (* (bml-pow2 64) 0) 'fixnum)
          (* (bml-pow2 127) -2)
          (* (bml-pow2 200) (bml-pow2 56))))
  (9223372036854775808
   85070591730234615865843651857942052864
   85070591730234615847396907784232501249
   -85070591730234615856620279821087277056
   0 t
   -340282366920938463463374607431768211456
   115792089237316195423570985008687907853269984665640564039457584007913129639936))

(deftest bignum-mul.divides-back
  (let ((bad '()) (x 1))
    (dotimes (i 40)
      (setf x (+ (* x 1000003) 12345678901))
      (let ((y (- (* x 7) 3)))
        (multiple-value-bind (q r) (floor (* x y) y)
          (unless (and (= q x) (zerop r)) (push i bad)))))
    bad)
  nil)

(deftest bignum-logand.fixnum-mask
  (let ((p (+ (bml-pow2 100) 77)) (n (- (bml-pow2 100))))
    (list (logand p 255) (logand 255 p) (logand n 255) (logand (1- n) 255)
          (logand p 0) (logand p most-positive-fixnum)
          (logand (- (bml-pow2 64) 1) (bml-pow2 62))
          (logand (- (bml-pow2 64)) most-positive-fixnum)
          (logand (- 1 (bml-pow2 64)) most-positive-fixnum)
          (funcall #'logand p 255)
          (logand p 255 15)
          (typep (logand n 255) 'fixnum)))
  (77 77 0 255 0 77 4611686018427387904 0 1 77 13 t))

(deftest bignum-logand.negative-fixnum-and-bignums
  (let ((p (+ (bml-pow2 100) 255)) (q (+ (bml-pow2 100) (bml-pow2 70) 6)))
    (list (logand p -256) (logand -256 p) (logand p q) (logand (- p) q)
          (logand (- p) (- q)) (logand p (- (bml-pow2 64)))
          (logand most-negative-fixnum (- (bml-pow2 64)))))
  (1267650600228229401496703205376 1267650600228229401496703205376
   1267650600228229401496703205382 1180591620717411303424
   -1267650601408821022214114509056 1267650600228229401496703205376
   -18446744073709551616))
