;;; Results of the 64-bit-word MOD / REM and * paths that come out small, zero
;;; or at a limb boundary, and divisors whose top word is at the edges of the
;;; divisor-reciprocal computation.

(defun %blr-p (bits) (- (ash 1 bits) 159))

(deftest bignum-limb-results.mod-small
  (let ((bad '()))
    (dolist (bits '(130 256 384 521 1024 2000))
      (let ((p (%blr-p bits)))
        (dolist (k (list 3 (ash 1 70) (1- (ash 1 200))))
          (dolist (r (list 0 1 5 (1- (ash 1 31)) (ash 1 31) (1- (ash 1 32)) (ash 1 32)
                           most-positive-fixnum (1+ most-positive-fixnum) (ash 1 64)
                           (1+ (ash 1 64)) (ash 1 128) (- p 1)))
            (let ((a (+ (* p k) r)))
              (unless (and (eql (mod a p) r)
                           (eql (rem a p) r)
                           (eql (rem (- a) p) (- r))
                           (eql (mod (- a) p) (if (zerop r) 0 (- p r)))
                           (eql (mod a (- p)) (if (zerop r) 0 (- r p))))
                (push (list bits r) bad)))))))
    bad)
  nil)

(deftest bignum-limb-results.mod-types
  (let ((p (%blr-p 384)))
    (list (typep (mod (* p 7) p) 'fixnum)
          (typep (mod (+ (* p 7) 5) p) 'fixnum)
          (eql (mod (+ (* p 7) most-positive-fixnum) p) most-positive-fixnum)
          (typep (mod (+ (* p 7) (ash 1 63)) p) 'bignum)))
  (t t t t))

(deftest bignum-limb-results.mul
  (let ((bad '()))
    (dolist (ab '(130 200 256 384 521 1000 2048))
      (dolist (bb '(130 256 521 2048))
        (let* ((a (- (ash 1 ab) 1)) (b (- (ash 1 bb) 1))
               (expect (- (+ (ash 1 (+ ab bb)) 1) (ash 1 ab) (ash 1 bb))))
          (dolist (sa '(1 -1))
            (dolist (sb '(1 -1))
              (unless (eql (* (* sa a) (* sb b)) (* sa sb expect))
                (push (list ab bb sa sb) bad)))))))
    bad)
  nil)

(deftest bignum-limb-results.reciprocal-edges
  (let ((bad '()))
    (dolist (top (list (ash 1 63) (1+ (ash 1 63)) (1- (ash 1 64)) (- (ash 1 64) 2)
                       #xFFFFFFFF00000000 #x80000000FFFFFFFF #x8000000100000000
                       #xFFFFFFFE00000001 #xC000000000000000 #xAAAAAAAAAAAAAAAA))
      (dolist (shift '(0 1 31 32 63))
        (let ((b (ash (+ (ash top 64) #x123456789ABCDEF) (- shift))))
          (dolist (a (list (1- (ash 1 300)) (+ (ash 1 255) 77) (* b b 3)
                           (1- (* b (ash 1 130))) (* b (1- (ash 1 128)))))
            (unless (and (= (mod a b) (- a (* (floor a b) b)))
                         (= (rem (- a) b) (- (- a) (* (truncate (- a) b) b))))
              (push (list top shift) bad))))))
    bad)
  nil)
