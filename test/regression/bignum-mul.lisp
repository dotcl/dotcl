;;; Integer multiplication must stay bit-identical when the 64-bit-limb path
;;; is limited to larger operands. The reference uses shifts and additions.

(defun %bm-next (s)
  (mod (+ (* s 6364136223846793005) 1442695040888963407) (expt 2 64)))

(defun %bm-number (bits seed)
  "A BITS-bit positive integer with the top bit set, and the next seed."
  (let ((x 0) (s seed))
    (loop for got from 0 below bits by 64
          do (setf s (%bm-next s) x (logior (ash x 64) s)))
    (values (logior (ash 1 (1- bits)) (ldb (byte bits 0) x)) s)))

(defun %bm-ref (a b)
  (cond ((zerop b) 0)
        ((minusp b) (- (%bm-ref a (- b))))
        (t (let ((r 0) (i 0))
             (loop while (plusp b)
                   do (when (oddp b) (setq r (+ r (ash a i))))
                      (setq b (ash b -1) i (1+ i)))
             r))))

(defun %bm-ok (a b)
  (= (* a b) (%bm-ref a b)))

(deftest bignum-mul.random
  (let ((bad '()) (seed 24680))
    (dolist (bits '(60 64 65 95 96 127 128 129 159 160 161 191 192 193
                    255 256 257 287 288 319 320 321 383 384 385 511 512
                    513 767 768 1023 1024 1535 1536 2047 2048 2049 3072 4096))
      (dotimes (k 2)
        (multiple-value-bind (a s1) (%bm-number bits seed)
          (multiple-value-bind (b s2) (%bm-number (+ bits (if (evenp k) 0 37)) s1)
            (setf seed s2)
            (unless (%bm-ok a b) (push (list bits k) bad))
            (unless (%bm-ok (- a) (- b)) (push (list bits k '-) bad))
            (unless (%bm-ok a (- b)) (push (list bits k '+ -) bad))
            (unless (%bm-ok (- a) b) (push (list bits k '- +) bad))))))
    bad)
  nil)

(deftest bignum-mul.fixnum-times-bignum
  (let ((bad '()) (seed 13579))
    (dolist (f (list 0 1 -1 2 -2 100000 most-positive-fixnum most-negative-fixnum))
      (dolist (bits '(64 65 128 129 160 192 256 320 384 512 1024 2048 4096))
        (multiple-value-bind (a s1) (%bm-number bits seed)
          (setf seed s1)
          (dolist (b (list a (- a)))
            (unless (%bm-ok f b) (push (list f bits) bad))
            (unless (%bm-ok b f) (push (list bits f) bad))))))
    bad)
  nil)

(deftest bignum-mul.powers-of-two
  (let ((bad '()))
    (dolist (p '(0 1 31 32 63 64 65 127 128 129 191 192 193 255 256 319
                 320 384 512 1024 2047 2048 4096 8192))
      (dolist (q '(0 1 63 64 65 128 255 320 512 1024 2048 4096))
        (let* ((a (ash 1 p)) (b (ash 1 q))
               (want (ash 1 (+ p q))))
          (unless (= (* a b) want) (push (list p q) bad))
          (unless (= (* (- a) b) (- want)) (push (list p q '-) bad))
          (unless (= (* a (- b)) (- want)) (push (list p q '+ -) bad))
          (unless (= (* (- a) (- b)) want) (push (list p q '- -) bad)))))
    bad)
  nil)

(deftest bignum-mul.edges
  (let ((bad '()))
    (dolist (bits '(96 128 160 192 256 320 384 512 1024 2048 4096 8192))
      (let* ((ones (1- (ash 1 bits)))
             (top (ash 1 (1- bits)))
             (cases (list (list ones ones) (list ones (1+ ones))
                          (list top top) (list top (1- top))
                          (list (+ top (1- (ash 1 (floor bits 2)))) ones)
                          (list (1- (ash 1 (+ bits 65))) ones)
                          (list (- (1- (ash 1 bits))) (ash 1 (1- bits))))))
        (dolist (c cases)
          (unless (%bm-ok (first c) (second c)) (push (list bits c) bad)))))
    (dolist (a (list 0 1 -1 most-positive-fixnum most-negative-fixnum
                     (expt 2 63) (- (expt 2 63)) (1- (expt 2 63))))
      (dolist (b (list 0 1 -1 (expt 2 64) (- (expt 2 64))
                       (1- (ash 1 200)) (ash 1 300) (- (ash 1 4096))))
        (unless (%bm-ok a b) (push (list a b) bad))))
    bad)
  nil)

(deftest bignum-mul.known
  (list (* (- (ash 1 300) 1) (ash 1 300))
        (* (- (ash 1 521) 1) (1- (ash 1 521)))
        (* (* -1 (expt 3 400)) (- (expt 7 200)))
        (* 100000 (- (ash 1 199) 1)))
  (4149515568880992958512407863691161151012446232242436899995657329690652811412908146399707046910067817953711800342855100773016990023943381641950624197973514332679882040064803340288000
   47125446914534694131579097993419809976955095716785201420286055195012674566357244479460731079205201122720511132925006540350105785156086431086764996857554304847155991333706718342307167456986269662311038377104760933477381254100896222805785374204495333936040246318307567782851014765052850751581472024524956029996236801
   738090967671823225533371038865267670935456934609336001642110489266299849820898993506027059744363501209175310738991631712542649063520597361069426099109112273173031950719427340493626098496717318132758648815306396025043354958220333703554671663056297691040351448889300297878820974779906815155484863344495752844483869552423505347654645581383484578209378139350208001
   80346902212949513777098104617058130126110149689139641765068700000))
