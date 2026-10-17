;;; REM and MOD of integers whose divisor has 2 to 32 limbs of 64 bits and whose
;;; dividend has at most 64, against the quotient from TRUNCATE / FLOOR.

(defun %brw-next (s) (mod (+ (* s 6364136223846793005) 1442695040888963407) (expt 2 64)))

(defun %brw-number (bits seed)
  "A BITS-bit integer (top bit set) from SEED, and the next seed."
  (let ((x 0) (s seed))
    (loop for got from 0 below bits by 64
          do (setf s (%brw-next s) x (logior (ash x 64) s)))
    (values (logior (ash 1 (1- bits)) (ldb (byte bits 0) x)) s)))

(defun %brw-check (a b)
  (and (= (rem a b) (- a (* (truncate a b) b)))
       (= (mod a b) (- a (* (floor a b) b)))))

(deftest bignum-rem-words.random
  (let ((bad '()) (seed 12345))
    (dolist (bb '(65 100 127 128 129 192 255 256 257 384 521 640 1000 1023 1024 1025 1536 2047 2048))
      (dolist (extra '(0 1 2 63 64 65 200 521 1024 2048))
        (let ((ab (+ bb extra)))
          (dotimes (k 3)
            (multiple-value-bind (a s1) (%brw-number ab seed)
              (multiple-value-bind (b s2) (%brw-number bb s1)
                (setf seed s2)
                (dolist (sa '(1 -1))
                  (dolist (sb '(1 -1))
                    (unless (%brw-check (* sa a) (* sb b))
                      (push (list ab bb sa sb) bad))))))))))
    bad)
  nil)

(deftest bignum-rem-words.edges
  (let ((bad '()))
    (dolist (bb '(65 128 129 256 521 1024 2048))
      (let* ((ones (1- (ash 1 bb)))
             (pow (ash 1 (1- bb)))
             (cases (list (list (* ones ones) ones)
                          (list (1- (* ones ones)) ones)
                          (list (+ (* ones (1- (ash 1 64))) (1- ones)) ones)
                          (list (1- (ash 1 (* 2 bb))) (1+ pow))
                          (list (ash 1 (+ bb 100)) (1- pow))
                          (list (* pow pow) pow)
                          (list (+ (* (1+ pow) (1- (ash 1 bb))) pow) (1+ pow))
                          (list ones ones)
                          (list (1- ones) ones)
                          (list (+ (ash ones 64) (1- (ash 1 64))) ones))))
        (dolist (c cases)
          (destructuring-bind (a b) c
            (unless (%brw-check a b) (push (list bb (integer-length a)) bad))))))
    bad)
  nil)

(deftest bignum-rem-words.values
  (let ((p (- (expt 2 521) 1)))
    (list (mod (expt 3 400) p) (rem (- (expt 3 700)) (expt 7 100))
          (mod (- (expt 3 700)) (expt 7 100)) (mod (expt 3 700) (- (expt 7 100)))))
  (5452381628096975477683399792634254755373083568376763922364192897506434467969082568047246725454888454853403848622364712605369235692416180127109045066507248597
   -132450768561278027262888164123670056978784076338769597509052399021227404036616112390
   3102025741063479964081759604976546753878419122565855803424842932370464055600311947611
   -3102025741063479964081759604976546753878419122565855803424842932370464055600311947611))
