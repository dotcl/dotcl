;;; A LET* variable that the body assigns, whose init and every assigned value
;;; stay in one integer range, is held in a raw Int64 slot, and an inline call
;;; assigned to it (a block around a LET ending in an IF) is computed raw rather
;;; than boxed at the merge. Results match the generic computation.

(declaim (inline %lsar-step))
(defun %lsar-step (bit rmdr poly mask)
  (declare (type (signed-byte 56) rmdr poly mask) (type bit bit))
  (let ((nr (logior bit (* rmdr 2))))
    (if (zerop (logand mask nr)) nr (logxor nr poly))))

(defun %lsar-run (poly n)
  (declare (type (signed-byte 56) poly) (fixnum n))
  (let* ((mask (ash 1 (1- (integer-length poly))))
         (r (%lsar-step 1 0 poly mask)))
    (dotimes (k (- n 1))
      (setf r (%lsar-step 0 r poly mask)))
    r))

;; The same arithmetic with no declarations and no inline function.
(defun %lsar-ref (poly n)
  (let* ((mask (ash 1 (1- (integer-length poly))))
         (r (let ((nr (logior 1 (* 0 2)))) (if (zerop (logand mask nr)) nr (logxor nr poly)))))
    (dotimes (k (- n 1))
      (setf r (let ((nr (logior 0 (* r 2)))) (if (zerop (logand mask nr)) nr (logxor nr poly)))))
    r))

(deftest let-star-assigned-int.crc-step-matches-reference
  (loop for poly in '(1099587256329 3 7 1023 33554467 -5 36028797018963967)
        for n in '(2000 50 1 17 3000 40 10)
        unless (eql (%lsar-run poly n) (%lsar-ref poly n))
          collect (list poly n))
  nil)

(defun %lsar-dup (n)
  (declare (fixnum n))
  (let* ((x 1) (x (+ x 10)) (y 0))
    (dotimes (i n) (setf y (+ y x)))
    (list x y)))

(deftest let-star-assigned-int.duplicate-names
  (%lsar-dup 3)
  (11 33))

(defun %lsar-later-init (n)
  (declare (fixnum n))
  (let* ((a 5) (b (* a 2)) (c 0))
    (dotimes (i n) (setf a (+ a 1)) (setf c (+ c b)))
    (list a b c)))

(deftest let-star-assigned-int.later-init-reads-earlier
  (%lsar-later-init 4)
  (9 10 40))

(defun %lsar-grow (n)
  (let* ((a 1) (b 0))
    (dotimes (i n) (setf a (* a 3)) (setf b (+ b a)))
    (list a b)))

(deftest let-star-assigned-int.unbounded-growth-stays-exact
  (%lsar-grow 50)
  (717897987691852588770249 1076846981537778883155372))

(declaim (inline %lsar-big))
(defun %lsar-big (x) (declare (type integer x)) (let ((y (* x x))) (if (> y 0) y (- y))))

(defun %lsar-assign-fixnum (x)
  (let ((r 0))
    (declare (fixnum r))
    (setq r (%lsar-big x))
    r))

(deftest-compiled-only let-star-assigned-int.fixnum-declaration-violation-still-signals
  (list (%lsar-assign-fixnum 100)
        (handler-case (%lsar-assign-fixnum (expt 2 40)) (error () :error)))
  (10000 :error))
