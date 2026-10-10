;;; The first value of (FLOOR X D) / (TRUNCATE X D) with a constant divisor has
;;; a known range when X does, and is computed as a raw int64.
;;;
;;; (dotimes (k (floor len 8)) (let ((o (* 8 k))) ...)) with LEN declared
;;; (mod array-dimension-limit) called Runtime.FloorFixnumBoxed and
;;; Runtime.DotimesLimit for the count, and the counter had no range, so (* 8 k)
;;; was Runtime.MultiplyFixnum (boxed) and every offset built from it went
;;; through the generic + . The quotient is monotonic in X, so its range is the
;;; quotients of X's bounds. A power-of-two divisor is a shift.

(setf dotcl:*save-sil* t)

(deftype %fcd-index () '(mod #.array-dimension-limit))

(defun %fcd-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %fcd-words (len a out)
  (declare (type (simple-array (unsigned-byte 8) (*)) a)
           (type (simple-array fixnum (*)) out)
           (type %fcd-index len)
           (optimize speed (safety 0)))
  (dotimes (k (floor len 8))
    (let ((o (* 8 k)))
      (setf (aref out k) (+ (aref a (+ o 7)) o))))
  out)

(defun %fcd-trunc (len)
  (declare (type %fcd-index len))
  (let ((n (truncate len 3)))
    (* n 5)))

(deftest-emitting-only floor-const-divisor-range.sil
  (list (%fcd-count "FloorFixnum" #'%fcd-words)
        (%fcd-count "DotimesLimit" #'%fcd-words)
        (%fcd-count "MultiplyFixnum" #'%fcd-words)
        (%fcd-count "Runtime.Add" #'%fcd-words)
        (%fcd-count "TruncateFixnumBoxed" #'%fcd-trunc)
        (%fcd-count "TruncateFixnumL" #'%fcd-trunc))
  (0 0 0 0 0 1))

;;; Values: signs, rounding direction, power-of-two and other divisors,
;;; negative divisors, and the second value where it is wanted.
(defun %fcd-f8 (x) (declare (type (integer -100 100) x)) (+ (floor x 8) 0))
(defun %fcd-t8 (x) (declare (type (integer -100 100) x)) (+ (truncate x 8) 0))
(defun %fcd-tn8 (x) (declare (type (integer 0 100) x)) (+ (truncate x 8) 0))
(defun %fcd-f7 (x) (declare (type (integer -100 100) x)) (+ (floor x 7) 0))
(defun %fcd-t7 (x) (declare (type (integer -100 100) x)) (+ (truncate x 7) 0))
(defun %fcd-fm4 (x) (declare (type (integer -100 100) x)) (+ (floor x -4) 0))
(defun %fcd-tm4 (x) (declare (type (integer -100 100) x)) (+ (truncate x -4) 0))
(defun %fcd-f1 (x) (declare (type (integer -100 100) x)) (+ (floor x 1) 0))
(defun %fcd-mv (x) (declare (type (integer -100 100) x)) (floor x 8))
(defun %fcd-mvl (x) (declare (type (integer -100 100) x))
  (multiple-value-list (truncate x 8)))

(deftest floor-const-divisor-range.values
  (let ((xs '(-100 -17 -9 -8 -7 -1 0 1 7 8 9 17 100)))
    (list
     (loop for x in xs
           always (and (= (%fcd-f8 x) (floor x 8))
                       (= (%fcd-t8 x) (truncate x 8))
                       (= (%fcd-f7 x) (floor x 7))
                       (= (%fcd-t7 x) (truncate x 7))
                       (= (%fcd-fm4 x) (floor x -4))
                       (= (%fcd-tm4 x) (truncate x -4))
                       (= (%fcd-f1 x) x)
                       (equal (multiple-value-list (%fcd-mv x))
                              (multiple-value-list (floor x 8)))
                       (equal (%fcd-mvl x) (multiple-value-list (truncate x 8)))))
     (loop for x from 0 to 100 always (= (%fcd-tn8 x) (truncate x 8)))
     (list (%fcd-f8 -1) (%fcd-t8 -1) (%fcd-f8 -9) (%fcd-t8 -9))
     (let ((out (make-array 4 :element-type 'fixnum :initial-element -1))
           (a (make-array 37 :element-type '(unsigned-byte 8))))
       (dotimes (i 37) (setf (aref a i) i))
       (coerce (%fcd-words 37 a out) 'list))
     (list (%fcd-trunc 0) (%fcd-trunc 2) (%fcd-trunc 3) (%fcd-trunc 100))))
  (t t (-1 0 -2 -1) (7 23 39 55) (0 0 5 165)))

(setf dotcl:*save-sil* nil)
