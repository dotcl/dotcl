;;; DO variables without a declaration whose start, step and limit are fixnums
;;; (cl-bench fft: (do ((i j (+ i le))) ((> i n)))) get an int64 slot. The last
;;; step can leave int64 when the limit is near the top; it then leaves the
;;; loop, which is what the end test would have done. A step by a variable
;;; whose sign is not known is compiled twice and chosen by the sign at entry.
;;; Values were checked against SBCL with (signed-byte 64) for fixnum.

(setf dotcl:*save-sil* t)

(defun %dvs-sil (fn) (%flat-text (dotcl:function-sil fn)))

(defun %dvs-count (needle s)
  (let ((k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (1+ q))) (return k))))))

(defun %dvs-fft (n m)
  (declare (fixnum n m))
  (let ((sum 0) (le 0) (le1 0))
    (declare (fixnum sum le le1))
    (do ((l 1 (1+ l))) ((> l m))
      (setq le (expt 2 l) le1 (floor le 2))
      (do ((j 1 (1+ j))) ((> j le1))
        (do ((i j (+ i le))) ((> i n))
          (setq sum (logand (+ sum (* i l)) #xffffff)))))
    sum))

(defun %dvs-counts (m le1)
  (declare (fixnum m le1))
  (let ((s 0))
    (declare (fixnum s))
    (do ((l 1 (1+ l))) ((> l m))
      (do ((j 1 (1+ j))) ((>= j le1))
        (setq s (logand (+ s j l) #xffff))))
    s))

(defun %dvs-pos (j le n)
  (declare (fixnum j n) (type (integer 1 1000) le))
  (let ((r '()))
    (do ((i j (+ i le))) ((> i n) (nreverse r)) (push i r))))

(defun %dvs-var (j le n k)
  (declare (fixnum j le n k))
  (let ((r '()) (c 0))
    (declare (fixnum c))
    (do ((i j (+ i le))) ((> i n) (nreverse r))
      (push i r)
      (when (>= (incf c) k) (return (nreverse r))))))

(defun %dvs-swap (j le n)
  (declare (fixnum j le n))
  (let ((r '()))
    (do ((i j (+ le i))) ((>= i n) (nreverse r)) (push i r))))

(defun %dvs-down (hi le lo k)
  (declare (fixnum hi le lo k))
  (let ((r '()) (c 0))
    (declare (fixnum c))
    (do ((i hi (- i le))) ((< i lo) (nreverse r))
      (push i r)
      (when (>= (incf c) k) (return (nreverse r))))))

(defun %dvs-top (n)
  (declare (fixnum n))
  (let ((r '()))
    (do ((i (- most-positive-fixnum 5) (+ i 2))) ((> i n) (nreverse r)) (push i r))))

(defun %dvs-bottom (n)
  (declare (fixnum n))
  (let ((r '()))
    (do ((i (+ most-negative-fixnum 4) (1- i))) ((<= i n) (nreverse r)) (push i r))))

(defun %dvs-result-reads (j le n)
  (declare (fixnum j le n))
  (do ((i j (+ i le))) ((> i n) i)))

(defun %dvs-captured (j le n)
  (declare (fixnum j le n))
  (let ((fs '()))
    (do ((i j (+ i le))) ((> i n))
      (push (lambda () i) fs))
    (mapcar #'funcall fs)))

(defun %dvs-parallel (n)
  (declare (fixnum n))
  (do ((i (- most-positive-fixnum 3) (+ i 2))
       (k 0 (1+ k))
       (acc '() (cons i acc)))
      ((> i n) (list k acc))))

(defun %dvs-assigned-step (n k)
  (declare (fixnum n k))
  (let ((d 1) (r '()) (c 0))
    (declare (fixnum d c))
    (do ((i 0 (+ i d))) ((> i n) (nreverse r))
      (push i r)
      (setq d (if (evenp c) -2 3))
      (when (>= (incf c) k) (return (nreverse r))))))

(defvar *dvs-step* 1)
(declaim (fixnum *dvs-step*))
(defun %dvs-flip () (setq *dvs-step* (- *dvs-step*)))
(defun %dvs-special-step (n k)
  (declare (fixnum n k))
  (let ((r '()) (c 0))
    (declare (fixnum c))
    (setq *dvs-step* 5)
    (do ((i 0 (+ i *dvs-step*))) ((> i n) (nreverse r))
      (push i r)
      (%dvs-flip)
      (when (>= (incf c) k) (return (nreverse r))))))

(defun %dvs-mpf () most-positive-fixnum)
(defun %dvs-mnf () most-negative-fixnum)

(deftest do-variable-step.values
  (let ((mpf most-positive-fixnum) (mnf most-negative-fixnum))
    (list (%dvs-fft 100 4) (%dvs-fft 1024 10) (%dvs-fft 7 0)
          (%dvs-counts 5 7) (%dvs-counts 0 3)
          (%dvs-pos 3 5 30) (%dvs-pos (- mpf 2500) 1000 mpf) (%dvs-pos 10 1 9)
          (%dvs-var 1 3 20 100) (%dvs-var (- mpf 2500) 1000 mpf 100)
          (%dvs-var 0 -3 5 4) (%dvs-var 0 (- mpf) 5 4) (%dvs-var 7 0 9 3)
          (%dvs-var 0 mpf mpf 10)
          (%dvs-swap 0 4 17) (%dvs-swap (- mpf 1) 1 mpf)
          (%dvs-down 20 6 0 100) (%dvs-down (+ mnf 10) 4 mnf 100)
          (%dvs-down 5 -5 3 4) (%dvs-down 5 (- mpf) 3 4) (%dvs-down 5 mnf 3 3)
          (%dvs-top mpf) (%dvs-top (- mpf 2))
          (%dvs-bottom mnf)
          (%dvs-result-reads (- mpf 1) 5 mpf) (%dvs-result-reads 0 4 10)
          (%dvs-captured (- mpf 1) 1 mpf)
          (%dvs-parallel mpf)
          (%dvs-assigned-step 10 6)
          (%dvs-special-step 100 5)))
  (25448 12072448 0 195 0 (3 8 13 18 23 28)
   (9223372036854773307 9223372036854774307 9223372036854775307) NIL
   (1 4 7 10 13 16 19)
   (9223372036854773307 9223372036854774307 9223372036854775307) (0 -3 -6 -9)
   (0 -9223372036854775807 -18446744073709551614 -27670116110564327421) (7 7 7)
   (0 9223372036854775807) (0 4 8 12 16) (9223372036854775806) (20 14 8 2)
   (-9223372036854775798 -9223372036854775802 -9223372036854775806) (5 10 15 20)
   (5 9223372036854775812 18446744073709551619 27670116110564327426)
   (5 9223372036854775813 18446744073709551621)
   (9223372036854775802 9223372036854775804 9223372036854775806)
   (9223372036854775802 9223372036854775804)
   (-9223372036854775804 -9223372036854775805 -9223372036854775806
    -9223372036854775807)
   9223372036854775811 12 (9223372036854775808 9223372036854775808)
   (2 (9223372036854775806 9223372036854775804)) (0 -2 1 -1 2 0) (0 -5 0 -5 0))
)

;; The counting loops keep no generic compare or add: in the fft shape only the
;; copy taken for a negative LE does, and the limit and step stay raw.
(deftest-emitting-only do-variable-step.shape
  (flet ((counts (fn)
           (let ((s (%dvs-sil fn)))
             (list (%dvs-count "IsTrue" s) (%dvs-count "Runtime.Add)" s)
                   (%dvs-count "Runtime.Increment" s) (plusp (%dvs-count "STEPOK" s))))))
    (list (counts #'%dvs-fft) (counts #'%dvs-counts) (counts #'%dvs-pos)
          (counts #'%dvs-top) (counts #'%dvs-result-reads) (counts #'%dvs-captured)))
  ((1 1 0 t) (0 0 0 t) (0 0 0 t) (0 0 0 t) (2 2 0 nil) (2 2 0 nil)))

(setf dotcl:*save-sil* nil)
