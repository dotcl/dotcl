;;; INCF / DECF of a structure slot declared FIXNUM is one primitive that adds on the raw slot.
;;; Same results, same order of evaluation, and a result outside the fixnum range or a delta that
;;; is no number is the same TYPE-ERROR, with the slot left as it was.

(defstruct sif (n 0 :type fixnum) (d 0d0 :type double-float) (any 0) (u 0 :type (unsigned-byte 8)))
(defstruct (sif2 (:include sif)) (m 1 :type fixnum))

(defun %sif-incf (s) (incf (sif-n s)))
(defun %sif-incf-by (s k) (incf (sif-n s) k))
(defun %sif-decf-by (s k) (decf (sif-n s) k))

(deftest struct-incf.values
  (let ((s (make-sif :n 5)) (b (make-sif2 :n 7 :m 100)))
    (list (%sif-incf s) (%sif-incf s) (%sif-incf-by s 10) (%sif-decf-by s 3) (sif-n s)
          (%sif-incf b) (incf (sif2-m b) 5) (decf (sif2-m b)) (sif-n b) (sif2-m b)
          (%sif-incf-by s -20) (%sif-decf-by s -1000) (%sif-incf-by s 0) (sif-n s)
          (let ((i 0)) (dotimes (k 70000) (incf (sif-n s))) (incf i) (sif-n s))))
  (6 7 17 14 14 8 105 104 8 104 -6 994 994 994 70994))

(deftest struct-incf.fixnum-range
  (let ((s (make-sif :n most-positive-fixnum)) (m (make-sif :n most-negative-fixnum)))
    (list (handler-case (%sif-incf s) (type-error () :type-error))
          (sif-n s)
          (handler-case (%sif-decf-by m 1) (type-error () :type-error))
          (sif-n m)
          (handler-case (%sif-incf-by s (expt 10 30)) (type-error () :type-error))
          (handler-case (%sif-incf-by s 1.5) (type-error () :type-error))
          (handler-case (%sif-incf-by s "x") (type-error () :type-error))
          (sif-n s)
          (%sif-decf-by s 1) (%sif-incf s) (sif-n s)
          (%sif-incf-by m 1) (%sif-decf-by m 1) (sif-n m)))
  (:type-error #.most-positive-fixnum :type-error #.most-negative-fixnum :type-error :type-error :type-error
   #.most-positive-fixnum #.(1- most-positive-fixnum) #.most-positive-fixnum #.most-positive-fixnum
   #.(1+ most-negative-fixnum) #.most-negative-fixnum #.most-negative-fixnum))

(deftest struct-incf.other-slots-and-places
  (let ((s (make-sif :d 1.5d0 :any 10 :u 250)))
    (list (incf (sif-any s) 5) (decf (sif-any s)) (incf (sif-d s) 0.25d0)
          (incf (sif-u s) 5) (handler-case (incf (sif-u s) 1) (type-error () :type-error)) (sif-u s)
          (handler-case (%sif-incf 5) (type-error () :type-error))
          (handler-case (%sif-incf nil) (type-error () :type-error))
          (handler-case (%sif-incf (make-pt-ish)) (error () :error))))
  (15 14 1.75d0 255 :type-error 255 :type-error :type-error :error))

(defun make-pt-ish () (list 1 2))

(deftest struct-incf.evaluation-order
  (let ((log nil) (s (make-sif :n 1)) (s2 (make-sif :n 100)))
    (flet ((n (x) (push x log) x))
      (incf (sif-n (n s)) (n 5))
      (decf (sif-n (progn (n :obj) s2)) (n 7)))
    (list (reverse (mapcar (lambda (x) (if (typep x (quote sif)) :s x)) log)) (sif-n s) (sif-n s2)))
  ((:s 5 :obj 7) 6 93))
