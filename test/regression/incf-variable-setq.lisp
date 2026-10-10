;;; INCF / DECF of a variable is a SETQ of the sum, and a literal delta stays
;;; in the sum.
;;;
;;; The expansion went through the place's store temporary and a temporary for
;;; the delta: (incf i) was (let* ((d 1) (new (+ i d))) (setq i new)). Both
;;; temporaries are boxed bindings. On a variable whose slot holds a raw int64
;;; (a FIXNUM LET binding, a declared parameter) the SETQ lowers the sum it is
;;; given natively, but here it was given NEW, so every step went through the
;;; generic promoting add and unboxed the result again, and the 1 was boxed
;;; first. The same loop written with SETQ had none of that.

(setf dotcl:*save-sil* t)

(defun %ivs-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (+ q 1))) (return k))))))

(defun %ivs-sum (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0)))
  (let ((i 0) (s 0))
    (declare (fixnum i s))
    (loop until (>= i n) do (incf s i) (incf i))
    s))

(defun %ivs-down (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0)))
  (let ((i n) (k 0))
    (declare (fixnum i k))
    (loop until (<= i 0) do (decf i 2) (incf k))
    k))

(deftest incf-variable-setq.expansion
  (list (macroexpand-1 '(incf v))
        (macroexpand-1 '(decf v 3))
        (car (macroexpand-1 '(incf v (f)))))
  ((setq v (+ v 1)) (setq v (- v 3)) let))

;; No promoting add, no boxed 1: the steps are checked int64 adds.
(deftest-emitting-only incf-variable-setq.raw-steps
  (list (%ivs-count "Runtime.AddFixnum)" #'%ivs-sum)
        (%ivs-count "Runtime.SubtractFixnum)" #'%ivs-down)
        (%ivs-count "Runtime.AddFixnum)" #'%ivs-down)
        (%ivs-count "UNBOX-FIXNUM" #'%ivs-sum))
  (0 0 0 1))

(defvar *ivs-special* 0)

(deftest incf-variable-setq.values
  (list (%ivs-sum 100) (%ivs-sum 70000) (%ivs-down 7)
        ;; The delta is evaluated once, before the variable is read.
        (let ((x 1) (calls 0))
          (incf x (progn (incf calls) 10))
          (decf x (progn (incf calls) 0.5d0))
          (list x calls))
        ;; Promotion past fixnum on an undeclared variable.
        (let ((x most-positive-fixnum)) (incf x) (- x most-positive-fixnum))
        ;; A special variable and a symbol macro still update their place.
        (let ((*ivs-special* 5)) (incf *ivs-special* 2) *ivs-special*)
        (let ((c (list 1 2)))
          (symbol-macrolet ((head (car c)))
            (incf head 40)
            (decf head))
          c)
        ;; INCF returns the new value.
        (let ((x 3)) (list (incf x) (decf x 10) x)))
  (4950 2449965000 4 (10.5d0 2) 1 7 (40 2) (4 -6 -6)))

(setf dotcl:*save-sil* nil)
