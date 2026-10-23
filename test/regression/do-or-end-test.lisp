;;; A DO whose end test is an OR with a counting comparison among its clauses
;;; ((OR (> (ABS Z) 4.0) (> ITER 300)), cl-bench mandelbrot/dfloat) counts the
;;; variable in an int64 slot, as the bare comparison does. The loop can end
;;; earlier through another clause; it never goes past the comparison's limit.
;;; Values were checked against SBCL.

(setf dotcl:*save-sil* t)

(defun %dor-mset (c1 c2)
  (declare (type double-float c1 c2))
  (let ((z1 0.0d0) (z2 0.0d0) (aux 0.0d0))
    (declare (double-float z1 z2 aux))
    (do ((iter 0 (1+ iter)))
        ((or (> (abs (+ (* z1 z1) (* z2 z2))) 4.0)
             (> iter 300))
         iter)
      (setq aux z1
            z1 (+ (* z1 z1) (- (* z2 z2)) c1)
            z2 (+ (* 2.0d0 z2 aux) c2)))))

(defun %dor-first (v)
  (do ((i 0 (1+ i)))
      ((or (>= i (length v)) (eql (svref v i) 'x)) i)))

(defun %dor-limit (n k)
  (declare (fixnum n k))
  (let ((r '()))
    (do ((i 0 (+ i 3)))
        ((or (= (length r) k) (>= i n)) (nreverse r))
      (push i r))))

(defun %dor-down (n)
  (declare (fixnum n))
  (do ((i n (1- i)) (acc '() (cons i acc)))
      ((or (< i 0) (and (evenp i) (< i 3))) (list i acc))))

(defun %dor-top (start)
  (do ((i start (1+ i)))
      ((or nil (> i 9223372036854775807)) i)))

(defun %dor-nested-or (n)
  (do ((i 0 (1+ i)))
      ((or (> i n) (or (= i 7) nil)) i)))

(deftest do-or-end-test.values
  (list (loop for i below 20 collect (%dor-mset 0.0001d0 (/ i 20 0.25d0)))
        (%dor-first #(a b x c)) (%dor-first #(a b))
        (%dor-limit 100 4) (%dor-limit 10 100)
        (%dor-down 9) (%dor-down -1)
        (%dor-top 9223372036854775806)
        (%dor-nested-or 3) (%dor-nested-or 100))
  ((301 301 301 301 18 17 3 2 2 2 1 1 1 1 1 1 1 1 1 1)
   2 2 (0 3 6 9) (0 3 6 9) (2 (3 4 5 6 7 8 9)) (-1 NIL)
   9223372036854775808 4 7))

(deftest-emitting-only do-or-end-test.shape
  (let ((s (%flat-text (dotcl:function-sil #'%dor-mset))))
    (list (and (search "Math.AbsDouble" s) t)
          (search "Runtime.IsTrueGt" s)
          (search "Runtime.Increment" s)))
  (t nil nil))
