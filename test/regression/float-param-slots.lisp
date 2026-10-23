;;; A parameter declared DOUBLE-FLOAT or SINGLE-FLOAT and read in float
;;; arithmetic lives in a raw slot, filled once from the argument (with the type
;;; check), and a tail self-call stores the next value there raw: the loop boxes
;;; nothing.

(setf dotcl:*save-sil* t)

(defun %fps-count (needle fn)
  (let ((s (%flat-text (dotcl:function-sil fn))) (k 0) (p 0))
    (loop (let ((q (search needle s :start2 p)))
            (if q (progn (incf k) (setq p (1+ q))) (return k))))))

(defun %fps-mandel (cr ci limit)
  (declare (double-float cr ci) (fixnum limit))
  (labels ((iter (zr zi k)
             (declare (double-float zr zi) (fixnum k))
             (if (or (= k limit) (> (+ (* zr zr) (* zi zi)) 4d0))
                 k
                 (iter (+ (- (* zr zr) (* zi zi)) cr) (+ (* 2d0 zr zi) ci) (+ k 1)))))
    (iter 0d0 0d0 0)))

(defun %fps-series (n)
  (declare (fixnum n))
  (labels ((iter (i x acc)
             (declare (fixnum i) (single-float x acc))
             (if (= i n)
                 acc
                 (iter (+ i 1) (+ x 0.5f0) (+ (* acc 0.5f0) x)))))
    (iter 0 0f0 0f0)))

(defun %fps-defun-loop (x n acc)
  (declare (double-float x acc) (fixnum n))
  (if (<= n 0)
      acc
      (%fps-defun-loop x (- n 1) (+ acc (* x x)))))

;;; The argument of a raw slot is checked on entry and on a tail call that is not
;;; statically a float.
(defun %fps-untyped-arg (x n)
  (declare (double-float x) (fixnum n))
  (if (<= n 0)
      (* x 2d0)
      (%fps-untyped-arg (identity-opaque x) (- n 1))))

(defun identity-opaque (x) x)
(declaim (notinline identity-opaque))

(defun %fps-te (thunk)
  (handler-case (funcall thunk) (type-error () :type-error)))

(deftest float-param-slots.values
  (list (%fps-mandel 0.25d0 0.5d0 50) (%fps-mandel 2d0 2d0 50)
        (%fps-series 4) (%fps-defun-loop 1.5d0 4 0d0)
        (%fps-untyped-arg 1.5d0 3)
        (%fps-te (lambda () (%fps-mandel 1 0d0 10)))
        (%fps-te (lambda () (%fps-defun-loop 1 2 0d0))))
  (50 1 2.125 9d0 3d0 :type-error :type-error))

(deftest-compiled-only float-param-slots.no-boxes
  (list (%fps-count "NEWOBJ \"DoubleFloat\"" #'%fps-mandel)
        (%fps-count "NEWOBJ \"SingleFloat\"" #'%fps-series)
        (%fps-count "NEWOBJ \"DoubleFloat\"" #'%fps-defun-loop))
  (0 0 0))

(setf dotcl:*save-sil* nil)
