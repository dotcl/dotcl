;;; (SQRT X) of a double that cannot be negative (a sum of squares, an ABS, a variable bound to one)
;;; is a double: it stays in the native double lane, so code that goes on to use the root does not
;;; box every intermediate. A root that can be negative still returns a COMPLEX, and NaN and -0.0
;;; behave as before.

(defun %dsn-inv (dx dy)
  (declare (double-float dx dy))
  (let* ((r2 (+ (* dx dx) (* dy dy)))
         (inv (/ 1d0 (* r2 (sqrt r2)))))
    inv))

(defun %dsn-generic-inv (dx dy)
  (let* ((r2 (+ (* dx dx) (* dy dy))))
    (/ 1d0 (* r2 (sqrt r2)))))

(defun %dsn-loop (n)
  (declare (fixnum n))
  (let ((s 0d0))
    (declare (double-float s))
    (dotimes (i n)
      (let* ((dx (+ 1d0 (* i 1d-3))) (dy 2d0)
             (r2 (+ (* dx dx) (* dy dy)))
             (inv (/ 1d0 (* r2 (sqrt r2)))))
        (setq s (+ s (* dx inv 0.5d0)))))
    s))

(defun %dsn-abs (x)
  (declare (double-float x))
  (let ((a (abs x))) (declare (double-float a)) (* a (sqrt a))))

(defun %dsn-plain (x) (declare (double-float x)) (sqrt x))

(defun %dsn-mutated (x)
  (declare (double-float x))
  (let* ((r (* x x)))
    (setq r (- r 1d0))
    (sqrt r)))

(defun %dsn-negative-literal () (let ((r -4d0)) (declare (double-float r)) (sqrt r)))

(deftest-compiled-only double-sqrt-nonneg-lane.no-allocation
  (progn (%dsn-loop 100)
         (< (min-allocated-bytes (lambda () (%dsn-loop 100000))) 4096))
  t)

(deftest double-sqrt-nonneg-lane.values
  (list (= (%dsn-inv 3d0 4d0) (/ 1d0 125d0))
        (= (%dsn-inv 1.5d0 -2.5d0) (%dsn-generic-inv 1.5d0 -2.5d0))
        (= (%dsn-loop 1000) (let ((s 0d0))
                              (dotimes (i 1000 s)
                                (let* ((dx (+ 1d0 (* i 1d-3))) (r2 (+ (* dx dx) 4d0)))
                                  (setq s (+ s (* dx (/ 1d0 (* r2 (sqrt (the (double-float 0d0) r2)))) 0.5d0)))))))
        (%dsn-abs -4d0))
  (t t t 8d0))

(deftest double-sqrt-nonneg-lane.negative-and-special-values
  (list (%dsn-plain -4d0)
        (%dsn-plain 4d0)
        (let ((r (%dsn-plain -0d0))) (list r (minusp (float-sign r))))
        (let ((r (%dsn-plain (- (/ 0d0 0d0))))) (and (floatp r) (/= r r)))
        (%dsn-mutated 0.5d0)
        (%dsn-mutated 2d0)
        (let ((r (%dsn-inv (/ 0d0 0d0) 1d0))) (and (floatp r) (/= r r)))
        (let ((r (%dsn-inv -0d0 0d0))) r)
        (%dsn-negative-literal)
        (flet ((sqrt (x) (list :local x))) (let ((r 4d0)) (declare (double-float r)) (sqrt (* r r)))))
  (#C(0d0 2d0) 2d0 (-0d0 t) t #C(0d0 0.8660254037844386d0) 1.7320508075688772d0 t
   #.(/ 1d0 0d0) #C(0d0 2d0) (:local 16d0)))
