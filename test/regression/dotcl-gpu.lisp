;;; regression: full-tier-only
;;; contrib/dotcl-gpu: GPU-MAP runs a lambda over arrays on ILGPU's CPU accelerator.
;;; Does nothing when ILGPU cannot be restored (no .NET SDK, no network) or when
;;; the build has no code generation (the kernel is built from the IL dotcl emits).

(require "dotcl-gpu")

(defvar *dg-ok*
  (and (member :dotcl-emit *features*)
       (handler-case (progn (dotcl-gpu:ensure-ilgpu) t)
         (error () nil))))

(defun dg-vector (n &optional (f #'identity))
  (let ((v (make-array n :element-type 'double-float)))
    (dotimes (i n v) (setf (aref v i) (funcall f (float i 1d0))))))

(defun dg-close (a b)
  (and (= (length a) (length b))
       (every (lambda (x y) (< (abs (- x y)) 1d-9)) a b)))

(deftest dotcl-gpu.double-the-array
  (or (not *dg-ok*)
      (let ((out (dotcl-gpu:gpu-map '(lambda (x) (* 2d0 x)) (dg-vector 1000))))
        (and (typep out '(simple-array double-float (*)))
             (= (length out) 1000)
             (dg-close out (dg-vector 1000 (lambda (x) (* 2d0 x)))))))
  t)

(deftest dotcl-gpu.two-arrays
  (or (not *dg-ok*)
      (dg-close (dotcl-gpu:gpu-map '(lambda (a b) (+ (* a a) b)) (dg-vector 100) (dg-vector 100 #'-))
                (dg-vector 100 (lambda (x) (+ (* x x) (- x))))))
  t)

(deftest dotcl-gpu.three-arrays
  (or (not *dg-ok*)
      (dg-close (dotcl-gpu:gpu-map '(lambda (a x y) (+ (* a x) y))
                                   (dg-vector 50 (lambda (x) (declare (ignore x)) 2d0))
                                   (dg-vector 50) (dg-vector 50 (lambda (x) (declare (ignore x)) 1d0)))
                (dg-vector 50 (lambda (x) (+ (* 2d0 x) 1d0)))))
  t)

(deftest dotcl-gpu.math-call
  (or (not *dg-ok*)
      (dg-close (dotcl-gpu:gpu-map '(lambda (x) (sqrt (+ (* x x) 1d0))) (dg-vector 100))
                (dg-vector 100 (lambda (x) (sqrt (+ (* x x) 1d0))))))
  t)

(deftest dotcl-gpu.kernel-is-kept
  (or (not *dg-ok*)
      (progn
        (dotcl-gpu:clear-kernel-cache)
        (dotcl-gpu:gpu-map '(lambda (x) (+ x 3d0)) (dg-vector 10))
        (let ((after-first (hash-table-count dotcl-gpu::*kernels*)))
          (dotcl-gpu:gpu-map (list 'lambda '(x) '(+ x 3d0)) (dg-vector 10))
          (equal (list after-first (hash-table-count dotcl-gpu::*kernels*)) '(1 1)))))
  t)

(deftest dotcl-gpu.empty-array
  (or (not *dg-ok*)
      (equalp (dotcl-gpu:gpu-map '(lambda (x) (* 2d0 x)) (dg-vector 0)) (dg-vector 0)))
  t)

;;; What a device function cannot be: a branch, a loop.
(deftest dotcl-gpu.unsupported-function
  (or (not *dg-ok*)
      (flet ((unsupported-p (form)
               (handler-case (progn (dotcl-gpu:gpu-map form (dg-vector 4)) :no-error)
                 (dotcl-gpu:unsupported-kernel () :unsupported))))
        (equal (list (unsupported-p '(lambda (x) (if (> x 2d0) x 0d0)))
                     (unsupported-p '(lambda (x) (do ((i 0 (1+ i)) (s 0d0 (+ s x))) ((= i 3) s)))))
               '(:unsupported :unsupported))))
  t)

(deftest dotcl-gpu.argument-errors
  (or (not *dg-ok*)
      (flet ((err (thunk) (handler-case (progn (funcall thunk) :no-error) (error () :error))))
        (every (lambda (r) (eq r :error))
               (list (err (lambda () (dotcl-gpu:gpu-map 'car (dg-vector 3))))
                     (err (lambda () (dotcl-gpu:gpu-map '(lambda (x) x) (vector 1d0 2d0))))
                     (err (lambda () (dotcl-gpu:gpu-map '(lambda (x y) x) (dg-vector 3) (dg-vector 4))))
                     (err (lambda () (dotcl-gpu:gpu-map '(lambda (x) x))))
                     (err (lambda () (dotcl-gpu:gpu-map '(lambda (x y) x) (dg-vector 3))))))))
  t)
