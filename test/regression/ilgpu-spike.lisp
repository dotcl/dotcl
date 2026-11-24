;;; regression: full-tier-only
;;; samples/GpuSpike: a Lisp function compiled by dotcl runs as an ILGPU kernel
;;; (CPU accelerator). Skipped when ILGPU cannot be restored (no network, no SDK);
;;; needs code generation to compile the function.

;;; The package does not exist when the sample cannot be loaded, so its symbols are
;;; looked up at run time.
(defvar *gpu-spike-ok*
  (handler-case (progn (unless (member :dotcl-emit *features*) (error "no code generation"))
                       (load "samples/GpuSpike/ilgpu-spike.lisp")
                       (funcall (find-symbol "ENSURE-ILGPU" "GPU-SPIKE"))
                       t)
    (error () nil)))

(defun gs (name &rest args)
  (apply (find-symbol name "GPU-SPIKE") args))

(defvar *gpu-spike-fasl*
  (when *gpu-spike-ok*
    (let ((src (merge-pathnames "gpu-spike.lisp" (regression-temp-dir/)))
          (fasl (merge-pathnames "gpu-spike.fasl" (regression-temp-dir/))))
      (with-open-file (s src :direction :output :if-exists :supersede)
        (let ((*package* (find-package :cl-user)))
          (prin1 '(declaim (ftype (function (double-float double-float double-float) double-float)
                                  gs-axpy gs-hyp))
                 s)
          (terpri s)
          (prin1 '(defun gs-axpy (a x y)
                   (declare (optimize (debug 0) (safety 0)) (double-float a x y))
                   (+ (* a x) y))
                 s)
          (terpri s)
          (prin1 '(defun gs-hyp (a x y)
                   (declare (optimize (debug 0) (safety 0)) (double-float a x y))
                   (sqrt (+ (* a a) (* x x) (* y y))))
                 s)))
      (let ((*error-output* (make-broadcast-stream)))
        (compile-file src :output-file fasl))
      fasl)))

(deftest-emitting-only ilgpu-spike.typed-entry-is-straight-line-il
  (or (not *gpu-spike-ok*)
      (let ((il (gs "TYPED-ENTRY-IL" (gs "FIND-TYPED-ENTRY" *gpu-spike-fasl* "GS_AXPY_typed"))))
        (equal (mapcar #'car il)
               '("ldarg.1" "stloc.0" "ldarg.2" "stloc.1" "ldarg.3" "stloc.2"
                 "ldloc.0" "ldloc.1" "mul" "ldloc.2" "add" "ret"))))
  t)

(deftest-emitting-only ilgpu-spike.axpy
  (or (not *gpu-spike-ok*)
      (let ((y (gs "RUN-TYPED-ENTRY" *gpu-spike-fasl* "GS_AXPY_typed" 2d0 :n 1000)))
        (and (= (length y) 1000)
             (= (first y) 1d0) (= (nth 10 y) 21d0) (= (car (last y)) 1999d0)
             (every (lambda (v i) (= v (+ (* 2d0 i) 1d0))) y (loop for i below 1000 collect (float i 1d0))))))
  t)

(deftest-emitting-only ilgpu-spike.sqrt-call
  (or (not *gpu-spike-ok*)
      (let ((y (gs "RUN-TYPED-ENTRY" *gpu-spike-fasl* "GS_HYP_typed" 2d0 :n 100)))
        (every (lambda (v i) (< (abs (- v (sqrt (+ 4d0 (* i i) 1d0)))) 1d-9))
               y (loop for i below 100 collect (float i 1d0)))))
  t)
