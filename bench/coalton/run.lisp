;;;; Times the Coalton kernels in kernels.lisp and the ray tracer that ships
;;;; in Coalton's examples/raytrace, on dotcl or SBCL.
;;;;
;;;; Expects Quicklisp, and Coalton (with examples/raytrace) visible to ASDF.
;;;; Start the Lisp with COALTON_ENV=release and a fresh ASDF cache. Prints one
;;;; line per kernel:
;;;;
;;;;   FIRST <kernel> <ms of the first, unmeasured warm-up run>
;;;;   RESULT <kernel> <min ms> <checksum> <all ms...>
;;;;
;;;; Environment:
;;;;   COALTON_BENCH_OUT     directory for the compiled kernels (default: /tmp/)
;;;;   COALTON_BENCH_RUNS    measured runs per kernel (default 5, after 1 warm-up)
;;;;   COALTON_BENCH_ONLY    comma-separated kernel names to run (default: all)
;;;;   COALTON_BENCH_REPEAT  run the selected kernels this many times without
;;;;                         printing, after the measurement (for profiling)

(unless (find-package "QL")
  (require "quicklisp"))

(defpackage #:coalton-bench-run
  (:use #:cl))

(in-package #:coalton-bench-run)

(defun env (name default)
  (let ((v (uiop:getenv name)))
    (if (and v (plusp (length v))) v default)))

(defun split-commas (s)
  (loop with start = 0
        for pos = (position #\, s :start start)
        collect (string-trim " " (subseq s start pos))
        while pos
        do (setf start (1+ pos))))

(defun full-gc ()
  #+sbcl (sb-ext:gc :full t)
  #+dotcl (dotcl:gc)
  #-(or sbcl dotcl) nil)

(defun now-ms ()
  (/ (get-internal-real-time) (/ internal-time-units-per-second 1000d0)))

(format t "~&IMPL ~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))
(finish-output)

(let ((t0 (now-ms)))
  (ql:quickload '("coalton" "coalton-raytrace") :silent t)
  (format t "~&LOAD coalton ~D ms~%" (round (- (now-ms) t0))))

(format t "~&COALTON-MODE ~A~%"
        (if (uiop:symbol-call "COALTON-IMPL/SETTINGS" "COALTON-RELEASE-P") "release" "development"))

(defvar *here* (make-pathname :name nil :type nil
                              :defaults (or *load-truename* *compile-file-truename*)))

(let* ((out (uiop:ensure-directory-pathname (env "COALTON_BENCH_OUT" "/tmp/")))
       (fasl (merge-pathnames (make-pathname :name "kernels" :type
                                             (pathname-type (compile-file-pathname "x.lisp")))
                              out))
       (t0 (now-ms)))
  (ensure-directories-exist out)
  (load (compile-file (merge-pathnames "kernels.lisp" *here*) :output-file fasl))
  (format t "~&COMPILE kernels ~D ms~%" (round (- (now-ms) t0))))

;;; The ray tracer at a reduced size: 160x90 pixels, 4 samples, depth 6.

(defun raytrace-kernel ()
  (let* ((w 160) (h 90)
         (pixels (make-array (* w h 3) :element-type 'double-float :initial-element 0d0))
         (scene (coalton:call-coalton-function
                 (find-symbol "MAKE-SCENE" "COALTON-RAYTRACE")))
         (camera (coalton:call-coalton-function
                  (find-symbol "MAKE-CAMERA" "COALTON-RAYTRACE")
                  (float (/ w h) 1d0))))
    (coalton:call-coalton-function (find-symbol "RENDER!" "COALTON-RAYTRACE")
                                   scene camera w h 4 6 12345 pixels)
    (uiop:symbol-call "COALTON-RAYTRACE/BENCHMARK" "IMAGE-CHECKSUM" pixels)))

(defvar *all-kernels*
  (append (symbol-value (find-symbol "*KERNELS*" "COALTON-BENCH"))
          (list (cons "raytrace" #'raytrace-kernel))))

(defvar *selected*
  (let ((only (env "COALTON_BENCH_ONLY" nil)))
    (if only
        (let ((names (split-commas only)))
          (remove-if-not (lambda (k) (member (car k) names :test #'string=)) *all-kernels*))
        *all-kernels*)))

(defun checksum-string (v)
  (if (floatp v)
      (let ((*read-default-float-format* 'double-float))
        (format nil "~,6E" (float v 1d0)))
      (format nil "~A" v)))

(let ((runs (parse-integer (env "COALTON_BENCH_RUNS" "5"))))
  (dolist (k *selected*)
    (let ((fn (cdr k)) (result nil) (times '()))
      (let ((t0 (now-ms)))
        (setf result (funcall fn))
        (format t "~&FIRST ~A ~,2F~%" (car k) (- (now-ms) t0)))
      (dotimes (i runs)
        (full-gc)
        (let ((t0 (now-ms)))
          (setf result (funcall fn))
          (push (- (now-ms) t0) times)))
      (setf times (nreverse times))
      (format t "~&RESULT ~A ~,2F ~A~{ ~,2F~}~%"
              (car k) (reduce #'min times) (checksum-string result) times)
      (finish-output))))

(let ((repeat (parse-integer (env "COALTON_BENCH_REPEAT" "0"))))
  (when (plusp repeat)
    (format t "~&PROFILE-START~%")
    (finish-output)
    (dotimes (i repeat)
      (dolist (k *selected*) (funcall (cdr k))))
    (format t "~&PROFILE-END~%")
    (finish-output)))

(format t "~&DONE~%")
(finish-output)
(uiop:quit 0)
