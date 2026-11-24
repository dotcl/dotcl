;;; Portable bench harness: the same file runs in dotcl and in SBCL.
;;;
;;; BENCH warms up for 1.5 s of wall time (the shipped runtime tiers its code up during that), then times
;;; REPS runs of BODY. It prints a human line and a machine line:
;;;   RESULT <TAB> group <TAB> name <TAB> ns/op <TAB> bytes/op
;;; The files in this directory are loaded from the repository root, e.g.
;;;   dotcl --load bench/perf/b1-closure.lisp
;;;   sbcl --load bench/perf/b1-closure.lisp
(defvar *bench-group* "bench")
(defun now-ns ()
  #+sbcl (multiple-value-bind (s us) (sb-ext:get-time-of-day) (+ (* s 1000000000) (* us 1000)))
  #+dotcl (* 1000000 (/ (dotnet:static "System.Diagnostics.Stopwatch" "GetTimestamp") 1d6)))
(defun alloc-bytes ()
  #+sbcl (sb-ext:get-bytes-consed)
  #+dotcl (dotnet:static "System.GC" "GetAllocatedBytesForCurrentThread"))
(defmacro bench (name reps &body body)
  `(progn
     (let ((t0 (now-ns)))
       (loop while (< (- (now-ns) t0) 1500000000)
             do (dotimes (i (max 1 (floor ,reps 10))) ,@body)))
     (let ((a0 (alloc-bytes)) (t0 (now-ns)))
       (dotimes (i ,reps) ,@body)
       (let ((ns (/ (float (- (now-ns) t0)) ,reps)) (by (/ (float (- (alloc-bytes) a0)) ,reps)))
         (format t "~50A ~12,2F ns/op ~10,1F B/op~%" ,name ns by)
         (format t "RESULT~C~A~C~A~C~,3F~C~,1F~%" #\Tab *bench-group* #\Tab ,name #\Tab ns #\Tab by)
         (finish-output)))))
;; Quicklisp, for the files that need a library.
(defun load-ql-system (name)
  #+dotcl (require "quicklisp")
  #+sbcl (unless (find-package :ql)
           (dolist (p '(".roswell/lisp/quicklisp/setup.lisp" "quicklisp/setup.lisp"))
             (let ((f (merge-pathnames p (user-homedir-pathname))))
               (when (and (not (find-package :ql)) (probe-file f)) (load f)))))
  (handler-bind ((warning (function muffle-warning)))
    (funcall (intern "QUICKLOAD" "QL") name :silent t)))
