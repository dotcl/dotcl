;;;; Times one ASDF build of a system and of everything it depends on. Works on
;;;; dotcl and SBCL. Start the Lisp with an empty XDG_CACHE_HOME for a cold build:
;;;;
;;;;   CT_SYSTEM=ironclad XDG_CACHE_HOME=$(mktemp -d) dotcl ctime.lisp
;;;;   CT_SYSTEM=ironclad XDG_CACHE_HOME=$(mktemp -d) sbcl --non-interactive --load ctime.lisp
;;;;
;;;; Environment:
;;;;   CT_SYSTEM  the system to build
;;;;   CT_WARM    optional: a system to build first in the same process, so the
;;;;              timed build runs on a warmed-up compiler
;;;;
;;;; Output lines:
;;;;   FIND   time of ASDF:FIND-SYSTEM (includes :defsystem-depends-on builds)
;;;;   TOTAL  time of ASDF:LOAD-SYSTEM; on dotcl also the JIT time summed over
;;;;          all threads (background tier-up included) and the methods JITted
;;;;   SYS    per system: source files, compile-op ms, load-op ms (and on dotcl
;;;;          the JIT ms inside each)
;;;;   DAG    the file-level dependency graph ASDF's plan implies (each file's
;;;;          compile-op needs the load-op of these files), scheduled greedily
;;;;          on P workers with the measured compile+load time of each file:
;;;;          serial time and the makespan for P = 2, 4, 8 and unbounded
;;;;   FILE   the most expensive files
;;;; With DOTCL_PHASE_PROF=1, dotcl also prints its compile phase table.
(require "asdf")

(defpackage :ctime (:use :cl))
(in-package :ctime)

(defun now-ms () (/ (get-internal-real-time) (/ internal-time-units-per-second 1000d0)))
(defun jit-ms ()
  #+dotcl (/ (dotnet:invoke (dotnet:static "System.Runtime.JitInfo" "GetCompilationTime" nil) "TotalMilliseconds") 1d0)
  #-dotcl 0)
(defun jit-methods ()
  #+dotcl (dotnet:static "System.Runtime.JitInfo" "GetCompiledMethodCount" nil)
  #-dotcl 0)

(defvar *stats* (make-hash-table :test 'equal)) ; system -> #(compile-ms load-ms files compile-jit load-jit)
(defvar *depth* 0)
(defvar *files* (make-hash-table :test 'eq)) ; component -> #(compile-ms load-ms)
(defvar *order* nil)

(defun bucket (c)
  (let ((k (asdf:primary-system-name (asdf:component-system c))))
    (or (gethash k *stats*) (setf (gethash k *stats*) (make-array 5 :initial-element 0)))))

(defmacro timed (slot jslot c &body body)
  `(if (plusp *depth*)
       (progn ,@body)
       (let ((*depth* 1) (t0 (now-ms)) (j0 (jit-ms)) (b (bucket ,c)))
         (multiple-value-prog1 (progn ,@body)
           (incf (aref b ,slot) (- (now-ms) t0))
           (incf (aref b ,jslot) (- (jit-ms) j0))))))

(defun frec (c)
  (or (gethash c *files*) (progn (push c *order*) (setf (gethash c *files*) (make-array 2 :initial-element 0)))))

(defmethod asdf:perform :around ((o asdf:compile-op) (c asdf:cl-source-file))
  (incf (aref (bucket c) 2))
  (let ((t0 (now-ms)))
    (multiple-value-prog1 (timed 0 3 c (call-next-method))
      (incf (aref (frec c) 0) (- (now-ms) t0)))))

(defmethod asdf:perform :around ((o asdf:load-op) (c asdf:cl-source-file))
  (let ((t0 (now-ms)))
    (multiple-value-prog1 (timed 1 4 c (call-next-method))
      (incf (aref (frec c) 1) (- (now-ms) t0)))))

;;; File-level dependency DAG: nearest cl-source-file load-ops below each compile-op.
(defvar *memo* (make-hash-table :test 'equal))
(defun file-deps (o c)
  (let ((key (cons (type-of o) c)))
    (multiple-value-bind (v hit) (gethash key *memo*)
      (if hit v
          (setf (gethash key *memo*)
                (let ((acc nil))
                  (asdf::map-direct-dependencies
                   o c (lambda (do dc)
                         (if (and (typep dc 'asdf:cl-source-file) (typep do 'asdf:load-op))
                             (pushnew dc acc)
                             (unless (and (eq dc c) (eq (type-of do) (type-of o)))
                               (dolist (x (file-deps do dc)) (pushnew x acc))))))
                  acc))))))

(defun schedule (files cost deps p)
  "Greedy list scheduling of FILES (in build order) on P workers. Returns makespan."
  (let ((finish (make-hash-table :test 'eq))
        (workers (make-list p :initial-element 0d0)))
    (dolist (f files)
      (let* ((ready (reduce #'max (mapcar (lambda (d) (gethash d finish 0d0)) (funcall deps f)) :initial-value 0d0))
             (w (reduce #'min workers))
             (start (max ready w)))
        (setf (gethash f finish) (+ start (funcall cost f)))
        (setf workers (sort (cons (gethash f finish) (cdr (sort workers #'<))) #'<))))
    (reduce #'max (loop for f in files collect (gethash f finish 0d0)) :initial-value 0d0)))

(defun dag-report ()
  (let* ((files (reverse *order*))
         (cop (asdf:make-operation 'asdf:compile-op))
         (deps (lambda (f) (remove-if-not (lambda (d) (gethash d *files*)) (file-deps cop f))))
         (cost (lambda (f) (let ((v (gethash f *files*))) (+ (aref v 0) (aref v 1)))))
         (serial (reduce #'+ (mapcar cost files))))
    (format t "~&DAG files ~D serial ~D ms~{ ~A~}~%" (length files) (round serial)
            (loop for p in '(2 4 8 1000)
                  collect (format nil "p~D=~D" p (round (schedule files cost deps p)))))
    (let ((top (subseq (sort (copy-list files) #'> :key cost) 0 (min 8 (length files)))))
      (dolist (f top)
        (format t "~&FILE ~A/~A compile ~D load ~D deps ~D~%"
                (asdf:primary-system-name (asdf:component-system f)) (asdf:component-name f)
                (round (aref (gethash f *files*) 0)) (round (aref (gethash f *files*) 1))
                (length (funcall deps f)))))))

(let* ((sys (uiop:getenv "CT_SYSTEM"))
       (t0 (now-ms)))
  (let ((w (uiop:getenv "CT_WARM")))
    (when (and w (plusp (length w)))
      (handler-bind ((warning #'muffle-warning)) (asdf:load-system w))
      (clrhash *stats*) (clrhash *files*) (setf *order* nil)
      (format t "~&WARM ~A ~D ms~%" w (round (- (now-ms) t0)))
      (setf t0 (now-ms))))
  #+dotcl (dotcl:phase-reset)
  (asdf:find-system sys)
  (format t "~&FIND ~A ~D ms~%" sys (round (- (now-ms) t0)))
  (let ((t1 (now-ms)) (j1 (jit-ms)) (m1 (jit-methods)))
    (handler-bind ((warning #'muffle-warning))
      (let ((*compile-verbose* nil) (*compile-print* nil))
        (asdf:load-system sys)))
    (format t "~&IMPL ~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))
    (format t "~&TOTAL ~A ~D ms jit ~D ms methods ~D~%" sys (round (- (now-ms) t1))
            (round (- (jit-ms) j1)) (- (jit-methods) m1)))
  (let ((rows nil))
    (maphash (lambda (k v) (push (cons k v) rows)) *stats*)
    (setf rows (sort rows #'> :key (lambda (r) (+ (aref (cdr r) 0) (aref (cdr r) 1)))))
    (dolist (r rows)
      (let ((v (cdr r)))
        (format t "~&SYS ~A files ~D compile ~D load ~D cjit ~D ljit ~D~%" (car r)
                (aref v 2) (round (aref v 0)) (round (aref v 1)) (round (aref v 3)) (round (aref v 4))))))
  (handler-case (dag-report) (error (e) (format t "~&DAG error ~A~%" e)))
  #+dotcl (dotcl:phase-report)
  (finish-output))
