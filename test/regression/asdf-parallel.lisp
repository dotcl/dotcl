;;; contrib/asdf-parallel: a small system whose files form a dependency graph
;;; (two to three of them can compile at once) is built once serially and once
;;; with four workers, each copy under its own package, and the two give the
;;; same results. The files use what a parallel build can get wrong: a DEFVAR
;;; from a file compiled on another thread that a dependent file binds, a
;;; struct and an inline function from a dependency, a macro whose expander
;;; calls a function defined at compile time, the same MACROLET name with two
;;; meanings in files compiled at the same time, and LOAD-TIME-VALUE.

(defparameter *apar-files*
  '(("package" ()
     "(defpackage :~a (:use :cl) (:export #:run))")
    ("vars" ("package")
     "(in-package :~a)
      (defvar *depth* 0)
      (defun depth () *depth*)")
    ("shapes" ("package")
     "(in-package :~a)
      (defstruct pt (x 0) (y 0))
      (declaim (inline norm1))
      (defun norm1 (p) (+ (abs (pt-x p)) (abs (pt-y p))))
      (defun shapes-step () (macrolet ((stp (a) `(* 10 ,a))) (stp 4)))")
    ("gen" ("package")
     "(in-package :~a)
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (defun squares-of (n) (loop for i below n collect (* i i))))
      (defmacro squares (n) `',(squares-of n))
      (defun gen-step () (macrolet ((stp (a b) `(list ,a ,b))) (stp 1 2)))")
    ("use1" ("vars" "shapes")
     "(in-package :~a)
      (defun use1 (n) (let ((*depth* n)) (list (depth) (norm1 (make-pt :x n :y (- n))))))")
    ("use2" ("gen")
     "(in-package :~a)
      (defun use2 () (list (squares 5) (load-time-value (length (squares-of 7)))))")
    ("top" ("use1" "use2")
     "(in-package :~a)
      (defun run () (list (use1 3) (use2) (shapes-step) (gen-step)))")))

(defun %apar-build (pkg jobs)
  "Write the system for package PKG and build it with JOBS workers. Returns
   what RUN returns and the driver's statistics."
  (require "asdf-parallel")
  (let* ((name (string-downcase pkg))
         (dir (format nil "~a/dotcl-apar-~a-~a/" (regression-temp-dir) name
                      (get-internal-real-time))))
    (ensure-directories-exist dir)
    (with-open-file (s (concatenate 'string dir name ".asd")
                       :direction :output :if-exists :supersede)
      (format s "(defsystem ~s :components (~{~a~^ ~}))" name
              (loop for (file deps) in *apar-files*
                    collect (format nil "(:file ~s :depends-on ~s)" file deps))))
    (loop for (file nil text) in *apar-files*
          do (with-open-file (s (concatenate 'string dir file ".lisp")
                                :direction :output :if-exists :supersede)
               (format s text name)))
    (let ((reg (find-symbol "*CENTRAL-REGISTRY*" "ASDF")))
      (push dir (symbol-value reg))
      (unwind-protect
           (progn
             (funcall (find-symbol "LOAD-SYSTEM" "ASDF-PARALLEL") name :jobs jobs :force t)
             (list (funcall (find-symbol "RUN" pkg))
                   (symbol-value (find-symbol "*STATISTICS*" "ASDF-PARALLEL"))))
        (setf (symbol-value reg) (remove dir (symbol-value reg) :test #'equal))
        (funcall (find-symbol "CLEAR-SYSTEM" "ASDF") name)))))

(deftest asdf-parallel.same-results
  (let ((serial (%apar-build "APAR-SERIAL" 1))
        (parallel (%apar-build "APAR-PARALLEL" 4)))
    (list (first serial) (equal (first serial) (first parallel))))
  (((3 6) ((0 1 4 9 16) 7) 40 (1 2)) t))

;;; An emit-free build loads the sources, so no compile-op reaches a worker.
(deftest-emitting-only asdf-parallel.compiled-on-workers
  (let ((stats (second (%apar-build "APAR-STATS" 4))))
    (list (getf stats :parallel)
          (>= (getf stats :peak) 2)
          (getf stats :retried)))
  (7 t nil))
