;;; compile-file marks the top level helpers and the module initializer as
;;; NoOptimization when nothing in them can run twice (no backward branch), so
;;; they are not compiled with full optimization at load. A helper holding a
;;; loop keeps the normal JIT.

(defun %from-array-list (a)
  (loop for i below (dotnet:invoke a "get_Length")
        collect (dotnet:invoke a "GetValue" i)))

;; ((name . no-optimization-p) ...) for _toplevel_* and ModuleInit
(defun %from-methods (fasl)
  (let ((asm (dotnet:static "System.Reflection.Assembly" "LoadFile"
                            (namestring (truename fasl))))
        (out '()))
    (dolist (ty (%from-array-list (dotnet:invoke asm "GetTypes")) (nreverse out))
      (dolist (m (%from-array-list (dotnet:invoke ty "GetMethods")))
        (let ((nm (dotnet:invoke m "get_Name")))
          (when (or (string= nm "ModuleInit")
                    (and (> (length nm) 10) (string= "_toplevel_" nm :end2 10)))
            (push (cons nm (and (search "NoOptimization"
                                        (dotnet:invoke (dotnet:invoke m "GetMethodImplementationFlags")
                                                       "ToString"))
                                t))
                  out)))))))

(defun %from-compile (name text)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string text s))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    fasl))

(deftest-emitting-only fasl-run-once-minopts.straight-line
  (let* ((fasl (%from-compile "from-a"
                              "(defpackage :from-p (:use :cl))
(in-package :from-p)
(defvar *log* (list :start))
(defun f1 () :f1)
(push (f1) *log*)
(defparameter *v* (vector 1 2 3))
(push (length *v*) *log*)
"))
         (ms (%from-methods fasl)))
    (load fasl)
    (list (reverse (symbol-value (find-symbol "*LOG*" "FROM-P")))
          (and (assoc "ModuleInit" ms :test #'string=) t)
          (and (find-if (lambda (e) (string/= (car e) "ModuleInit")) ms) t)
          (every #'cdr ms)))
  ((:start :f1 3) t t t))

(deftest-emitting-only fasl-run-once-minopts.loop
  (let* ((fasl (%from-compile "from-b"
                              "(defpackage :from-q (:use :cl))
(in-package :from-q)
(defvar *n* (let ((n 0)) (dotimes (i 1000) (incf n i)) n))
"))
         (ms (%from-methods fasl)))
    (load fasl)
    (list (symbol-value (find-symbol "*N*" "FROM-Q"))
          (cdr (assoc "ModuleInit" ms :test #'string=))
          (and (find-if (lambda (e) (and (string/= (car e) "ModuleInit") (not (cdr e)))) ms) t)))
  (499500 t t))
