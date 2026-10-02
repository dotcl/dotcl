;;; compile-file puts consecutive top level forms into one helper method
;;; instead of one method each (each is JIT-compiled at load to run once). The
;;; forms must still run in file order, interleaved correctly with what goes
;;; straight into the module initializer (function registrations), and a form
;;; with a loop, a non-local exit or a condition handler must behave as before.

(defun %cftm-array-list (a)
  (loop for i below (dotnet:invoke a "get_Length")
        collect (dotnet:invoke a "GetValue" i)))

(defun %cftm-count-methods (fasl prefix)
  (let ((asm (dotnet:static "System.Reflection.Assembly" "LoadFile"
                            (namestring (truename fasl))))
        (n 0))
    (dolist (ty (%cftm-array-list (dotnet:invoke asm "GetTypes")) n)
      (dolist (m (%cftm-array-list (dotnet:invoke ty "GetMethods")))
        (let ((nm (dotnet:invoke m "get_Name")))
          (when (and (>= (length nm) (length prefix))
                     (string= prefix nm :end2 (length prefix)))
            (incf n)))))))

(defun %cftm-compile (name text)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string text s))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    fasl))

(deftest-compiled-only cf-toplevel-merged.order-and-effects
  (let ((fasl (%cftm-compile "cftm-a"
                              "(defpackage :cftm-p (:use :cl))
(in-package :cftm-p)
(defvar *log* '())
(push :start *log*)
(defun f1 () :f1)
(push (f1) *log*)
(defun f2 (x) (declare (ignore x)) (f1))
(push (f2 0) *log*)
(push (let ((n 0)) (dotimes (i 3) (incf n)) n) *log*)
(push (block b (dolist (x '(1 2 3)) (when (= x 2) (return-from b x)))) *log*)
(push (handler-case (error \"boom\") (error () :handled)) *log*)
(push (catch 'tag (throw 'tag :thrown)) *log*)
(values 1 2 3)
(push (multiple-value-list (values :a :b)) *log*)
(defparameter *later* (f2 1))
(push *later* *log*)
(push :end *log*)
")))
    (load fasl)
    (let ((p (find-package "CFTM-P")))
      (list (reverse (symbol-value (find-symbol "*LOG*" p)))
            ;; Far fewer helpers than top level forms (there are 16).
            (<= (%cftm-count-methods fasl "_toplevel_") 6))))
  ((:start :f1 :f1 3 2 :handled :thrown (:a :b) :f1 :end) t))

(deftest-compiled-only cf-toplevel-merged.error-mid-file
  ;; An error in a merged form stops the load there: forms before it ran,
  ;; forms after it did not.
  (let ((fasl (%cftm-compile "cftm-b"
                              "(defpackage :cftm-q (:use :cl))
(in-package :cftm-q)
(defvar *log* '())
(push 1 *log*)
(push 2 *log*)
(error \"stop here\")
(push 3 *log*)
")))
    (list (handler-case (progn (load fasl) :loaded)
            (error () :error))
          (reverse (symbol-value (find-symbol "*LOG*" "CFTM-Q")))))
  (:error (1 2)))
