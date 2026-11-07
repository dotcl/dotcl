;;; A LOAD-TIME-VALUE form in a function that a source file defines is evaluated
;;; when the file is loaded, once, as EVAL and COMPILE do (SBCL and ECL do the
;;; same); it used to wait for the first call of the function.

(defvar *ltvs-log* nil)

(defun %ltvs-load (name text)
  (let ((path (regression-temp-file name)))
    (with-open-file (o path :direction :output :if-exists :supersede) (write-string text o))
    (load path)))

(deftest-compiled-only load-time-value-source-load.evaluated-at-load
  (progn
    (setq *ltvs-log* nil)
    (%ltvs-load "ltvs-a.lisp"
                "(defun ltvs-f () (load-time-value (progn (push :evaluated *ltvs-log*) 7)))
(defparameter *ltvs-seen* (copy-list *ltvs-log*))")
    (list (symbol-value (find-symbol "*LTVS-SEEN*" "CL-USER"))
          (funcall (find-symbol "LTVS-F" "CL-USER"))
          (funcall (find-symbol "LTVS-F" "CL-USER"))
          *ltvs-log*))
  ((:evaluated) 7 7 (:evaluated)))

(deftest load-time-value-source-load.in-let-and-flet
  (progn
    (%ltvs-load "ltvs-b.lisp"
                "(defun ltvs-g (x) (flet ((h (y) (+ y (load-time-value (+ 1 2))))) (h x)))")
    (funcall (find-symbol "LTVS-G" "CL-USER") 10))
  13)
