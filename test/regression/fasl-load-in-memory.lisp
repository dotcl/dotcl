;;; LOAD of the fasl COMPILE-FILE just wrote builds the same module in memory
;;; instead of reading the file (off by default, forced on here
;;; with DOTCL_FASL_LOAD_IN_MEMORY). Only while the file is unchanged: a fasl
;;; replaced after COMPILE-FILE is read from the file.

(defun flm-dir ()
  (pathname (concatenate 'string (regression-temp-dir) "/flm/")))

(defun flm-write (name &rest lines)
  (let ((path (merge-pathnames name (flm-dir))))
    (ensure-directories-exist path)
    (with-open-file (f path :direction :output :if-exists :supersede)
      (dolist (l lines) (write-line l f)))
    path))

(defun flm-in-memory-count ()
  (dotnet:static "DotCL.Runtime" "FaslsLoadedInMemory"))

(defun flm-compile (name out-name &rest lines)
  (let ((src (apply #'flm-write name lines))
        (out (merge-pathnames out-name (flm-dir))))
    (compile-file src :output-file out)
    out))

(defun flm-copy-file (from to)
  (with-open-file (in from :element-type '(unsigned-byte 8))
    (with-open-file (o to :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
      (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
        (read-sequence buf in)
        (write-sequence buf o)))))

(deftest-emitting-only fasl-load-in-memory.compile-then-load
  (with-fasl-load-in-memory ("1")
    (let* ((out (flm-compile "flm-a.lisp" "flm-a.fasl"
                             "(defpackage :flm-a (:use :cl))"
                             "(in-package :flm-a)"
                             "(defun f (x) (handler-case (/ 10 x) (division-by-zero () :div0)))"
                             "(defvar *v* '(1 2 3))"))
           (n (flm-in-memory-count))
           (pkg *package*))
      (load out)
      (list (- (flm-in-memory-count) n)
            (eq pkg *package*)
            (funcall (find-symbol "F" "FLM-A") 2)
            (funcall (find-symbol "F" "FLM-A") 0)
            (symbol-value (find-symbol "*V*" "FLM-A"))
            ;; Once only: the same file again is read from the file.
            (progn (load out) (- (flm-in-memory-count) n)))))
  (1 t 5 :div0 (1 2 3) 1))

(deftest-emitting-only fasl-load-in-memory.renamed-into-place
  ;; ASDF compiles to a temporary name and renames the fasl into place.
  (with-fasl-load-in-memory ("1")
    (let* ((tmp (flm-compile "flm-b.lisp" "flm-b-tmp.fasl" "(defun flm-b-f () :b)"))
           (out (merge-pathnames "flm-b.fasl" (flm-dir)))
           (n (flm-in-memory-count)))
      (rename-file tmp out)
      (load out)
      (list (- (flm-in-memory-count) n) (funcall (find-symbol "FLM-B-F")))))
  (1 :b))

(deftest-emitting-only fasl-load-in-memory.file-replaced-after-compile
  (with-fasl-load-in-memory ("1")
    (let* ((other (flm-compile "flm-c2.lisp" "flm-c2.fasl" "(defun flm-c-f () :replaced)"))
           (out (flm-compile "flm-c.lisp" "flm-c.fasl" "(defun flm-c-f () :compiled)"))
           (n (flm-in-memory-count)))
      (flm-copy-file other out)
      (load out)
      (list (- (flm-in-memory-count) n) (funcall (find-symbol "FLM-C-F")))))
  (0 :replaced))

(defstruct flm-pt x)
(defvar *flm-mlf-calls* 0)
(defmethod make-load-form ((p flm-pt) &optional env)
  (incf *flm-mlf-calls*)
  (make-load-form-saving-slots p :environment env))

(deftest-emitting-only fasl-load-in-memory.make-load-form-once
  (with-fasl-load-in-memory ("1")
    (setf *flm-mlf-calls* 0)
    (let* ((out (flm-compile "flm-d.lisp" "flm-d.fasl"
                             "(defun flm-d-f () #.(make-flm-pt :x 7))"))
           (calls *flm-mlf-calls*)
           (n (flm-in-memory-count)))
      (load out)
      (list (- (flm-in-memory-count) n)
            (= calls *flm-mlf-calls*)
            (flm-pt-x (funcall (find-symbol "FLM-D-F"))))))
  (1 t 7))
