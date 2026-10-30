;;; What a function loaded from a FASL reports about its name and parameters.
;;;
;;; FUNCTION-LAMBDA-EXPRESSION's third value is the symbol the function was
;;; defined under, not a symbol of the same name interned into an internal
;;; package. DOTCL:FUNCTION-LAMBDA-LIST gives the parameter names as the source
;;; wrote them (symbols of the defining package), where the FASL carries them
;;; uninterned. moptilities and millet compare these with EQUAL.
;;;
;;; COMPILE-FILE needs the emitter, so the FASL cases skip the emit-free build.

(defun %fnf-compile-and-load ()
  (let ((src (regression-temp-file "fnf-src.lisp")))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string "(defpackage :fnf-pkg (:use :cl))
(in-package :fnf-pkg)
(defun fnf-a (arg) arg)
(defun fnf-b (a b &optional c) (list a b c))
(defun fnf-c (&key ((:k kv) 1 kv-p)) (list kv kv-p))
(defun (setf fnf-d) (new x) (list new x))
" s))
    (load (compile-file src :output-file (regression-temp-file "fnf-src.fasl")))
    t))

(deftest-emitting-only fnf-load
  (%fnf-compile-and-load)
  t)

(deftest-emitting-only fnf-fle-name
  (eq (nth-value 2 (function-lambda-expression
                    (fdefinition (find-symbol "FNF-A" "FNF-PKG"))))
      (find-symbol "FNF-A" "FNF-PKG"))
  t)

(deftest-emitting-only fnf-fle-setf-name
  (let ((s (find-symbol "FNF-D" "FNF-PKG")))
    (equal (nth-value 2 (function-lambda-expression (fdefinition (list 'setf s))))
           (list 'setf s)))
  t)

(deftest fnf-fle-name-local-defun
  (progn
    (defun fnf-local (x) x)
    (eq (nth-value 2 (function-lambda-expression #'fnf-local)) 'fnf-local))
  t)

(deftest-emitting-only fnf-fle-name-interns-nothing
  (let* ((name (symbol-name (gensym "FNF-FRESH-")))
         (sym (intern name "FNF-PKG")))
    (eval `(defun ,sym () nil))
    (let ((fn (fdefinition sym)))
      (unintern sym "FNF-PKG")
      (function-lambda-expression fn)
      (nth-value 1 (find-symbol name "DOTCL-INTERNAL"))))
  nil)

(deftest-emitting-only fnf-lambda-list-required-optional
  (let ((pkg (find-package "FNF-PKG")))
    (equal (dotcl:function-lambda-list (find-symbol "FNF-B" pkg))
           (list (find-symbol "A" pkg) (find-symbol "B" pkg) '&optional (find-symbol "C" pkg))))
  t)

(deftest-emitting-only fnf-lambda-list-key-spec
  (let* ((pkg (find-package "FNF-PKG"))
         (ll (dotcl:function-lambda-list (find-symbol "FNF-C" pkg))))
    (list (first ll)
          (eq (second (first (second ll))) (find-symbol "KV" pkg))
          (eq (third (second ll)) (find-symbol "KV-P" pkg))))
  (&key t t))

;;; Loaded into a package the compiling process never saw: the package is
;;; deleted between COMPILE-FILE and LOAD, as a fresh process (or ASDF reading
;;; its cached FASL) would see it. The parameter names still come back as the
;;; source's symbols, not uninterned ones.

(defun %fnf-fresh-load ()
  (let ((src (regression-temp-file "fnf-fresh.lisp")))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-string "(defpackage :fnf-fresh (:use :cl))
(in-package :fnf-fresh)
(defun fresh-fact (fresh-n) (if (<= fresh-n 1) 1 (* fresh-n (fresh-fact (1- fresh-n)))))
(defun fresh-opt (fresh-a &optional (fresh-b 2 fresh-b-p) &rest fresh-r &key ((:k fresh-c) 3))
  (list fresh-a fresh-b fresh-b-p fresh-r fresh-c))
(defparameter *fresh-lambda* (lambda (fresh-x &optional (fresh-y 1)) (list fresh-x fresh-y)))
" s))
    (let ((fasl (compile-file src :output-file (regression-temp-file "fnf-fresh.fasl"))))
      (delete-package "FNF-FRESH")
      (load fasl)
      t)))

(deftest-emitting-only fnf-fresh-load
  (%fnf-fresh-load)
  t)

(defun %fnf-fresh-sym (name) (find-symbol name "FNF-FRESH"))

(deftest-emitting-only fnf-fresh-lambda-list-required
  (let ((ll (dotcl:function-lambda-list (%fnf-fresh-sym "FRESH-FACT"))))
    (list (length ll)
          (eq (first ll) (%fnf-fresh-sym "FRESH-N"))
          (eq (symbol-package (first ll)) (find-package "FNF-FRESH"))))
  (1 t t))

(deftest-emitting-only fnf-fresh-lambda-list-full
  (equal (dotcl:function-lambda-list (%fnf-fresh-sym "FRESH-OPT"))
         (list (%fnf-fresh-sym "FRESH-A")
               '&optional (list (%fnf-fresh-sym "FRESH-B") 2 (%fnf-fresh-sym "FRESH-B-P"))
               '&rest (%fnf-fresh-sym "FRESH-R")
               '&key (list (list :k (%fnf-fresh-sym "FRESH-C")) 3)))
  t)

(deftest-emitting-only fnf-fresh-lambda-list-anonymous
  (equal (dotcl:function-lambda-list (symbol-value (%fnf-fresh-sym "*FRESH-LAMBDA*")))
         (list (%fnf-fresh-sym "FRESH-X") '&optional (list (%fnf-fresh-sym "FRESH-Y") 1)))
  t)
