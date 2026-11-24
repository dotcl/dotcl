;;; A function loaded from a FASL has its lambda-list parameter names interned in
;;; the package it is loaded into, as when the source is read (SBCL does the same
;;; from the debug info). A library may IMPORT-FROM a name that only appears as a
;;; parameter of another library's function. Names of LET variables are not
;;; interned.

(defun %flni-load-fresh (stem)
  "Compile a file defining package FLNI-<stem> with a function whose parameters
are CTYPE, OPT, SUPP, REST, KEYVAR, KWSUPP and a LET variable LOCALVAR; delete
the package and load the FASL. Returns the package."
  (let* ((pkg (format nil "FLNI-~a" stem))
         (dir (regression-temp-dir/))
         (src (format nil "~aflni-~a.lisp" dir stem))
         (fasl (format nil "~aflni-~a.fasl" dir stem)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (format s "(defpackage ~a (:use :cl) (:export #:f))~%(in-package ~a)~%" pkg pkg)
      (write-string "(defun f (ctype &optional (opt 1 supp) &rest rest &key ((:k keyvar) 2 kwsupp))
  (declare (ignorable opt supp rest keyvar kwsupp))
  (let ((localvar 1)) (declare (ignorable localvar)) (list ctype)))
" s))
    (compile-file src :output-file fasl)
    (delete-package pkg)
    (load fasl)
    (find-package pkg)))

(defun %flni-state (stem names)
  (let ((p (%flni-load-fresh stem)))
    (mapcar (lambda (n) (and (find-symbol n p) t)) names)))

(deftest-emitting-only fasl-lambda-list-names-interned.parameters
  (%flni-state "A" '("CTYPE" "OPT" "SUPP" "REST" "KEYVAR" "KWSUPP"))
  (t t t t t t))

(deftest-emitting-only fasl-lambda-list-names-interned.let-variable-not-interned
  (%flni-state "B" '("LOCALVAR"))
  (nil))

(deftest-emitting-only fasl-lambda-list-names-interned.import-from
  (progn
    (%flni-load-fresh "C")
    (let ((p (make-package "FLNI-IMPORTER" :use '(:cl))))
      (unwind-protect
           (progn (import (find-symbol "CTYPE" "FLNI-C") p)
                  (package-name (symbol-package (find-symbol "CTYPE" p))))
        (delete-package p))))
  "FLNI-C")
