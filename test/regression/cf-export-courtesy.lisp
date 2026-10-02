;;; COMPILE-FILE evaluates a top-level EXPORT (and IMPORT, SHADOW,
;;; SHADOWING-IMPORT, USE-PACKAGE) early, although CLHS gives it no
;;; compile-time effect. antik does
;;;   (defvar *row-separator* '^)
;;;   (export *row-separator*)
;;; and DEFVAR gives the variable no value until load, so the early evaluation
;;; signalled "Unbound variable" and COMPILE-FILE failed. Such an error is not
;;; the file's error: it is dropped, and the form still runs at load.

(defpackage :cf-export-courtesy (:use :cl))

(deftest-compiled-only cf-export-courtesy.unbound-argument
  (let ((src "cfexp-tmp.lisp")
        (fasl "cfexp-tmp.fasl"))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (write-line "(in-package :cf-export-courtesy)" s)
             (write-line "(defvar *cfexp-sep* 'cfexp-caret)" s)
             (write-line "(export *cfexp-sep*)" s))
           (let ((compiled (handler-case (and (compile-file src :output-file fasl) :compiled)
                             (error (e) (list :error (princ-to-string e))))))
             (load fasl)
             (list compiled
                   (nth-value 1 (find-symbol "CFEXP-CARET" :cf-export-courtesy)))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  (:compiled :external))

(deftest-compiled-only cf-export-courtesy.early-export-still-happens
  ;; The convenience itself is kept: a symbol exported at top level is external
  ;; while the rest of the file is compiled.
  (let ((src "cfexp-tmp2.lisp")
        (fasl "cfexp-tmp2.fasl"))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (write-line "(in-package :cf-export-courtesy)" s)
             (write-line "(export 'cfexp-early)" s)
             (write-line "(defmacro cfexp-status () (list 'quote (nth-value 1 (find-symbol \"CFEXP-EARLY\" :cf-export-courtesy))))" s)
             (write-line "(defparameter *cfexp-status* (cfexp-status))" s))
           (compile-file src :output-file fasl)
           (load fasl)
           (symbol-value (find-symbol "*CFEXP-STATUS*" :cf-export-courtesy)))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  :external)

(deftest-compiled-only cf-export-courtesy.not-inside-load-only-eval-when
  ;; Inside (eval-when (:load-toplevel :execute) ...) the file asked for no
  ;; compile-time evaluation, so the early EXPORT is not done there: after
  ;; COMPILE-FILE the symbol is still internal, after LOAD it is external.
  ;; hu.dwim.stefil swaps a symbol this way (UNEXPORT + SHADOWING-IMPORT +
  ;; EXPORT) and doing half of it at compile time broke the load.
  (let ((src "cfexp-tmp3.lisp")
        (fasl "cfexp-tmp3.fasl"))
    (unwind-protect
         (progn
           (with-open-file (s src :direction :output :if-exists :supersede)
             (write-line "(in-package :cf-export-courtesy)" s)
             (write-line "(eval-when (:load-toplevel :execute) (export (intern \"CFEXP-LATE\" :cf-export-courtesy)))" s))
           (intern "CFEXP-LATE" :cf-export-courtesy)
           (compile-file src :output-file fasl)
           (let ((after-compile (nth-value 1 (find-symbol "CFEXP-LATE" :cf-export-courtesy))))
             (load fasl)
             (list after-compile
                   (nth-value 1 (find-symbol "CFEXP-LATE" :cf-export-courtesy)))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file fasl))))
  (:internal :external))
