;;; Regression tests for the per-call-site function-symbol cache in FASL code.
;;;
;;; The JIT path got one call-site cell per :load-sym-fn site, rooted in the
;;; constant pool. A .fasl has no constant pool to root anything in, so
;;; compile-file'd code kept resolving its callees through Startup.SymFn on
;;; every call — a string concatenation and a dictionary probe each time,
;;; measured at ~58 ns per call more than the same code loaded from source.
;;; The cell now lives in a static field of the generated type, filled by that
;;; type's initializer (so the CLR guarantees it exists before any body runs).
;;;
;;; Contract is the same as the JIT path: resolution still happens at RUNTIME on
;;; first execution, a miss is not pinned, and redefinition is always observed.
;;; What is specific here is that all of it has to survive the compile-file /
;;; load round trip.

;; Scratch directory for the generated .lisp/.fasl pairs — outside the tree, so
;; a run never leaves build products in the repository (and a loaded .fasl is a
;; loaded assembly, which Windows will not let the test delete afterwards).
;;
;; Kept as a NAMESTRING and never merged: this file runs after ~60 others in the
;; suite, and MERGE-PATHNAMES against whatever *DEFAULT-PATHNAME-DEFAULTS* they
;; leave behind is not something worth depending on here.
(defvar *fscs-dir*
  (let ((dir (concatenate 'string
                          (substitute #\/ #\\ (or (dotcl:getenv "TMPDIR")
                                                  (dotcl:getenv "TEMP")
                                                  "/tmp"))
                          "/dotcl-fscs-test/")))
    (ensure-directories-exist dir)
    dir))

(defun %fscs-compile-and-load (source-forms name)
  "Write SOURCE-FORMS to a scratch .lisp, compile-file it, load the .fasl, and
   return T. Everything the test asserts then runs out of a real compiled
   assembly, not out of the source."
  (let ((lisp (concatenate 'string *fscs-dir* name ".lisp")))
    (with-open-file (s lisp :direction :output :if-exists :supersede)
      (dolist (f source-forms) (write f :stream s :readably t) (terpri s)))
    (let ((fasl (compile-file lisp)))
      (load fasl)
      t)))

;;; ---- a forward reference is resolved, not pinned as a miss ----

;; %FSCS-LATE does not exist when this unit is compiled, and the call runs (and
;; fails) before it is defined. If the failure were cached in the site's cell,
;; the later definition would never be seen.
(deftest fscs-forward-reference
  (progn
    (%fscs-compile-and-load
     '((in-package :cl-user)
       (defun %fscs-caller () (%fscs-late 3)))
     "fscs-fwd")
    (let ((before (handler-case (funcall (intern "%FSCS-CALLER"))
                    (undefined-function () :undefined))))
      (eval '(defun %fscs-late (n) (* n 10)))
      (list before (funcall (intern "%FSCS-CALLER")))))
  (:undefined 30))

;;; ---- redefinition is observed through a compiled call site ----

(deftest fscs-redefinition
  (progn
    (%fscs-compile-and-load
     '((in-package :cl-user)
       (defun %fscs-target () :v1)
       (defun %fscs-site () (%fscs-target)))
     "fscs-redef")
    (let ((first (funcall (intern "%FSCS-SITE"))))
      (eval '(defun %fscs-target () :v2))
      (list first (funcall (intern "%FSCS-SITE")))))
  (:v1 :v2))

;; fmakunbound goes back to undefined and a later definition is picked up again.
(deftest fscs-fmakunbound-round-trip
  (progn
    (%fscs-compile-and-load
     '((in-package :cl-user)
       (defun %fscs-live () :alive)
       (defun %fscs-live-site () (%fscs-live)))
     "fscs-unbind")
    (let ((a (funcall (intern "%FSCS-LIVE-SITE"))))
      (fmakunbound (intern "%FSCS-LIVE"))
      (let ((b (handler-case (funcall (intern "%FSCS-LIVE-SITE"))
                 (undefined-function () :undefined))))
        (eval '(defun %fscs-live () :again))
        (list a b (funcall (intern "%FSCS-LIVE-SITE"))))))
  (:alive :undefined :again))

;;; ---- same name in two packages keeps its own resolution ----

(deftest fscs-per-package
  (progn
    (%fscs-compile-and-load
     '((defpackage :fscs-a (:use :cl) (:export #:who #:ask))
       (defpackage :fscs-b (:use :cl) (:export #:who #:ask))
       (in-package :fscs-a)
       (defun who () :a)
       (defun ask () (who))
       (in-package :fscs-b)
       (defun who () :b)
       (defun ask () (who)))
     "fscs-pkg")
    (list (funcall (intern "ASK" :fscs-a)) (funcall (intern "ASK" :fscs-b))))
  (:a :b))

;;; ---- (setf name), closures and mutual recursion round-trip ----

(deftest fscs-setf-and-closure
  (progn
    (%fscs-compile-and-load
     '((in-package :cl-user)
       (defvar *fscs-place* nil)
       (defun (setf %fscs-slot) (v) (setq *fscs-place* v))
       (defun %fscs-setf-site (v) (setf (%fscs-slot) v))
       (defun %fscs-helper (x) (* x 3))
       (defun %fscs-closure-site (x) (funcall (lambda (y) (%fscs-helper y)) x)))
     "fscs-misc")
    (funcall (intern "%FSCS-SETF-SITE") 9)
    (list (symbol-value (intern "*FSCS-PLACE*"))
          (funcall (intern "%FSCS-CLOSURE-SITE") 4)))
  (9 12))

(deftest fscs-mutual-recursion
  (progn
    (%fscs-compile-and-load
     '((in-package :cl-user)
       (defun %fscs-even (n) (if (= n 0) t (%fscs-odd (- n 1))))
       (defun %fscs-odd (n) (if (= n 0) nil (%fscs-even (- n 1)))))
     "fscs-mutual")
    (list (funcall (intern "%FSCS-EVEN") 10)
          (funcall (intern "%FSCS-ODD") 10)))
  (t nil))

;; Two fasls compiled separately: the callee lives in the first, the call site in
;; the second, so the site's cell has to resolve across compilation units.
(deftest fscs-cross-unit
  (progn
    (%fscs-compile-and-load
     '((in-package :cl-user)
       (defun %fscs-unit-a (x) (+ x 100)))
     "fscs-unit-a")
    (%fscs-compile-and-load
     '((in-package :cl-user)
       (defun %fscs-unit-b (x) (%fscs-unit-a x)))
     "fscs-unit-b")
    (funcall (intern "%FSCS-UNIT-B") 5))
  105)
