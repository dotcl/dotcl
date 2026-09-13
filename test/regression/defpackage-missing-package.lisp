;;; DEFPACKAGE :IMPORT-FROM / :SHADOWING-IMPORT-FROM naming a package that is
;;; not there.
;;;
;;; There are two different situations behind "the package does not exist", and
;;; they used to be treated as one:
;;;
;;;   the reader ate it   (:import-from #+sbcl #:sb-mop #:class-slots ...) has no
;;;                       live guard on dotcl, so both guarded names vanish and
;;;                       the FIRST SYMBOL of the list arrives in the package
;;;                       position. Falling back to DOTCL-MOP is what lets
;;;                       cl-mop / trivial-arguments and friends load at all.
;;;
;;;   it is just missing  a typo, or a package the user meant to load first.
;;;
;;; The fallback answered both, so the second silently imported nothing -- or,
;;; when a requested symbol name happened to exist in DOTCL-MOP, silently
;;; imported a symbol from a package nobody named. It is now keyed on whether
;;; the name in the package position is itself a DOTCL-MOP symbol, which is what
;;; distinguishes the two.

;;; --- the reader-stripped MOP shape still works ----------------------------

;;; Written as the libraries write it; the guards leave CLASS-SLOTS standing in
;;; for the package name.
(defpackage #:dmp-mop-shape
  (:use #:cl)
  (:import-from #+sbcl #:sb-mop #+ecl #:clos
                #:class-slots #:slot-definition-name))

;;; Both the stand-in name and the rest of the list come from DOTCL-MOP.
(deftest dmp-mop-shape-imports-standin
  (eq (find-symbol "CLASS-SLOTS" "DMP-MOP-SHAPE")
      (find-symbol "CLASS-SLOTS" "DOTCL-MOP"))
  t)

(deftest dmp-mop-shape-imports-rest
  (eq (find-symbol "SLOT-DEFINITION-NAME" "DMP-MOP-SHAPE")
      (find-symbol "SLOT-DEFINITION-NAME" "DOTCL-MOP"))
  t)

(defpackage #:dmp-mop-shadowing
  (:use #:cl)
  (:shadowing-import-from #+sbcl #:sb-mop #:class-slots))

(deftest dmp-mop-shadowing-shape-works
  (eq (find-symbol "CLASS-SLOTS" "DMP-MOP-SHADOWING")
      (find-symbol "CLASS-SLOTS" "DOTCL-MOP"))
  t)

;;; --- a package that is genuinely missing now says so ----------------------

(deftest dmp-missing-package-signals
  (handler-case
      (progn (eval '(defpackage #:dmp-typo
                      (:use #:cl)
                      (:import-from #:dmp-no-such-package #:anything)))
             :no-error)
    (package-error () :package-error)
    (error () :other-error))
  :package-error)

;;; The dangerous one: a requested symbol name that DOES exist in DOTCL-BOP.
;;; This used to import DOTCL-MOP:CLASS-SLOTS even though the user named a
;;; completely different package.
(deftest dmp-missing-package-does-not-borrow-from-mop
  (handler-case
      (progn (eval '(defpackage #:dmp-borrow
                      (:use #:cl)
                      (:import-from #:dmp-no-such-package #:class-slots)))
             :no-error)
    (package-error () :package-error)
    (error () :other-error))
  :package-error)

(deftest dmp-missing-shadowing-package-signals
  (handler-case
      (progn (eval '(defpackage #:dmp-typo2
                      (:use #:cl)
                      (:shadowing-import-from #:dmp-no-such-package #:anything)))
             :no-error)
    (package-error () :package-error)
    (error () :other-error))
  :package-error)

;;; The error is continuable, so a caller that would rather go on still can.
(deftest dmp-missing-package-is-continuable
  (handler-bind ((package-error (lambda (c)
                                  (let ((r (find-restart 'continue c)))
                                    (when r (invoke-restart r))))))
    (eval '(defpackage #:dmp-continued
             (:use #:cl)
             (:import-from #:dmp-no-such-package #:anything)))
    (and (find-package "DMP-CONTINUED")
         (null (find-symbol "ANYTHING" "DMP-CONTINUED"))))
  t)
