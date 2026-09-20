;;; #. whose form returns NO values reads no object at all.
;;;
;;; CLHS 2.2: a reader macro function that returns zero values makes the reader
;;; start over, exactly as for a comment. #. passes through whatever its form
;;; evaluated to, so (values) there means "contribute nothing".
;;;
;;; dotcl took the primary value instead, which is NIL for (values), so a form
;;; that meant nothing contributed a NIL to whichever list it sat in. metacopy
;;; writes its DEFPACKAGE that way:
;;;
;;;     (:use #:common-lisp #:moptilities #:metacopy-system
;;;           #.(if *load-with-contextl* '#:contextl (values)))
;;;
;;; and the NIL became a package designator: "USE-PACKAGE: no package named NIL".

(deftest read-time-eval-no-values-contributes-nothing
  (values (read-from-string "(a #.(values) b)"))
  (a b))

(deftest read-time-eval-no-values-in-several-positions
  (values (read-from-string "(#.(values) a #.(values) #.(values) b #.(values))"))
  (a b))

;;; Standing alone there is nothing left to read, so the read hits end of input
;;; rather than producing NIL.

(deftest read-time-eval-no-values-alone-is-end-of-input
  (handler-case (progn (read-from-string "#.(values)") :no-error)
    (end-of-file () :end-of-file)
    (error () :other-error))
  :end-of-file)

;;; One value still contributes that value, and several contribute the primary.

(deftest read-time-eval-one-value-contributes-it
  (values (read-from-string "(a #.'x b)"))
  (a x b))

(deftest read-time-eval-several-values-contributes-the-primary
  (values (read-from-string "(a #.(values 'p 'q) b)"))
  (a p b))

(deftest read-time-eval-nil-value-is-still-a-nil
  (values (read-from-string "(a #.nil b)"))
  (a nil b))

;;; The shape metacopy actually uses: a conditional that either names a package
;;; or contributes nothing, inside a DEFPACKAGE option.

(defvar *rtenv-want-extra* nil)

(deftest read-time-eval-conditional-package-designator
  (progn
    (eval (read-from-string
           "(defpackage :rtenv-pkg
              (:use #:common-lisp
                    #.(if *rtenv-want-extra* '#:keyword (values))))"))
    (mapcar #'package-name (package-use-list (find-package :rtenv-pkg))))
  ("COMMON-LISP"))
