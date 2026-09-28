;;; PRINT-UNREADABLE-OBJECT :TYPE T prints the type name the way WRITE would
;;; under the current *PRINT-ESCAPE*: a package prefix when the symbol is not
;;; accessible in *PACKAGE*, |escapes| when needed, and bare under PRINC.
;;; Expected strings are SBCL's.

(defpackage :%puo-pkg (:use :cl))
(defclass %puo-pkg::trial () ())
(defclass %puo-pkg::|weird name| () ())
(defmethod print-object ((x %puo-pkg::trial) s)
  (print-unreadable-object (x s :type t) (princ "hi" s)))
(defmethod print-object ((x %puo-pkg::|weird name|) s)
  (print-unreadable-object (x s :type t) (princ "hi" s)))

(deftest print-unreadable-type-name.qualified
  (let ((*package* (find-package :cl-user)))
    (prin1-to-string (make-instance '%puo-pkg::trial)))
  "#<%PUO-PKG::TRIAL hi>")

(deftest print-unreadable-type-name.princ
  (let ((*package* (find-package :cl-user)))
    (princ-to-string (make-instance '%puo-pkg::trial)))
  "#<TRIAL hi>")

(deftest print-unreadable-type-name.accessible
  (let ((*package* (find-package :%puo-pkg)))
    (prin1-to-string (make-instance '%puo-pkg::trial)))
  "#<TRIAL hi>")

(deftest print-unreadable-type-name.escaped
  (let ((*package* (find-package :cl-user)))
    (list (prin1-to-string (make-instance '%puo-pkg::|weird name|))
          (princ-to-string (make-instance '%puo-pkg::|weird name|))))
  ("#<%PUO-PKG::|weird name| hi>" "#<weird name hi>"))

;; *PRINT-LENGTH* / *PRINT-LEVEL* do not truncate a compound type name.
(deftest print-unreadable-type-name.no-truncation
  (let ((*print-length* 0) (*print-level* 0) (*package* (find-package :cl-user)))
    (with-output-to-string (s)
      (print-unreadable-object ((make-array '(2 2)) s :type t))))
  "#<(SIMPLE-ARRAY T (2 2)) >")
