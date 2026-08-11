;;; DEFPACKAGE's :DOCUMENTATION option must reach the package object.
;;; SBCL's genesis copies each target package's docstring from the host package
;;; of the same name, so dropping it here loses package documentation in an
;;; image cross-built on dotcl.

(defpackage "DEFPACKAGE-DOC-PROBE"
  (:use "CL")
  (:documentation "probe docstring"))

(deftest defpackage-documentation-option
  (documentation (find-package "DEFPACKAGE-DOC-PROBE") t)
  "probe docstring")

;; Still settable afterwards, and the setter wins.
(deftest defpackage-documentation-setf
  (progn (setf (documentation (find-package "DEFPACKAGE-DOC-PROBE") t) "replaced")
         (documentation (find-package "DEFPACKAGE-DOC-PROBE") t))
  "replaced")

;; No :documentation option — NIL, not an error.
(defpackage "DEFPACKAGE-DOC-PROBE-2" (:use "CL"))

(deftest defpackage-documentation-absent
  (documentation (find-package "DEFPACKAGE-DOC-PROBE-2") t)
  nil)
