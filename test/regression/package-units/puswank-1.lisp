;;; Version 1 of a small library that package-units.lisp loads several times
;;; under the same package names.

(defpackage "PUSWANK/BACKEND" (:use "CL") (:export "SPAWN-AND-WAIT"))
(in-package "PUSWANK/BACKEND")

(defun spawn-and-wait (fn)
  (dotcl:thread-join (dotcl:make-thread fn)))

(defpackage "PUSWANK-API" (:use "CL"))

(defpackage "PUSWANK"
  (:use "CL" "PUSWANK/BACKEND")
  (:nicknames "PUSW")
  (:export "CONNECTION-INFO" "*VERSION*" "CONNECTION" "CONN-VERSION"))
(in-package "PUSWANK")

(defvar *version* "1")
(defun connection-info () (list :version *version*))
(defun casify-char (c) (char-upcase c))
(defclass connection () ((version :initform *version* :reader conn-version)))

(provide "PUSWANK-TEST-MODULE")
