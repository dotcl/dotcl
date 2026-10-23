;;; Version 2 of the library in puswank-1.lisp: a different *VERSION*,
;;; FIND-CYCLE instead of CASIFY-CHAR, and no PUSWANK-API package.

(defpackage "PUSWANK/BACKEND" (:use "CL") (:export "SPAWN-AND-WAIT"))
(in-package "PUSWANK/BACKEND")

(defun spawn-and-wait (fn)
  (dotcl:thread-join (dotcl:make-thread fn)))

(defpackage "PUSWANK"
  (:use "CL" "PUSWANK/BACKEND")
  (:nicknames "PUSW")
  (:export "CONNECTION-INFO" "*VERSION*" "CONNECTION" "CONN-VERSION"))
(in-package "PUSWANK")

(defvar *version* "2")
(defun connection-info () (list :version *version*))
(defun find-cycle (x) x)
(defclass connection () ((version :initform *version* :reader conn-version)))

(provide "PUSWANK-TEST-MODULE")
