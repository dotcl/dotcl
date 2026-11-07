;;; EXPORT of a symbol whose name is already taken in the package by another symbol
;;; -- the package's own, or one it inherits -- is a name conflict (CLHS 11.1.1.2.5):
;;; a PACKAGE-ERROR with a CONTINUE restart, and the package's symbol stays. It used to
;;; replace that symbol silently. Exports that are not conflicts are unchanged.

(defpackage :enc-src (:use) (:export "X" "Z"))
(defpackage :enc-base (:use) (:export "W"))
(defpackage :enc-own (:use) (:intern "X"))
(defpackage :enc-inh (:use :enc-base))
(defpackage :enc-plain (:use))

(defun %enc-status (name package)
  (multiple-value-bind (s status) (find-symbol name package)
    (list (and s (package-name (symbol-package s))) status)))

(deftest export-name-conflict.own-symbol
  (list (handler-case (progn (export (find-symbol "X" :enc-src) :enc-own) :no-error)
          (package-error () :package-error))
        (%enc-status "X" :enc-own))
  (:package-error ("ENC-OWN" :internal)))

(deftest export-name-conflict.inherited-symbol
  (progn
    (intern "W" :enc-src)
    (list (handler-case (progn (export (find-symbol "W" :enc-src) :enc-inh) :no-error)
            (package-error () :package-error))
          (%enc-status "W" :enc-inh)))
  (:package-error ("ENC-BASE" :inherited)))

(deftest export-name-conflict.continue-restart-takes-the-new-symbol
  (progn
    (defpackage :enc-own2 (:use) (:intern "Z"))
    (list (handler-bind ((package-error (lambda (c) (declare (ignore c)) (invoke-restart 'continue))))
            (export (find-symbol "Z" :enc-src) :enc-own2) :continued)
          (%enc-status "Z" :enc-own2)))
  (:continued ("ENC-SRC" :external)))

(deftest export-name-conflict.valid-exports-unchanged
  (let ((s (intern "OWN" :enc-plain)))
    (list (export s :enc-plain)                              ; its own symbol
          (export s :enc-plain)                              ; again: no effect
          (export (find-symbol "X" :enc-src) :enc-src)       ; already external
          (%enc-status "OWN" :enc-plain)))
  (t t t ("ENC-PLAIN" :external)))

(deftest export-name-conflict.inherited-same-symbol
  (progn
    (defpackage :enc-inh2 (:use :enc-base))
    (list (export (find-symbol "W" :enc-base) :enc-inh2)
          (%enc-status "W" :enc-inh2)))
  (t ("ENC-BASE" :external)))
