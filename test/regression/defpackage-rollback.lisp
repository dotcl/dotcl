;;; A DEFPACKAGE form that fails leaves nothing behind: the package it made is deleted again, and one it
;;; redefined is put back as it was. Forms that succeed behave as before.

(defpackage :dpr-a (:export #:conf))
(defpackage :dpr-b (:export #:conf))

(defun %dpr-error-p (form)
  (handler-case (progn (eval form) nil) (error () t)))

(deftest-emitting-only defpackage-rollback.new-package-removed
  (list (%dpr-error-p '(defpackage :dpr-n1 (:use :dpr-a :dpr-b)))
        (find-package :dpr-n1)
        (%dpr-error-p '(defpackage :dpr-n2 (:use :cl) (:import-from :dpr-a "NOPE")))
        (find-package :dpr-n2)
        (%dpr-error-p '(defpackage :dpr-n3 (:use :dpr-no-such)))
        (find-package :dpr-n3)
        (%dpr-error-p '(defpackage :dpr-n4 (:nicknames :dpr-a))))
  (t nil t nil t nil t))

(deftest-emitting-only defpackage-rollback.nickname-and-later-clause
  (list (%dpr-error-p '(defpackage :dpr-n5 (:nicknames :dpr-n5-nick) (:use :dpr-a :dpr-b)))
        (find-package :dpr-n5)
        (find-package :dpr-n5-nick))
  (t nil nil))

(deftest-emitting-only defpackage-rollback.existing-package-restored
  (progn
    (eval '(defpackage :dpr-e1 (:use :cl) (:nicknames :dpr-e1-old) (:export #:keep)))
    (let ((before-use (mapcar #'package-name (package-use-list :dpr-e1))))
      (list (%dpr-error-p '(defpackage :dpr-e1 (:use :cl :dpr-a :dpr-b) (:nicknames :dpr-e1-new)
                            (:export #:added)))
            (equal before-use (mapcar #'package-name (package-use-list :dpr-e1)))
            (package-nicknames :dpr-e1)
            (multiple-value-list (find-symbol "ADDED" :dpr-e1))
            (multiple-value-bind (s st) (find-symbol "KEEP" :dpr-e1) (list (symbol-name s) st)))))
  (t t ("DPR-E1-OLD") (nil nil) ("KEEP" :external)))

(deftest-emitting-only defpackage-rollback.valid-forms
  (progn
    (eval '(defpackage :dpr-v1 (:use)))
    (eval '(defpackage :dpr-v2 (:use :cl) (:shadow #:car) (:export #:car #:foo)))
    (eval '(defpackage :dpr-v2 (:use :cl) (:shadow #:car) (:export #:car #:foo #:bar)))
    (eval '(defpackage :dpr-v3 (:use :cl) (:import-from :dpr-a "CONF") (:intern #:hidden)))
    (list (package-use-list :dpr-v1)
          (sort (loop for s being the external-symbols of :dpr-v2 collect (symbol-name s)) #'string<)
          (eq (find-symbol "CONF" :dpr-v3) (find-symbol "CONF" :dpr-a))
          (nth-value 1 (find-symbol "HIDDEN" :dpr-v3))))
  (nil ("BAR" "CAR" "FOO") t :internal))
