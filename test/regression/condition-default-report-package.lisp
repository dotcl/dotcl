;;; The default report of a condition without :report names its class qualified whatever *package* is:
;;; PKG:NAME for an exported symbol, PKG::NAME for an internal one.

(defpackage :cdrp-test (:use :cl) (:export #:cdrp-exported))
(in-package :cdrp-test)
(define-condition cdrp-exported (error) ())
(define-condition cdrp-internal (error) ())
(in-package :cl-user)

(deftest condition-default-report.exported
  (princ-to-string (make-condition 'cdrp-test:cdrp-exported))
  "Condition CDRP-TEST:CDRP-EXPORTED was signalled.")

(deftest condition-default-report.internal
  (princ-to-string (make-condition 'cdrp-test::cdrp-internal))
  "Condition CDRP-TEST::CDRP-INTERNAL was signalled.")

(deftest condition-default-report.in-own-package
  (let ((*package* (find-package :cdrp-test)))
    (princ-to-string (make-condition 'cdrp-test:cdrp-exported)))
  "Condition CDRP-TEST:CDRP-EXPORTED was signalled.")
