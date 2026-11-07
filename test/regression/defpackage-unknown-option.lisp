;;; DEFPACKAGE signals a PROGRAM-ERROR for an option it does not know (CLHS 11.2.13),
;;; so a misspelt option is not silently dropped. Every standard option, and SBCL's
;;; :LOCK and :IMPLEMENT that libraries write, is still accepted.

(defun %dpuo (form)
  (handler-case (progn (eval form) :ok) (program-error () :program-error)))

(deftest defpackage-unknown-option.unknown
  (list (%dpuo '(defpackage #:dpuo-1 (:use) (:no-such-option 1)))
        (%dpuo '(defpackage #:dpuo-2 (:use) (:exprot "A")))
        (%dpuo '(defpackage #:dpuo-3 (:use) :bare-keyword))
        (%dpuo '(defpackage #:dpuo-4 (:use) "string")))
  (:program-error :program-error :program-error :program-error))

(deftest defpackage-unknown-option.known-options-accepted
  (list (%dpuo '(defpackage #:dpuo-5 (:use) (:export "A") (:intern "B") (:nicknames "DPUO-5N")
                  (:documentation "d") (:size 10) (:shadow "C")))
        (%dpuo '(defpackage #:dpuo-6 (:use :cl) (:import-from :cl "CAR") (:lock nil)))
        (%dpuo '(defpackage #:dpuo-7 (:use) (:local-nicknames (:l #:dpuo-5))))
        (%dpuo '(defpackage #:dpuo-8))
        (%dpuo '(defpackage #:dpuo-9 (:shadowing-import-from :cl "CDR"))))
  (:ok :ok :ok :ok :ok))

(deftest defpackage-unknown-option.package-not-created-by-the-bad-form
  (progn (%dpuo '(defpackage #:dpuo-10 (:use) (:no-such-option 1)))
         (null (find-package "DPUO-10")))
  t)
