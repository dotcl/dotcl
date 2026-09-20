;;; A package name-conflict error has to say what conflicted.
;;;
;;; The name-conflict signallers build their condition with
;;; MAKE-CONDITION-FROM-TYPE, passing the initargs by name. They named them with
;;; Startup.Sym(":PACKAGE") and friends -- which looks up a symbol *called*
;;; ":PACKAGE", not the keyword :PACKAGE -- so not one initarg ever landed. The
;;; condition came out with no package and no message, and every one of these
;;; errors printed as the report's fallback:
;;;
;;;     Package error on NIL.
;;;
;;; The other half is the class: PACKAGE-ERROR is not a SIMPLE-CONDITION, so a
;;; :FORMAT-CONTROL given to it has no slot to live in. SIMPLE-PACKAGE-ERROR
;;; exists in this image for exactly that reason and is what these signal now.
;;;
;;; This cost a real diagnosis: hu.dwim.walker failed to load with nothing but
;;; "Package error on NIL." to go on.

(defpackage :pem-source (:use))
(defpackage :pem-target (:use))

(defvar *pem-imported* (intern "CLASH" :pem-source))
(defvar *pem-existing* (intern "CLASH" :pem-target))

(deftest package-error-names-the-package-and-the-symbols
  (handler-case (progn (import *pem-imported* :pem-target) :no-error)
    (package-error (e)
      (let ((text (format nil "~a" e)))
        (list (and (search "CLASH" text) t)
              (and (search "PEM-TARGET" text) t)
              (package-name (package-error-package e))))))
  (t t "PEM-TARGET"))

;;; The report must not fall back to the package-less form.

(deftest package-error-report-is-not-the-bare-fallback
  (handler-case (progn (import *pem-imported* :pem-target) :no-error)
    (package-error (e)
      (and (search "Package error on NIL" (format nil "~a" e)) t)))
  nil)

;;; It stays a PACKAGE-ERROR for handlers, and gains SIMPLE-CONDITION so the
;;; format control has somewhere to live.

(deftest package-error-is-both-package-error-and-simple-condition
  (handler-case (progn (import *pem-imported* :pem-target) :no-error)
    (package-error (e) (list (typep e 'package-error) (typep e 'simple-condition))))
  (t t))

;;; CLHS 11.1.1.2.5: the conflict is correctable. CONTINUE uninterns the
;;; existing symbol and completes the import.

(defpackage :pem-continue (:use))
(defvar *pem-continue-existing* (intern "CLASH" :pem-continue))

(deftest package-error-continue-restart-completes-the-import
  (handler-bind ((package-error
                   (lambda (c)
                     (let ((r (find-restart 'continue c)))
                       (when r (invoke-restart r))))))
    (import *pem-imported* :pem-continue)
    (eq (find-symbol "CLASH" :pem-continue) *pem-imported*))
  t)

;;; A package designator that names nothing still reports the name it was given.

(deftest package-error-unknown-package-names-it
  (handler-case (progn (import *pem-imported* "PEM-NO-SUCH-PACKAGE") :no-error)
    (package-error (e)
      (and (search "PEM-NO-SUCH-PACKAGE" (format nil "~a" e)) t)))
  t)
