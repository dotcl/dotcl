;;; save-application: a bundled image answers ASDF about the systems it contains.
;;;
;;; Libraries routinely bind their own version at load time from
;;; (asdf:component-version (asdf:find-system :self)) — dexador and cl-str both
;;; do. A bundle has no .asd files, so on the deployed side that form signalled
;;; MISSING-COMPONENT and the whole image refused to load. save-application now
;;; emits asdf:register-preloaded-system for every system in the closure, with
;;; the version read while the building image still had a real registry.
;;;
;;; Hermetic and in-process: writes a temp .asd + source, builds a core from it,
;;; then takes the system back out of ASDF's registry and off the search path
;;; before loading that core — the deployment situation, without a sub-process.
;;; The test asserts that removal actually worked before drawing any conclusion,
;;; so it cannot pass by accident. asdf symbols are reached through
;;; read-from-string so loading this file does not require asdf up front.

(defun %sap-eval (string) (eval (read-from-string string)))

(defun %sap-preloaded-system-case ()
  (require "asdf")
  (let* ((dir (format nil "~a/dotcl-sap-~a/"
                      (or (dotcl:getenv "TEMP") "/tmp") (get-internal-real-time)))
         (dirp (substitute #\/ #\\ dir))
         (core (concatenate 'string dirp "sapsys.core")))
    (ensure-directories-exist dirp)
    (with-open-file (s (concatenate 'string dirp "sapsys.asd")
                       :direction :output :if-exists :supersede)
      (write-string "(defsystem \"sapsys\" :version \"1.2.3\"
                       :components ((:file \"sapsys\")))" s))
    (with-open-file (s (concatenate 'string dirp "sapsys.lisp")
                       :direction :output :if-exists :supersede)
      ;; The load-time form that has no answer in a bundle.
      (write-string "(defpackage :sapsys (:use :cl) (:export #:version))
                     (in-package :sapsys)
                     (defparameter *v*
                       (asdf:component-version (asdf:find-system :sapsys)))
                     (defun version () *v*)" s))
    (%sap-eval (format nil "(pushnew ~s asdf:*central-registry* :test #'equal)" dirp))
    (%sap-eval "(asdf:load-system :sapsys)")
    (dotcl:save-application core :system "sapsys")
    ;; --- become the deployed image: no registry entry, nothing on the search
    ;; path that could answer for sapsys.
    (%sap-eval (format nil "(setf asdf:*central-registry*
                              (remove ~s asdf:*central-registry* :test #'equal))" dirp))
    (%sap-eval "(asdf:clear-system \"sapsys\")")
    (let ((still-findable (%sap-eval "(and (asdf:find-system \"sapsys\" nil) t)")))
      (if still-findable
          ;; The simulation failed; say so rather than reporting a pass.
          :system-still-findable
          (handler-case
              (progn (load core)
                     (let ((v (funcall (read-from-string "sapsys:version"))))
                       (if (equal v "1.2.3") :ok (list :wrong-version v))))
            (error (e) (list :load-failed (type-of e))))))))

(deftest-compiled-only save-application-registers-bundled-systems
  (%sap-preloaded-system-case)
  :ok)
