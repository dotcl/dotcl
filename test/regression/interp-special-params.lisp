;;; A lambda parameter whose name is globally special (DEFVAR, or a standard
;;; variable like *PACKAGE*) must be bound DYNAMICALLY on both evaluator paths.
;;;
;;; The tree-walk interpreter only did that for names the body itself declared
;;; special; a globally special one went into its lexical alist. Interpreted code
;;; reading the variable saw the new value, but any function it called did not:
;;; INTERN still interned into the outer *PACKAGE*.
;;;
;;; The compiler evaluates MACROLET expanders with the interpreter, so this also
;;; reached compiled code. SBCL's vm-ir2tran builds VOP names in a MACROLET with
;;;   (lambda (s &aux (ptype ...) (*package* (find-package "SB-VM"))) ...)
;;; and SYMBOLICATE interned them in the wrong package, which stopped the SBCL
;;; cross-build ("... is not the name of a defined VOP").

(defpackage :isp-pkg (:use :cl))
(defvar *isp-var* :outer)
(defun isp-read-var () *isp-var*)

(defun %isp (mode form)
  (let ((dotcl:*evaluator-mode* mode))
    (handler-case (eval form)
      (error (e) (list :error (princ-to-string e))))))

(defparameter %isp-forms
  '((funcall (lambda (&aux (*package* (find-package "ISP-PKG")))
               (package-name (symbol-package (intern "ISP-Q")))))
    (mapcar (lambda (s &aux (*package* (find-package "ISP-PKG")))
              (declare (ignore s))
              (package-name (symbol-package (intern "ISP-Q"))))
            '(1))
    (funcall (lambda (*isp-var*) (isp-read-var)) :required)
    (funcall (lambda (&optional (*isp-var* :optional)) (isp-read-var)))
    (funcall (lambda (&key ((:v *isp-var*) :key)) (isp-read-var)))
    (funcall (lambda (&rest *isp-var*) (isp-read-var)) 1 2)
    ;; the binding is undone on exit
    (progn (funcall (lambda (*isp-var*) (isp-read-var)) :inner) (isp-read-var))))

(deftest interp-special-params.compile
  (mapcar (lambda (f) (%isp :compile f)) %isp-forms)
  ("ISP-PKG" ("ISP-PKG") :required :optional :key (1 2) :outer))

(deftest interp-special-params.interpret
  (mapcar (lambda (f) (%isp :interpret f)) %isp-forms)
  ("ISP-PKG" ("ISP-PKG") :required :optional :key (1 2) :outer))

;;; The MACROLET expander runs in the interpreter even in compiled code.
(defun isp-macrolet-package ()
  (macrolet ((m ()
               `(list ,@(mapcar (lambda (s &aux (*package* (find-package "ISP-PKG")))
                                  `',(intern s))
                                '("ISP-M")))))
    (package-name (symbol-package (first (m))))))

(deftest interp-special-params.macrolet-expander
  (isp-macrolet-package)
  "ISP-PKG")
