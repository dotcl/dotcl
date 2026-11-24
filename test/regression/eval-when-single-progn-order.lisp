;;; A top level EVAL-WHEN whose one body form is a PROGN (or a macro call that expands to
;;; one) holds top level forms: they run in order when LOAD reads a source file. It used to
;;; be compiled as one unit in which a DEFUN was installed before the forms ahead of it
;;; ran: fare-memoization's DEFINE-MEMO-FUNCTION (an UNMEMOIZE ahead of the DEFUN) then
;;; failed with "the symbol's function was changed since".

(defun %ewsp-load-forms (forms)
  (let ((path (regression-temp-file (format nil "ewsp-~D.lisp" (random 1000000)))))
    (with-open-file (o path :direction :output :if-exists :supersede)
      (dolist (f forms) (let ((*package* (find-package :cl-user)) (*print-readably* nil)) (prin1 f o) (terpri o))))
    (unwind-protect (load path) (delete-file path))))

(defvar *ewsp-log* nil)

(deftest eval-when-single-progn-order.progn-body
  (progn
    (setf *ewsp-log* nil)
    (%ewsp-load-forms
     '((defun ewsp-f () 1)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (progn (push (funcall 'ewsp-f) *ewsp-log*) (defun ewsp-f () 2)))
       (push (funcall 'ewsp-f) *ewsp-log*)))
    (reverse *ewsp-log*))
  (1 2))

(defmacro ewsp-redefine (name value)
  `(progn (push (funcall ',name) *ewsp-log*) (defun ,name () ,value)))

(deftest eval-when-single-progn-order.macro-body
  (progn
    (setf *ewsp-log* nil)
    (%ewsp-load-forms
     '((defun ewsp-g () 1)
       (eval-when (:compile-toplevel :load-toplevel :execute) (ewsp-redefine ewsp-g 2))
       (push (funcall 'ewsp-g) *ewsp-log*)))
    (reverse *ewsp-log*))
  (1 2))

(deftest eval-when-single-progn-order.single-plain-form-unchanged
  (progn
    (setf *ewsp-log* nil)
    (%ewsp-load-forms
     '((eval-when (:compile-toplevel :load-toplevel :execute) (push :one *ewsp-log*))
       (eval-when (:execute) (progn (push :a *ewsp-log*) (push :b *ewsp-log*)))))
    (reverse *ewsp-log*))
  (:one :a :b))
