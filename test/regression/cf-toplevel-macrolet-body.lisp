;;; The body of a top level MACROLET is made of top level forms (CLHS 3.2.3.1),
;;; and so is the expansion of a call to one of its macros. COMPILE-FILE kept a
;;; MACROLET with one body form whole: (macrolet ((m () `(progn ...))) (m))
;;; became one unit, and the creation forms of the literals in it (MAKE-LOAD-FORM)
;;; ran when the fasl started that unit, before the forms in front of them.
;;; SBCL's build expands one such local macro into every delayed DEFSTRUCT, and a
;;; layout literal's creation form needs the class an earlier DEFSTRUCT made.
;;; A call to a local macro is now expanded in its scope and the result is
;;; processed a form at a time. A form that names none of the local macros is no
;;; longer wrapped in the MACROLET again, so an IN-PACKAGE in the body changes
;;; how the rest of the file is read, as it does outside one.

(defvar *ctmb-flag*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct ctmb-lay name)
  (defmethod make-load-form ((x ctmb-lay) &optional e)
    (declare (ignore e))
    `(ctmb-load-lay ',(ctmb-lay-name x)))
  (defun ctmb-load-lay (name)
    ;; Made at load only after the form before the literal has run.
    (unless (boundp '*ctmb-flag*) (error "creation form ran too early"))
    (make-ctmb-lay :name name))
  (defclass ctmb-k () ((v :initarg :v :reader ctmb-v)))
  (defmethod make-load-form ((x ctmb-k) &optional e)
    (declare (ignore e))
    `(ctmb-make-k ,(ctmb-v x))))

(defun %ctmb-compile-load (name forms)
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (dolist (f forms) (prin1 f s) (terpri s))))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    (makunbound '*ctmb-flag*)
    (fmakunbound 'ctmb-make-k)
    (handler-case (progn (load fasl) :loaded)
      (error (c) (princ-to-string c)))))

;; A structure literal whose creation form needs the earlier form's effect.
(deftest-compiled-only cf-toplevel-macrolet-body.structure-literal
  (list (%ctmb-compile-load
         "ctmb-struct"
         '((macrolet ((m ()
                        `(progn
                           (defparameter *ctmb-flag* t)
                           (defparameter *ctmb-l* ',(make-ctmb-lay :name :lay)))))
             (m))))
        (ctmb-lay-name (symbol-value '*ctmb-l*)))
  (:loaded :lay))

;; An instance literal whose creation form calls a function the same expansion
;; defines.
(deftest-compiled-only cf-toplevel-macrolet-body.instance-literal
  (list (%ctmb-compile-load
         "ctmb-inst"
         '((macrolet ((m ()
                        `(progn
                           (defun ctmb-make-k (n) (make-instance 'ctmb-k :v (* n 10)))
                           (defparameter *ctmb-x* ',(make-instance 'ctmb-k :v 4)))))
             (m))))
        (ctmb-v (symbol-value '*ctmb-x*)))
  (:loaded 40))

;; EVAL-WHEN, DEFMACRO and IN-PACKAGE in the expansion act on the forms after
;; them, inside the expansion and in the rest of the file (as in SBCL 2.6.8).
(defvar *ctmb-ct-mark* nil)
(deftest-compiled-only cf-toplevel-macrolet-body.later-forms-see-effects
  (progn
    (when (find-package :ctmb-p) (delete-package :ctmb-p))
    (setf *ctmb-ct-mark* nil)
    (list (%ctmb-compile-load
           "ctmb-effects"
           '((defpackage :ctmb-p (:use :cl))
             (macrolet ((m ()
                          `(progn
                             (eval-when (:compile-toplevel :load-toplevel :execute)
                               (defparameter *ctmb-ct* 7))
                             (defmacro ctmb-mac () *ctmb-ct*)
                             (defparameter *ctmb-v* (ctmb-mac))
                             (eval-when (:compile-toplevel)
                               (setf *ctmb-ct-mark* :at-compile-time))
                             (in-package :ctmb-p))))
               (m))
             ;; Written unqualified: read in CTMB-P when the IN-PACKAGE took effect.
             (defparameter ctmb-read-here t)))
          *ctmb-ct-mark*
          (symbol-value '*ctmb-v*)
          (let ((s (find-symbol "CTMB-READ-HERE" :ctmb-p)))
            (and s (boundp s) (symbol-value s)))
          (package-name *package*)))
  (:loaded :at-compile-time 7 t "COMMON-LISP-USER"))
