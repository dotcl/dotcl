;;; A DEFTYPE (or other form with compile time side effects) inside a top level
;;; LOCALLY, MACROLET or SYMBOL-MACROLET is still a top level form (CLHS
;;; 3.2.3.1), so COMPILE-FILE makes the type known for the rest of the file.
;;; COMPILE-FILE ran DEFTYPE at compile time only when DEFTYPE was the operator
;;; of the top level form itself; a LOCALLY with one body form was not looked
;;; into at all. serapeum's DEFUNION expands to
;;; (locally (declare ...) (symbol-macrolet (...) (deftype ...) ...)), and the
;;; exhaustiveness checks later in the same file could not tell what the union
;;; type was.

(defvar *cfts-seen* nil)

(defun %cfts-compile (name forms)
  "Compile FORMS as a file and return what the last form recorded at compile
time in *CFTS-SEEN*."
  (let* ((dir (regression-temp-dir))
         (src (merge-pathnames (format nil "~a.lisp" name) dir))
         (fasl (merge-pathnames (format nil "~a.fasl" name) dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (dolist (f forms) (prin1 f s) (terpri s))))
    (setf *cfts-seen* :unset)
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    *cfts-seen*))

(defmacro %cfts-record (&rest types)
  `(eval-when (:compile-toplevel)
     (setf *cfts-seen*
           (list ,@(mapcar (lambda (ty)
                             `(multiple-value-list (subtypep 'fixnum ',ty)))
                           types)))))

(deftest-compiled-only cf-toplevel-scope-compile-time.deftype
  (%cfts-compile
   "cfts-deftype"
   '((locally (deftype cfts-a () 'integer))
     (locally (declare (optimize speed))
       (symbol-macrolet ((cfts-x 1))
         (deftype cfts-b () 'integer)
         (defstruct cfts-s slot)))
     (symbol-macrolet ((cfts-y 1)) (deftype cfts-c () 'integer))
     (macrolet ((cfts-m () ''integer)) (deftype cfts-d () (cfts-m)))
     (%cfts-record cfts-a cfts-b cfts-c cfts-d)))
  ((t t) (t t) (t t) (t t)))
