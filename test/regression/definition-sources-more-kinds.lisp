;;; Definitions the location table used to miss: the :METHOD options of a
;;; DEFGENERIC, DEFINE-METHOD-COMBINATION, and a DEFUN that is not CL:DEFUN.

(defun %dsm-source ()
  (let ((src (merge-pathnames "dsm-fixture.lisp" (regression-temp-dir))))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l '("(defpackage \"DS-MORE\" (:use \"CL\") (:shadow \"DEFUN\"))"   ; 1
                   "(in-package \"DS-MORE\")"                                    ; 2
                   "(defgeneric g1 (x)"                                          ; 3
                   "  (:method ((x integer)) x)"                                 ; 4
                   "  (:method :before ((x (eql 'k))) x))"                       ; 5
                   "(define-method-combination mc1 :operator +)"                 ; 6
                   "(defmacro defun (name) `(defvar ,name 1))"                   ; 7
                   "(defun not-a-function)"                                      ; 8
                   "(defclass c1 () ((a :accessor c1-a)))"))                     ; 9
        (write-line l s)))
    (namestring (truename src))))

(defun %dsm-short (plist)
  (list* (getf plist :kind) (getf plist :line)
         (when (eq (getf plist :kind) :method)
           (list (getf plist :qualifiers) (getf plist :specializers)))))

(defun %dsm-results ()
  (flet ((sym (n) (find-symbol n "DS-MORE")))
    (write-to-string
     (list (sort (mapcar #'%dsm-short (dotcl:definition-sources (sym "G1")))
                 #'string< :key #'prin1-to-string)
           (mapcar #'%dsm-short (dotcl:definition-sources (sym "MC1")))
           (mapcar #'%dsm-short (dotcl:definition-sources (sym "NOT-A-FUNCTION")))
           (mapcar (lambda (m) (%dsm-short (dotcl:definition-source m)))
                   (append (dotcl-mop:generic-function-methods (fdefinition (sym "G1")))
                           (dotcl-mop:generic-function-methods (fdefinition (sym "C1-A"))))))
     :pretty nil :readably nil :escape t)))

;; The methods of a DEFGENERIC are located at its top level form.
(defparameter *dsm-expected*
  (concatenate 'string
               "(((:GENERIC-FUNCTION 3) (:METHOD 3 (:BEFORE) ((EQL DS-MORE::K))) "
               "(:METHOD 3 NIL (INTEGER))) "
               "((:METHOD-COMBINATION 6)) ((:VARIABLE 8)) "
               "((:METHOD 3 NIL (INTEGER)) (:METHOD 3 (:BEFORE) ((EQL DS-MORE::K))) "
               "(:METHOD 9 NIL (DS-MORE::C1))))"))

(deftest definition-sources-more-kinds.load-source
  (progn
    (when (find-package "DS-MORE") (delete-package "DS-MORE"))
    (load (%dsm-source))
    (let ((got (%dsm-results)))
      (or (string= got *dsm-expected*) got)))
  t)

(deftest-emitting-only definition-sources-more-kinds.load-fasl
  (let ((fasl (merge-pathnames "dsm-fixture.fasl" (regression-temp-dir)))
        (src (%dsm-source)))
    (when (find-package "DS-MORE") (delete-package "DS-MORE"))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file src :output-file fasl))
    (delete-package "DS-MORE")
    (load fasl)
    (let ((got (%dsm-results)))
      (or (string= got *dsm-expected*) got)))
  t)
