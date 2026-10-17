;;; A method's specializers in the location table: an (EQL constant) matches
;;; the method object, and reading them back interns nothing.

(defun %dsd-source ()
  (let ((src (merge-pathnames "dsd-fixture.lisp" (regression-temp-dir))))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l '("(defpackage \"DS-DETAIL\" (:use \"CL\"))"     ; 1
                   "(in-package \"DS-DETAIL\")"                  ; 2
                   "(defconstant +k+ 42)"                        ; 3
                   "(defgeneric dm (x))"                         ; 4
                   "(defmethod dm ((x (eql +k+))) x)"))          ; 5
        (write-line l s)))
    (namestring (truename src))))

(deftest definition-sources-method-detail.eql-constant
  (progn
    (when (find-package "DS-DETAIL") (delete-package "DS-DETAIL"))
    (load (%dsd-source))
    (let* ((gf (fdefinition (find-symbol "DM" "DS-DETAIL")))
           (m (first (dotcl-mop:generic-function-methods gf)))
           (plist (dotcl:definition-source m)))
      (list (getf plist :line) (getf plist :specializers)
            (progn (remove-method gf m)
                   (mapcar (lambda (p) (getf p :kind))
                           (dotcl:definition-sources (find-symbol "DM" "DS-DETAIL")))))))
  (5 ((eql 42)) (:generic-function)))

(deftest definition-sources-method-detail.read-back-interns-nothing
  (progn
    (dolist (p '("DS-DETAIL-Q" "DS-DETAIL-R"))
      (when (find-package p) (delete-package p)))
    (make-package "DS-DETAIL-Q" :use nil)
    (make-package "DS-DETAIL-R" :use nil)
    (let ((gone (intern "GONE" "DS-DETAIL-Q"))
          (dropped (intern "DROPPED" "DS-DETAIL-R"))
          (name (intern "DMR" "DS-DETAIL-R")))
      (dotcl:record-definition-sources
       `(defmethod ,name ((x ,gone) (y ,dropped))) "detail.lisp" 1)
      (unintern dropped "DS-DETAIL-R")
      (delete-package "DS-DETAIL-Q")
      (let ((specs (getf (first (dotcl:definition-sources name)) :specializers)))
        (list (mapcar #'symbol-name specs)
              (mapcar #'symbol-package specs)
              (find-symbol "DROPPED" "DS-DETAIL-R")))))
  (("GONE" "DROPPED") (nil nil) nil))
