;;; Removing a definition (FMAKUNBOUND, REMOVE-METHOD, DELETE-PACKAGE) drops
;;; its recorded location, also when the fasl's table is resolved only after.

(defun %dsr-source ()
  (let ((src (merge-pathnames "dsr-fixture.lisp" (regression-temp-dir))))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l '("(defpackage \"DS-RM\" (:use \"CL\"))"         ; 1
                   "(in-package \"DS-RM\")"                      ; 2
                   "(defvar foo 1)"                              ; 3
                   "(defun foo () 1)"                            ; 4
                   "(defgeneric bar (x))"                        ; 5
                   "(defmethod bar ((x integer)) x)"             ; 6
                   "(defmethod bar ((x string)) x)"              ; 7
                   "(defun (setf baz) (v) v)"                    ; 8
                   "(defmacro mac () 1)"))                       ; 9
        (write-line l s)))
    (namestring (truename src))))

(defun %dsr-short (plist)
  (list* (getf plist :kind) (getf plist :line)
         (when (eq (getf plist :kind) :method) (list (getf plist :specializers)))))

(defun %dsr-sym (name) (find-symbol name "DS-RM"))

(defun %dsr-remove-and-collect ()
  (let ((foo (%dsr-sym "FOO")) (bar (%dsr-sym "BAR"))
        (baz (%dsr-sym "BAZ")) (mac (%dsr-sym "MAC")))
    (remove-method (fdefinition bar) (find-method (fdefinition bar) '() (list (find-class 'string))))
    (fmakunbound foo)
    (fmakunbound (list 'setf baz))
    (fmakunbound mac)
    (let ((before (list (mapcar #'%dsr-short (dotcl:definition-sources foo))
                        (getf (dotcl:function-source-location foo) :line)
                        (mapcar #'%dsr-short (dotcl:definition-sources bar))
                        (dotcl:definition-sources (list 'setf baz))
                        (dotcl:definition-sources mac))))
      (delete-package "DS-RM")
      (append before
              (list (list (dotcl:definition-sources "DS-RM") (dotcl:definition-sources bar)
                          (dotcl:function-source-location bar)))))))

(defun %dsr-run (thunk)
  (when (find-package "DS-RM") (delete-package "DS-RM"))
  (funcall thunk)
  (prin1-to-string (%dsr-remove-and-collect)))

(defparameter *dsr-expected*
  (prin1-to-string '(((:variable 3)) 3 ((:generic-function 5) (:method 6 (integer))) nil nil
                     (nil nil nil))))

(deftest definition-sources-removal.load-source
  (let ((got (%dsr-run (lambda () (load (%dsr-source))))))
    (or (string= got *dsr-expected*) got))
  t)

(deftest-emitting-only definition-sources-removal.load-fasl
  (let ((fasl (merge-pathnames "dsr-fixture.fasl" (regression-temp-dir)))
        (src (%dsr-source)))
    (let ((got (%dsr-run (lambda ()
                           (let ((*error-output* (make-broadcast-stream)))
                             (compile-file src :output-file fasl))
                           (delete-package "DS-RM")
                           (load fasl)))))
      (or (string= got *dsr-expected*) got)))
  t)
