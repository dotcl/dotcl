;;; A definition that a top level macro form expands into (a DEFVAR inside
;;; the expansion of a library's DEFSECTION, say) is located at that macro
;;; form, both when the source is loaded and when its fasl is.

(defparameter *mx-sl-src* nil)

(defun %mx-sl-source ()
  (or *mx-sl-src*
      (let ((src (merge-pathnames "mx-sl-fixture.lisp" (regression-temp-dir))))
        (with-open-file (s src :direction :output :if-exists :supersede)
          (dolist (l '("(defpackage \"MX-SL\" (:use \"CL\"))"                          ; 1
                       "(in-package \"MX-SL\")"                                       ; 2
                       "(defmacro defthing (var fn)"                                  ; 3
                       "  `(progn (defvar ,var 1) (defun ,fn () 2)))"                 ; 4
                       "(defthing *mx-a* mx-a-fn)"                                    ; 5
                       ""                                                             ; 6
                       "(progn (defthing *mx-b* mx-b-fn))")) ; 7
            (write-line l s)))
        (setf *mx-sl-src* (namestring (truename src))))))

(defun %mx-sl-locs ()
  (mapcar (lambda (name)
            (let ((loc (dotcl:function-source-location (find-symbol name "MX-SL"))))
              (list (let ((file (and loc (probe-file (getf loc :file)))))
                      (and file (equal (namestring file) *mx-sl-src*)))
                    (getf loc :line))))
          '("*MX-A*" "MX-A-FN" "*MX-B*" "MX-B-FN" "DEFTHING")))

(defun %mx-sl-forget ()
  (loop for (definer name) in '((defvar "*MX-A*") (defun "MX-A-FN") (defvar "*MX-B*")
                                 (defun "MX-B-FN") (defmacro "DEFTHING"))
        do (dotcl:record-definition-sources
            (list definer (find-symbol name "MX-SL")) "elsewhere.lisp" 999)))

(deftest macro-definition-source-location.load-source
  (progn
    (load (%mx-sl-source))
    (%mx-sl-locs))
  ((t 5) (t 5) (t 7) (t 7) (t 3)))

(deftest-emitting-only macro-definition-source-location.load-fasl
  (let ((fasl (merge-pathnames "mx-sl-fixture.fasl" (regression-temp-dir))))
    (load (%mx-sl-source))
    (let ((*error-output* (make-broadcast-stream)))
      (compile-file *mx-sl-src* :output-file fasl))
    (%mx-sl-forget)
    (load fasl)
    (%mx-sl-locs))
  ((t 5) (t 5) (t 7) (t 7) (t 3)))
