;;; FUNCTION-SOURCE-LOCATION and a debugger frame lead to the function's own
;;; definition, not to a later definition of another kind under that name.

(defparameter *fslk-file*
  (let ((src (merge-pathnames "fslk-fixture.lisp" (regression-temp-dir))))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (dolist (l '("(defun fslk-foo () 1)"                          ; 1
                   "(defvar fslk-foo 2)"                            ; 2
                   "(defmethod fslk-bar ((x integer)) x)"           ; 3
                   "(defclass fslk-bar () ())"                      ; 4
                   "(deftype fslk-baz () 'integer)"))               ; 5
        (write-line l s)))
    (namestring (truename src))))

(load *fslk-file*)

(deftest function-source-location-kind.function-over-variable
  (getf (dotcl:function-source-location 'fslk-foo) :line)
  1)

(deftest function-source-location-kind.method-over-class
  (getf (dotcl:function-source-location 'fslk-bar) :line)
  3)

(deftest function-source-location-kind.other-kind-only
  (getf (dotcl:function-source-location 'fslk-baz) :line)
  5)

(deftest function-source-location-kind.frame
  (notnot (search "fslk-fixture.lisp:1"
                  (first (dotcl::%debugger-source-lines "FSLK-FOO" t 0))))
  t)
