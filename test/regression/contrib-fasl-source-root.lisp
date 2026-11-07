;;; A contrib fasl built for a package records its sources as /dotcl-src/NAME/...
;;; (see source-path-unmap). REQUIRE of the module finds the fasl in a contrib
;;; directory with the source next to it, so find-definition on the library's
;;; functions has to lead to that source without any REGISTER-SOURCE-ROOT call.

(defun %cfsr-write (path text)
  (ensure-directories-exist path)
  (with-open-file (o path :direction :output :if-exists :supersede)
    (write-string text o))
  path)

(deftest-emitting-only contrib-fasl-source-root.require-finds-source
  (let* ((root (format nil "~A/cfsr1/" (regression-temp-dir)))
         (build (format nil "~Abuild/cfsrmod" root))
         (moddir (format nil "~A/contrib/cfsrmod/" root))
         (text "(defun cfsr-probe-fn (x) (1+ x))
")
         (src (%cfsr-write (format nil "~A/cfsrmod.lisp" build) text))
         (paths (dotnet:static "DotCL.Runtime" "ContribExtraSearchPaths")))
    (%cfsr-write (format nil "~Acfsrmod.lisp" moddir) text)
    (let ((dotcl:*source-path-map* (list (cons build "/dotcl-src/cfsrmod"))))
      (compile-file src :output-file (format nil "~Acfsrmod.fasl" moddir)))
    (dotnet:invoke paths "Add" (format nil "~Acontrib/" root))
    (unwind-protect
         (progn
           (require "cfsrmod")
           (let ((file (substitute #\/ #\\
                                   (getf (dotcl:function-source-location 'cfsr-probe-fn) :file))))
             (list (and (search "/contrib/cfsrmod/cfsrmod.lisp" file) t)
                   (and (probe-file file) t))))
      (dotnet:invoke paths "Remove" (format nil "~Acontrib/" root))))
  (t t))
