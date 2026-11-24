;;; regression: full-tier-only skip-under-interp
;;; :r2r nil publishes the executable without ReadyToRun (the default, and :r2r t, publish
;;; with it). The flag used to be a no-op: the runtime project already published ReadyToRun,
;;; so :r2r t added the same property and :r2r nil changed nothing. Building an executable
;;; needs a publish, so it runs only where the contrib staging directory exists (see
;;; save-application-exe-contrib-fasl).

(defvar *saer-dir* "test/regression/.tmp-save-app-exe-r2r-nil/")

(defun saer-build ()
  (when (probe-file *saer-dir*)
    (dotnet:static "System.IO.Directory" "Delete" (namestring (truename *saer-dir*)) t))
  (ensure-directories-exist (merge-pathnames "x" *saer-dir*))
  (with-open-file (s (merge-pathnames "app.lisp" *saer-dir*) :direction :output :if-exists :supersede)
    (write-string "(defun saer-main () (princ :hello-r2r-nil) (terpri))" s))
  (let* ((dir (truename *saer-dir*))
         (exe (namestring (merge-pathnames "app" dir))))
    (dotcl:save-application exe :load (namestring (merge-pathnames "app.lisp" dir))
                                :toplevel "CL-USER::SAER-MAIN" :executable t :r2r nil)
    (and (search "HELLO-R2R-NIL" (let ((r (dotcl:run-process exe nil))) (concatenate 'string (second r) (third r))))
         t)))

(deftest-emitting-only save-application-exe-r2r-nil
  (if (probe-file "runtime/contrib/dotcl-gray/dotcl-gray.fasl")
      (saer-build)
      t)
  t)
