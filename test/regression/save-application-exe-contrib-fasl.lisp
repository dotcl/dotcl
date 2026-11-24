;;; regression: full-tier-only skip-under-interp
;;; A published executable keeps the contrib modules' .fasl files beside it. The single-file
;;; bundler took them for managed assemblies and packed them into the executable, so each
;;; (require "module") found only the .lisp and compiled it again at every start.
;;;
;;; The publish copies runtime/contrib/**, which a clean source tree does not have (it is the
;;; staging directory of a pack, or links to contrib/). Without it the executable is built with
;;; no contrib at all, so the test then checks only that the project file marks the contrib
;;; content to stay out of the single file.

(defvar *saecf-dir* "test/regression/.tmp-save-app-exe-contrib/")

(defun saecf-text (path)
  (with-open-file (s path)
    (let ((str (make-string (file-length s))))
      (subseq str 0 (read-sequence str s)))))

(defun saecf-staged-p ()
  (and (probe-file "runtime/contrib/dotcl-gray/dotcl-gray.fasl") t))

(defun saecf-build ()
  (when (probe-file *saecf-dir*)
    (dotnet:static "System.IO.Directory" "Delete" (namestring (truename *saecf-dir*)) t))
  (ensure-directories-exist (merge-pathnames "x" *saecf-dir*))
  (with-open-file (s (merge-pathnames "app.lisp" *saecf-dir*) :direction :output :if-exists :supersede)
    (write-string "(defun saecf-main () (require \"dotcl-gray\") (princ (if (find-package \"DOTCL-GRAY\") :has :none)) (terpri))" s))
  (let* ((dir (truename *saecf-dir*))
         (exe (namestring (merge-pathnames "app" dir))))
    (dotcl:save-application exe :load (namestring (merge-pathnames "app.lisp" dir))
                                :toplevel "CL-USER::SAECF-MAIN" :executable t)
    (list (and (probe-file (merge-pathnames "contrib/dotcl-gray/dotcl-gray.fasl" dir)) t)
          (and (search "HAS" (let ((r (dotcl:run-process exe nil))) (concatenate 'string (second r) (third r)))) t))))

(deftest save-application-exe-contrib-fasl
  (if (saecf-staged-p)
      (saecf-build)
      (list (and (search "<ExcludeFromSingleFile>true</ExcludeFromSingleFile>"
                         (saecf-text "runtime/runtime.csproj"))
                 t)
            t))
  (t t))
