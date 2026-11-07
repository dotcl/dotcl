;;; A system in the closure that is a module of the image (found through REQUIRE, no
;;; files of its own) is required by the saved application: the closure had no source for
;;; it, so a hunchentoot executable died at start with an undefined function from the
;;; socket module usocket depends on.

(require "asdf")
(require "dotcl-bsd-sockets")

(defvar *sarm-dir* "test/regression/.tmp-save-app-require/")

(defun sarm-exe ()
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defun sarm-build ()
  (ensure-directories-exist (merge-pathnames "x" *sarm-dir*))
  (with-open-file (s (merge-pathnames "sarm-app.asd" *sarm-dir*) :direction :output :if-exists :supersede)
    (write-string "(defsystem \"sarm-app\" :depends-on (\"dotcl-bsd-sockets\") :components ((:file \"sarm-app\")))" s))
  (with-open-file (s (merge-pathnames "sarm-app.lisp" *sarm-dir*) :direction :output :if-exists :supersede)
    (write-string "(defun sarm-main () 1)" s))
  (pushnew (pathname (namestring (truename *sarm-dir*))) asdf:*central-registry* :test #'equal)
  (let ((out (namestring (merge-pathnames "out.fasl" (truename *sarm-dir*)))))
    (dotcl:save-application out :system "sarm-app")
    ;; A fresh process that loads only the fasl: the module must come from the fasl.
    (let ((r (dotcl:run-process
              (sarm-exe)
              (list "--asm" (regression-child-core) "--load" out
                    "--eval" "(princ (if (find-package \"DOTCL-BSD-SOCKETS\") :has :none))"))))
      (and (search "HAS" (concatenate 'string (second r) (third r))) t))))

(deftest-emitting-only save-application-require-module
  (sarm-build)
  t)
