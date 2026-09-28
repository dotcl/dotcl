;;; `dotcl pack` refuses to build a system whose closure it cannot resolve.
;;;
;;; The closure walk used to skip a dependency ASDF could not find. When the
;;; missing systems were the root's own dependencies, ASDF could not list the
;;; root's components either, that failure was skipped as well, and the fasl
;;; came out with none of the application's code in it. pack then exited 0 and
;;; wrote packages whose tool died at its first call. Now the walk names every
;;; system it could not resolve and the build stops, and a closure with no Lisp
;;; sources at all is refused rather than packed. On the command line that means
;;; a non-zero exit and no .nupkg in the output directory.
;;;
;;; NOTE: stay in the default load package (CL-USER); an (in-package ...) into a
;;; (:use :cl)-only package hides the framework's DEFTEST macro. Helpers are
;;; pmd- prefixed. The fixture systems live in .asd files written to a temp dir
;;; rather than in this file, because a DEFSYSTEM inside a file the suite LOADs
;;; makes ASDF treat this file as the system's definition and read it again.

(require "asdf")

(defvar *pmd-dir* "test/regression/.tmp-pack-missing/")

(defun pmd-write (name text)
  (with-open-file (s (merge-pathnames name (truename *pmd-dir*))
                     :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (write-string text s)))

(defun pmd-path (name)
  (namestring (merge-pathnames name (truename *pmd-dir*))))

(defun pmd-setup ()
  (ensure-directories-exist *pmd-dir*)
  (ensure-directories-exist (merge-pathnames "empty-from/" (truename *pmd-dir*)))
  ;; An application whose dependency nothing can find.
  (pmd-write "pmd-app.asd"
             "(defsystem \"pmd-app\" :version \"1.0.0\"
  :description \"Fixture with a dependency nobody provides\"
  :author \"Fixture Author\" :license \"MIT\"
  :depends-on (\"pmd-absent-dependency\")
  :components ((:file \"pmd-app\")))")
  (pmd-write "pmd-app.lisp"
             "(defpackage #:pmd-app (:use #:cl) (:export #:main))
(in-package #:pmd-app)
(defun main () 1)")
  ;; A system with nothing to compile anywhere in its closure.
  (pmd-write "pmd-empty.asd"
             "(defsystem \"pmd-empty\" :components ())")
  (let ((out (merge-pathnames "out/" (truename *pmd-dir*))))
    (when (probe-file out)
      (dolist (f (directory (merge-pathnames "*.nupkg" out)))
        (delete-file f))))
  (pushnew (pathname (namestring (truename *pmd-dir*)))
           asdf:*central-registry* :test #'equal))

(defun pmd-pack-error (system)
  "Build SYSTEM's fasl the way `dotcl pack` does; return the error message, or
:NO-ERROR when the build went through."
  (pmd-setup)
  (handler-case
      (progn
        (dotnet:static "DotCL.DotclBuild" "PackFasl"
                       system (pmd-path (concatenate 'string system ".fasl")) nil nil nil)
        :no-error)
    (error (c) (princ-to-string c))))

;;; The missing system is named, as not found, together with who needs it.
(deftest-emitting-only pack-missing-dependency.names-the-missing-system
  (let ((msg (pmd-pack-error "pmd-app")))
    (and (stringp msg)
         (search "pmd-absent-dependency: not found (required by pmd-app)" msg)
         t))
  t)

;;; A closure with no sources would have made a fasl with no code in it.
(deftest-emitting-only pack-missing-dependency.empty-closure-is-refused
  (let ((msg (pmd-pack-error "pmd-empty")))
    (and (stringp msg) (search "no Lisp source files" msg) t))
  t)

;;; End to end: the CLI exits non-zero, says which system is missing, and writes
;;; no package. --from is an empty directory: the build stops before any donor
;;; package is read, which is exactly the point.
(defvar *pmd-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *pmd-core*
  (or (ignore-errors (namestring (truename "compiler/cil-out.sil")))
      "compiler/cil-out.sil"))

(defun pmd-cli ()
  (pmd-setup)
  (let* ((out (pmd-path "out/"))
         (result (dotcl:run-process
                  *pmd-exe*
                  (list "--asm" *pmd-core* "pack"
                        "--system" "pmd-app" "--id" "pmd-app" "--command" "pmd-app"
                        "-o" out "--from" (pmd-path "empty-from/")
                        "--dotcl-version" "99.0.0" "--rids" "any"
                        "--asd-search-path" (pmd-path "")))))
    (list (first result)
          (and (search "pmd-absent-dependency" (third result)) t)
          (directory (merge-pathnames "*.nupkg" out)))))

(deftest-emitting-only pack-missing-dependency.cli-fails-and-writes-nothing
  (pmd-cli)
  (1 t nil))
