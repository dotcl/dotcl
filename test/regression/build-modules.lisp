;;; `dotcl build <asd>` compiles the files inside the system's :module
;;; components, not just the system's direct children.
;;;
;;; The build listed the root's sources as its direct children that had an
;;; input file for COMPILE-OP. A :module has none, so every file under a module
;;; was dropped and the rest compiled as if it were the whole system; a static
;;; file does have one, so a README would have been compiled as Lisp. The
;;; --root-sources-out list (the MSBuild Inputs) came from the same place.
;;;
;;; Each case runs the CLI in a child process. The fixture system lives in a
;;; .asd written to a temp dir rather than in this file, because a DEFSYSTEM in
;;; a file the suite LOADs makes ASDF treat this file as its definition.
;;;
;;; NOTE: stay in the default load package (CL-USER); an (in-package ...) into a
;;; (:use :cl)-only package hides the framework's DEFTEST macro. Helpers are
;;; bmod- prefixed.

(defvar *bmod-dir* "test/regression/.tmp-build-modules/")

(defvar *bmod-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *bmod-core*
  (or (ignore-errors (namestring (truename "compiler/cil-out.sil")))
      "compiler/cil-out.sil"))

(defun bmod-path (name)
  (namestring (merge-pathnames name (truename *bmod-dir*))))

(defun bmod-write (name text)
  (with-open-file (s (bmod-path name)
                     :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (write-string text s)))

(defun bmod-setup ()
  (dolist (d '("" "app/" "app/src/" "app/src/sub/" "out/"))
    (ensure-directories-exist (merge-pathnames d *bmod-dir*)))
  (bmod-write "app/bmod-app.asd"
              "(defsystem \"bmod-app\"
  :components ((:file \"top\")
               (:static-file \"README\")
               (:module \"src\" :serial t
                :components ((:file \"a\")
                             (:module \"sub\" :components ((:file \"b\")))))))")
  (bmod-write "app/README" "Not Lisp: (unbalanced")
  (bmod-write "app/top.lisp"
              "(defpackage #:bmod-app (:use #:cl)) (in-package #:bmod-app)
(defvar *log* (list :top))")
  (bmod-write "app/src/a.lisp" "(in-package #:bmod-app) (push :a *log*)")
  (bmod-write "app/src/sub/b.lisp"
              "(in-package #:bmod-app) (push :b *log*)
(defun report () (format t \"LOG=~s~%\" (reverse *log*)))"))

(defun bmod-run (&rest args)
  (dotcl:run-process *bmod-exe* (list* "--asm" *bmod-core* args)))

;;; The fasl holds every file, in the order ASDF would load them.
(deftest-emitting-only build-modules.fasl-includes-module-files
  (progn
    (bmod-setup)
    (let* ((fasl (bmod-path "out/bmod-app.fasl"))
           (b (bmod-run "build" (bmod-path "app/bmod-app.asd") "--output" fasl))
           (r (bmod-run "--eval" (format nil "(load ~s)" fasl)
                        "--eval" "(bmod-app::report)")))
      (list (first b) (first r)
            (and (search "LOG=(:TOP :A :B)" (second r)) t))))
  (0 0 t))

;;; The MSBuild Inputs list names the module files and not the static file.
(deftest-emitting-only build-modules.root-sources-list-module-files
  (progn
    (bmod-setup)
    (let* ((list-file (bmod-path "out/root-sources.txt"))
           (r (bmod-run "build" (bmod-path "app/bmod-app.asd") "--resolve-deps"
                        "--manifest-out" (bmod-path "out/manifest.txt")
                        "--root-sources-out" list-file))
           (names (with-open-file (s list-file)
                    (loop for line = (read-line s nil)
                          while line
                          collect (file-namestring line)))))
      (list (first r) names)))
  (0 ("top.lisp" "a.lisp" "b.lisp")))
