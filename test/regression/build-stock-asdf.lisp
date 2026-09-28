;;; `dotcl build` (the MSBuild integration's two steps) keeps the bundled ASDF
;;; when stock asdf.asd / uiop.asd are visible to the build.
;;;
;;; A qlot or Quicklisp bundle often pins stock ASDF, so its asdf.asd sits in
;;; the source registry next to the application's dependencies. ASDF loads a
;;; registered asdf.asd of its own version "to allow loading from modified
;;; source", so the first .asd the build read rebuilt ASDF from sources that do
;;; not know dotcl. `dotcl pack` pinned ASDF and UIOP already; both build steps
;;; did not. The fixture's asdf.asd and uiop.asd stand in for the stock ones:
;;; same version as the bundled ASDF, and their only source signals, so loading
;;; them is visible.
;;;
;;; Each case runs the CLI in a child process: pinning is image-wide, and this
;;; image may have pinned already.
;;;
;;; NOTE: stay in the default load package (CL-USER); an (in-package ...) into a
;;; (:use :cl)-only package hides the framework's DEFTEST macro. Helpers are
;;; bsa- prefixed. The fixture systems live in .asd files written to a temp dir
;;; rather than in this file, because a DEFSYSTEM inside a file the suite LOADs
;;; makes ASDF treat this file as the system's definition.

(require "asdf")

(defvar *bsa-dir* "test/regression/.tmp-build-stock-asdf/")

(defvar *bsa-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *bsa-core*
  (or (ignore-errors (namestring (truename "compiler/cil-out.sil")))
      "compiler/cil-out.sil"))

(defun bsa-path (name)
  (ensure-directories-exist *bsa-dir*)
  (namestring (merge-pathnames name (truename *bsa-dir*))))

(defun bsa-write (name text)
  (with-open-file (s (bsa-path name)
                     :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (write-string text s)))

(defun bsa-setup ()
  (dolist (d '("" "stock/" "app/" "out/"))
    (ensure-directories-exist (merge-pathnames d *bsa-dir*)))
  (bsa-write "stock/asdf.asd"
             "(defsystem \"asdf\" :components ((:file \"boom-asdf\")))")
  (bsa-write "stock/version.lisp-expr"
             (format nil "~s~%" (asdf:asdf-version)))
  (bsa-write "stock/boom-asdf.lisp" "(error \"stock asdf source was loaded\")")
  (bsa-write "stock/uiop.asd"
             "(defsystem \"uiop\" :components ((:file \"boom-uiop\")))")
  (bsa-write "stock/boom-uiop.lisp" "(error \"stock uiop source was loaded\")")
  (bsa-write "app/bsa-app.asd"
             "(defsystem \"bsa-app\" :depends-on (\"uiop\")
  :components ((:file \"bsa-app\")))")
  (bsa-write "app/bsa-app.lisp" "(defpackage #:bsa-app (:use #:cl))"))

(defun bsa-build (&rest args)
  "Run `dotcl build` on the fixture app with stock/ on the search path.
Returns (exit-code stdout stderr)."
  (bsa-setup)
  (dotcl:run-process
   *bsa-exe*
   (append (list "--asm" *bsa-core* "build" (bsa-path "app/bsa-app.asd")
                 "--asd-search-path" (bsa-path "stock/"))
           args)))

(defun bsa-says (result text)
  (and (search text (concatenate 'string (second result) (third result))) t))

;;; Step 1 of the MSBuild build: resolving the dependencies.
(deftest-emitting-only build-stock-asdf.resolve-deps-keeps-bundled-asdf
  (let ((r (bsa-build "--resolve-deps" "--manifest-out" (bsa-path "out/m.txt"))))
    (list (first r) (bsa-says r "source was loaded")))
  (0 nil))

;;; Step 2: compiling the project.
(deftest-emitting-only build-stock-asdf.compile-keeps-bundled-asdf
  (let ((r (bsa-build "--output" (bsa-path "out/bsa-app.fasl"))))
    (list (first r) (bsa-says r "source was loaded")
          (and (probe-file (bsa-path "out/bsa-app.fasl")) t)))
  (0 nil t))
