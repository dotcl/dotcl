;;; `dotcl pack` keeps the bundled ASDF when stock asdf.asd / uiop.asd are
;;; visible, and reports a .asd that fails to load as itself.
;;;
;;; A qlot or Quicklisp bundle often pins stock ASDF, so its asdf.asd sits in
;;; the source registry next to the application's dependencies. ASDF loads a
;;; registered asdf.asd of its own version "to allow loading from modified
;;; source", so the first .asd pack read rebuilt ASDF from sources that do not
;;; know dotcl. Reading the system's metadata was that first read, and it ran
;;; before the fasl build pinned ASDF; its failure was taken as "no metadata",
;;; and pack said only "missing required option(s): --version". The fixture's
;;; asdf.asd and uiop.asd stand in for the stock ones: same version as the
;;; bundled ASDF, and their only source signals, so loading them is visible.
;;;
;;; Each case runs the CLI in a child process, --dry-run where the point is the
;;; metadata step: pinning is image-wide, and this image has pinned already.
;;;
;;; NOTE: stay in the default load package (CL-USER); an (in-package ...) into a
;;; (:use :cl)-only package hides the framework's DEFTEST macro. Helpers are
;;; psa- prefixed. The fixture systems live in .asd files written to a temp dir
;;; rather than in this file, because a DEFSYSTEM inside a file the suite LOADs
;;; makes ASDF treat this file as the system's definition and read it again.

(require "asdf")

(defvar *psa-dir* "test/regression/.tmp-pack-stock-asdf/")

(defvar *psa-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *psa-core*
  (regression-child-core))

(defun psa-path (name)
  (namestring (merge-pathnames name (truename *psa-dir*))))

(defun psa-write (name text)
  (with-open-file (s (psa-path name)
                     :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (write-string text s)))

(defun psa-setup ()
  (dolist (d '("" "stock/" "app/" "bad/" "from/"))
    (ensure-directories-exist (merge-pathnames d *psa-dir*)))
  ;; Stand-ins for a bundle's stock ASDF and UIOP.
  (psa-write "stock/asdf.asd"
             "(defsystem \"asdf\" :components ((:file \"boom-asdf\")))")
  (psa-write "stock/version.lisp-expr"
             (format nil "~s~%" (asdf:asdf-version)))
  (psa-write "stock/boom-asdf.lisp" "(error \"stock asdf source was loaded\")")
  (psa-write "stock/uiop.asd"
             "(defsystem \"uiop\" :components ((:file \"boom-uiop\")))")
  (psa-write "stock/boom-uiop.lisp" "(error \"stock uiop source was loaded\")")
  ;; An application that depends on UIOP, as nearly every one does.
  (psa-write "app/psa-app.asd"
             "(defsystem \"psa-app\" :version \"1.2.3\" :description \"d\"
  :author \"a\" :license \"MIT\" :depends-on (\"uiop\")
  :components ((:file \"psa-app\")))")
  (psa-write "app/psa-app.lisp" "(defpackage #:psa-app (:use #:cl))")
  ;; A .asd that cannot be loaded.
  (psa-write "bad/psa-bad.asd" "(error \"deliberately broken definition\")")
  ;; --dry-run checks the donor packages exist; their contents are not read.
  (psa-write "from/dotcl.99.0.0.nupkg" "")
  (psa-write "from/dotcl.any.99.0.0.nupkg" ""))

(defun psa-pack (system &rest dirs)
  "Run `dotcl pack --dry-run` on SYSTEM with DIRS on the search path.
Returns (exit-code stdout stderr)."
  (psa-setup)
  (dotcl:run-process
   *psa-exe*
   (append (list "--asm" *psa-core*)
           (loop for d in dirs append (list "--asd-search-path" (psa-path d)))
           (list "pack" "--system" system "--id" system "--command" system
                 "-o" (psa-path "out/") "--from" (psa-path "from/")
                 "--dotcl-version" "99.0.0" "--rids" "any" "--dry-run"))))

(defun psa-says (result text)
  (and (search text (concatenate 'string (second result) (third result))) t))

;;; The stock sources are never loaded, and the version comes from the .asd.
(deftest-emitting-only pack-stock-asdf.bundled-asdf-is-kept
  (let ((r (psa-pack "psa-app" "stock/" "app/")))
    (list (first r)
          (psa-says r "source was loaded")
          (psa-says r "missing required option")
          (psa-says r "psa-app.1.2.3.nupkg")))
  (0 nil nil t))

;;; A .asd that signals is reported with its own error text.
(deftest-emitting-only pack-stock-asdf.asd-error-is-reported-as-itself
  (let ((r (psa-pack "psa-bad" "bad/")))
    (list (first r)
          (psa-says r "deliberately broken definition")
          (psa-says r "missing required option")))
  (1 t nil))

;;; A system nobody can find is said to be not found.
(deftest-emitting-only pack-stock-asdf.unknown-system-is-not-found
  (let ((r (psa-pack "psa-nowhere" "app/")))
    (list (first r)
          (psa-says r "system psa-nowhere not found")
          (psa-says r "missing required option")))
  (1 t nil))
