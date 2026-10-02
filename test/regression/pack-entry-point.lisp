;;; `dotcl pack` without --toplevel calls the system's :entry-point.
;;;
;;; A system built as a program already says what to call: ASDF's :entry-point,
;;; the same thing PROGRAM-OP uses. pack ignored it, so leaving out --toplevel
;;; produced a tool that loaded the system and exited having called nothing,
;;; with exit code 0 and no output. Now the entry point is the default, and a
;;; pack with neither says so with a warning (a system that runs itself at load
;;; time needs neither, so it is not an error).
;;;
;;; NOTE: stay in the default load package (CL-USER); an (in-package ...) into a
;;; (:use :cl)-only package hides the framework's DEFTEST macro. Helpers are
;;; pep- prefixed. The fixture systems live in .asd files written to a temp dir
;;; rather than in this file, because a DEFSYSTEM inside a file the suite LOADs
;;; makes ASDF treat this file as the system's definition and read it again.

(require "asdf")

(defvar *pep-dir* "test/regression/.tmp-pack-entry-point/")

(defvar *pep-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *pep-core*
  (regression-child-core))

(defun pep-path (name)
  (namestring (merge-pathnames name (truename *pep-dir*))))

(defun pep-write (name text)
  (with-open-file (s (pep-path name)
                     :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (write-string text s)))

(defun pep-setup ()
  (dolist (d '("" "from/"))
    (ensure-directories-exist (merge-pathnames d *pep-dir*)))
  (pep-write "pep-exe.asd"
             "(defsystem \"pep-exe\" :version \"1.0.0\" :description \"d\"
  :author \"a\" :license \"MIT\" :entry-point \"pep-exe:main\"
  :components ((:file \"pep-exe\")))")
  (pep-write "pep-plain.asd"
             "(defsystem \"pep-plain\" :version \"1.0.0\" :description \"d\"
  :author \"a\" :license \"MIT\" :components ((:file \"pep-exe\")))")
  (pep-write "pep-exe.lisp"
             "(defpackage #:pep-exe (:use #:cl) (:export #:main))
(in-package #:pep-exe)
(defun main () 0)")
  ;; --dry-run checks the donor packages exist; their contents are not read.
  (pep-write "from/dotcl.99.0.0.nupkg" "")
  (pep-write "from/dotcl.any.99.0.0.nupkg" "")
  (pushnew (pathname (namestring (truename *pep-dir*)))
           asdf:*central-registry* :test #'equal))

(defun pep-pack (system &rest more)
  "Run `dotcl pack --dry-run` on SYSTEM. Returns (exit-code stdout stderr)."
  (pep-setup)
  (dotcl:run-process
   *pep-exe*
   (append (list "--asm" *pep-core* "--asd-search-path" (pep-path "")
                 "pack" "--system" system "--id" system "--command" system
                 "-o" (pep-path "out/") "--from" (pep-path "from/")
                 "--dotcl-version" "99.0.0" "--rids" "any" "--dry-run")
           more)))

(defun pep-says (result text)
  (and (search text (concatenate 'string (second result) (third result))) t))

;;; The metadata read carries the entry point.
(deftest-emitting-only pack-entry-point.read-from-the-asd
  (progn (pep-setup)
         (dotnet:invoke (dotnet:static "DotCL.DotclBuild" "ReadSystemMeta" "pep-exe" nil)
                        "EntryPoint"))
  "pep-exe:main")

;;; No --toplevel: the entry point is used, and pack says which it chose.
(deftest-emitting-only pack-entry-point.is-the-default-toplevel
  (let ((r (pep-pack "pep-exe")))
    (list (first r)
          (pep-says r "toplevel pep-exe:main (the system's :entry-point)")
          (pep-says r "warning")))
  (0 t nil))

;;; An explicit --toplevel wins, silently.
(deftest-emitting-only pack-entry-point.explicit-toplevel-wins
  (let ((r (pep-pack "pep-exe" "--toplevel" "pep-exe::other")))
    (list (first r) (pep-says r ":entry-point") (pep-says r "warning")))
  (0 nil nil))

;;; Neither: still packs, with a warning naming the system.
(deftest-emitting-only pack-entry-point.neither-warns
  (let ((r (pep-pack "pep-plain")))
    (list (first r)
          (pep-says r "warning: no --toplevel, and system pep-plain declares no :entry-point")))
  (0 t))
