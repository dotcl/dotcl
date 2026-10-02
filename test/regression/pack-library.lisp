;;; `dotcl pack --library`: an ASDF system as a NuGet package that another system
;;; names in a (:nuget ...) component.
;;;
;;; The packing itself is dotcl-nuget-asdf's (nuget-library-package.lisp checks
;;; the package's shape); this checks the command line around it: the options it
;;; needs, and the nuspec metadata it fills in the same way the tool path does
;;; (:license as a license expression, a README beside the .asd embedded).
;;;
;;; NOTE: stay in CL-USER, and keep the fixture in .asd files on disk: a
;;; DEFSYSTEM inside a file the suite LOADs makes ASDF treat this file as the
;;; system's definition.

(defvar *plb-exe*
  (or (ignore-errors (dotnet:static "System.Environment" "ProcessPath"))
      (error "cannot locate this process's executable")))

(defvar *plb-core* (regression-child-core))

(defvar *plb-dir*
  (let ((d (concatenate 'string (namestring (regression-temp-dir)) "/dotcl-plb/")))
    (ensure-directories-exist d)
    d))

(defun plb-write (name text)
  (let ((p (concatenate 'string *plb-dir* name)))
    (ensure-directories-exist p)
    (with-open-file (s p :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string text s))
    p))

(defun plb-setup ()
  (plb-write "plb-lib.asd"
             "(defsystem \"plb-lib\" :version \"2.0.1\" :author \"Someone <a@example.com>\"
  :description \"a library\" :license \"MIT\" :components ((:file \"plb\")))")
  (plb-write "plb.lisp" "(defpackage :plb (:use :cl)) (in-package :plb) (defun f () 1)")
  (plb-write "README.md" "# plb-lib"))

(defun plb-pack (&rest args)
  (plb-setup)
  (dotcl:run-process
   *plb-exe*
   (append (list "--asm" *plb-core* "--asd-search-path" *plb-dir* "pack" "--library")
           args)))

(defun plb-says (result text)
  (and (search text (concatenate 'string (second result) (third result))) t))

(defun plb-entries-and-nuspec (path)
  (let* ((zip (dotnet:static "System.IO.Compression.ZipFile" "OpenRead" path))
         (entries (dotnet:invoke zip "get_Entries"))
         (names (loop for i below (dotnet:invoke entries "get_Count")
                      collect (dotnet:invoke (dotnet:invoke entries "get_Item" i) "get_FullName")))
         (nuspec (let* ((e (dotnet:invoke zip "GetEntry" "plb-lib.nuspec"))
                        (r (dotnet:new "System.IO.StreamReader" (dotnet:invoke e "Open"))))
                   (prog1 (dotnet:invoke r "ReadToEnd") (dotnet:invoke r "Dispose")))))
    (dotnet:invoke zip "Dispose")
    (values (sort names #'string<) nuspec)))

;;; Only the system and the output directory are needed; the version comes from
;;; the .asd as on the tool path.
(deftest-emitting-only pack-library.writes-the-package
  (let* ((r (plb-pack "--system" "plb-lib" "-o" (concatenate 'string *plb-dir* "out/")))
         (path (concatenate 'string *plb-dir* "out/plb-lib.2.0.1.nupkg")))
    (multiple-value-bind (names nuspec) (and (probe-file path) (plb-entries-and-nuspec path))
      (list (first r)
            (plb-says r "pack: library id=plb-lib version=2.0.1")
            names
            (and nuspec (search "<license type=\"expression\">MIT</license>" nuspec) t)
            (and nuspec (search "<readme>README.md</readme>" nuspec) t)
            ;; the .asd's :author without the mail address, as on the tool path
            (and nuspec (search "<authors>Someone</authors>" nuspec) t))))
  (0 t
   ;; the output directory, inside the system's, is not carried
   ("README.md" "dotcl/plb-lib.systems" "dotcl/plb-lib/README.md"
    "dotcl/plb-lib/dotcl-build.sexp" "dotcl/plb-lib/plb-lib.asd" "dotcl/plb-lib/plb.lisp"
    "dotcl/plb-lib/plb.lisp.fasl" "plb-lib.nuspec")
   t t t))

(deftest-emitting-only pack-library.needs-an-output
  (let ((r (plb-pack "--system" "plb-lib")))
    (list (first r) (plb-says r "missing required option(s): -o/--output")))
  (2 t))

;;; --r2r compiles against the runtime images in the dotcl packages, so it needs
;;; --from, as on the tool path.
(deftest-emitting-only pack-library.needs-a-system
  (let ((r (plb-pack "-o" (concatenate 'string *plb-dir* "out/"))))
    (list (first r) (plb-says r "missing required option(s): --system")))
  (2 t))
