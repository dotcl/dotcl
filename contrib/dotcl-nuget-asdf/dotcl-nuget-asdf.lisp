;;; dotcl-nuget-asdf.lisp -- declare a NuGet dependency in a system definition.
;;;
;;; Usage:
;;;
;;;   (defsystem "my-app"
;;;     :defsystem-depends-on ("dotcl-nuget-asdf")
;;;     :serial t
;;;     :components ((:nuget "Newtonsoft.Json" :nuget-version "13.*")
;;;                  (:file "app")))
;;;
;;; A (:nuget ...) component resolves the package and registers its assemblies
;;; when the system is loaded, so app.lisp can name the types without the system
;;; having to call NUGET:REQUIRE from somewhere in its own code.
;;;
;;; Why a component rather than a :depends-on entry: ASDF's dependency syntax is
;;; a closed set (:feature, :version, :require), so a NuGet package cannot be
;;; spelled there. A component class is the extension point ASDF does offer, and
;;; it is what cffi-grovel uses for :cffi-grovel-file.
;;;
;;; The keywords are the ones NUGET:REQUIRE takes, with the component's name as
;;; the package id:
;;;
;;;   (:nuget "SkiaSharp" :nuget-version "2.88.7" :rid "win-arm64")
;;;   (:nuget "Newtonsoft.Json")                       ; latest stable
;;;   (:nuget "Some.Preview" :prerelease t)
;;;
;;; A declaration acts on a later load, maybe on another machine, so loading
;;; follows the project's lock file (see the header of the nuget contrib): an exact
;;; version is resolved and recorded there, a floating one ("13.*", or none at
;;; all) has to be recorded first by (nuget:restore), and loading stops with a
;;; message saying so until it is.
;;;
;;; Every (:nuget ...) a system reaches, through its own components and its
;;; dependencies', is resolved together when the system is loaded, so NuGet sees
;;; the whole set at once and unifies shared dependencies across it.
;;;
;;; Ordering is the system's business, as with any other component: put the
;;; :nuget component before the files that need it and mark the system :serial t,
;;; or name it in a :depends-on.

(require "asdf")
(require "dotcl-nuget")

(defpackage :dotcl-nuget-asdf
  (:use :cl)
  (:export #:nuget-package
           #:nuget-version #:nuget-source #:nuget-prerelease
           #:nuget-rid #:nuget-tfm
           #:system-nuget-components #:system-nuget-preamble
           #:resolve-system-for-rid))

(in-package :dotcl-nuget-asdf)

(defclass nuget-package (asdf:component)
  ;; VERSION-SPEC, not VERSION: ASDF's own component slot is named VERSION with
  ;; initarg :VERSION, so a slot of that name here would be the same effective
  ;; slot, and DEFSYSTEM consumes :VERSION before the component is made anyway --
  ;; it runs the value through ASDF's version syntax. An exact NuGet version
  ;; ("2.88.7") survives that and a floating one ("13.*") does not, which is the
  ;; worst shape a mistake can have: right in the easy case, silently NIL in the
  ;; interesting one. Hence a keyword of its own, and the check in PERFORM.
  ((version-spec :initarg :nuget-version :initform nil :reader nuget-version)
   (source :initarg :source :initform nil :reader nuget-source)
   (prerelease :initarg :prerelease :initform nil :reader nuget-prerelease)
   (rid :initarg :rid :initform nil :reader nuget-rid)
   (tfm :initarg :tfm :initform nil :reader nuget-tfm))
  (:documentation
   "A NuGet package a system depends on. The component's name is the package id;
the slots are the axes NUGET:RESOLVE identifies a package by."))

;;; ASDF resolves a component type by looking the name up in its own package, so
;;; the class has to be reachable as ASDF::NUGET. cffi-grovel does the same thing
;;; for :cffi-grovel-file (grovel/asdf.lisp). The name claims a symbol in a shared
;;; package, which is why it says what it is rather than something generic.
(setf (find-class 'asdf::nuget) (find-class 'nuget-package))

;;; A NuGet package is not a file: it has no source to read and produces nothing
;;; on disk that ASDF should track. Saying so keeps ASDF from computing an input
;;; or an output pathname from the component's name, which is a package id.
;;; DEFSYSTEM computes a pathname for every component as it parses, and reaches
;;; for the component's file type to do it. The methods that answer are on
;;; file-component and parent-component, and this is neither.
(defmethod asdf:source-file-type ((c nuget-package) system)
  (declare (ignore system))
  nil)

;;; The component's name is a package id, and ASDF would otherwise parse it as a
;;; relative pathname: "Newtonsoft.Json" happens to read as a name and a type, so
;;; it produces a nonsense path quietly, and a name without a dot -- or a system
;;; with no pathname of its own -- makes it an error instead. Answer the parent's
;;; own directory and let nothing depend on it.
(defmethod asdf:component-relative-pathname ((c nuget-package))
  (make-pathname :directory nil :name nil :type nil))

(defmethod asdf:input-files ((o asdf:operation) (c nuget-package))
  nil)

(defmethod asdf:output-files ((o asdf:operation) (c nuget-package))
  (values nil t))

(defun %require-arguments (c)
  "The keyword arguments for NUGET:REQUIRE, omitting the ones left unset so the
defaults in NUGET:RESOLVE apply."
  (let ((args '()))
    (when (nuget-tfm c) (setf args (list* :tfm (nuget-tfm c) args)))
    (when (nuget-rid c) (setf args (list* :rid (nuget-rid c) args)))
    (when (nuget-prerelease c) (setf args (list* :prerelease (nuget-prerelease c) args)))
    (when (nuget-source c) (setf args (list* :source (nuget-source c) args)))
    (when (nuget-version c) (setf args (list* :version (nuget-version c) args)))
    args))

;;; Resolving is what LOAD-OP means for this component. COMPILE-OP has nothing to
;;; do: there is no source to compile, and a file that needs the assemblies at
;;; compile time gets them because its own COMPILE-OP depends on this LOAD-OP.
(defmethod asdf:perform ((o asdf:compile-op) (c nuget-package))
  nil)

(defmethod asdf:perform ((o asdf:load-op) (c nuget-package))
  ;; :VERSION went to ASDF's slot, so this component was given a version that
  ;; will never reach NuGet. Say so rather than resolving the wrong package.
  (when (and (null (nuget-version c)) (asdf:component-version c))
    (error "~S: :VERSION is ASDF's own component version, and a NuGet version ~
spec does not belong there. Write :NUGET-VERSION ~S instead."
           (asdf:component-name c) (asdf:component-version c)))
  (%declare (list c)))

(defun %request (c)
  (nuget::make-req (asdf:component-name c)
                   (nuget::%spec (nuget-version c) (nuget-prerelease c))
                   (nuget-source c)
                   :declared))

(defun %declare (components &key rid)
  "Resolve COMPONENTS as declarations, grouped by the platform each asks for.
RID, when given, overrides it for the components that do not name their own."
  (let ((groups '()))
    (dolist (c components)
      (let* ((key (list (or (nuget-rid c) rid) (nuget-tfm c)))
             (g (assoc key groups :test #'equal)))
        (if g (push c (cdr g)) (push (list key c) groups))))
    (loop for ((r tfm) . cs) in (nreverse groups)
          do (nuget::%ensure (mapcar #'%request (reverse cs))
                             :mode :declared :rid r :tfm tfm))
    (%forget-missing-systems)))

;;; A package can carry ASDF systems (see the end of this file), and resolving it
;;; puts their directories on the central registry. But ASDF remembers, for the
;;; rest of the operation, that a system it looked for was not there -- and the
;;; walk that collected the declarations has just looked for every dependency,
;;; including the ones those packages provide. Forget the misses so the plan
;;; looks again.
(defun %forget-missing-systems ()
  (let* ((session-var (find-symbol "*ASDF-SESSION*" "ASDF/SESSION"))
         (session (and session-var (symbol-value session-var))))
    (when session
      (let ((cache (funcall (find-symbol "SESSION-CACHE" "ASDF/SESSION") session))
            (stale '()))
        (maphash (lambda (key value)
                   (when (and (consp key) (eq (car key) 'asdf:find-system)
                              (null (first value)))
                     (push key stale)))
                 cache)
        (dolist (key stale) (remhash key cache))))))

;;; A package can carry ASDF systems (see the end of this file), and names the ones
;;; it was made for in dotcl/<id>.systems. Declaring the package is then enough:
;;; its LOAD-OP depends on loading those systems, so a system that lists the
;;; package needs no :depends-on of its own for them -- ordered as any other
;;; component, by :serial t or by naming the (:nuget ...) component.
(defun %package-systems (c)
  "The systems package C was made for, as listed in its layout, or NIL."
  (let* ((state (nuget::%state (or (nuget-rid c) (nuget::%current-rid))
                               (or (nuget-tfm c) (nuget::%current-tfm))))
         (dir (nuget::state-out-dir state))
         (text (and dir (nuget::%read-text
                         (nuget::%combine dir "dotcl"
                                          (format nil "~(~A~).systems" (asdf:component-name c)))))))
    (when text
      (getf (ignore-errors
             (with-standard-io-syntax
               (let ((*read-eval* nil) (*package* (find-package "DOTCL-NUGET-ASDF")))
                 (read-from-string text))))
            :systems))))

(defmethod asdf:component-depends-on ((o asdf:load-op) (c nuget-package))
  (let ((systems (remove nil (mapcar (lambda (n) (asdf:find-system n nil))
                                     (%package-systems c)))))
    (if systems
        (cons (cons 'asdf:load-op systems) (call-next-method))
        (call-next-method))))

;;; Before a system is loaded, everything it declares -- through its own
;;; components and its dependencies' -- is resolved in one go. Performing each
;;; component on its own would resolve the set one package at a time, and a later
;;; package that needs a newer version of something an earlier one already
;;; registered would then be a conflict, where resolving them together lets NuGet
;;; pick the version that serves both. Each component's own LOAD-OP still runs,
;;; and finds itself already answered.
(defmethod asdf:operate :before ((o asdf:load-op) (s asdf:system) &key &allow-other-keys)
  (let ((cs (system-nuget-components s)))
    (when cs (%declare cs))))

;;; --- Carrying the declaration into a built artifact -------------------------
;;;
;;; A (:nuget ...) component does its work in PERFORM, and PERFORM runs when the
;;; system is LOADED. `dotcl build` and `dotcl pack` do not load the system: they
;;; concatenate its sources and compile the result, and ASDF's concatenation
;;; gathers CL-SOURCE-FILE components only. A component that is not a file
;;; contributes nothing, so the declaration was dropped -- silently, and exactly
;;; in the direction that matters, since the built artifact is what runs on a
;;; machine with no .NET SDK.
;;;
;;; So the declaration has to be turned back into source. SYSTEM-NUGET-PREAMBLE
;;; produces the forms, and the build prepends them to the concatenated file:
;;; then the shipped program asks for its packages itself, and finds them in the
;;; layout `dotcl pack` bundled beside it (NUGET:BUNDLED-ROOT).

(defun system-nuget-components (system)
  "Every (:nuget ...) component reachable from SYSTEM, dependencies first.

Other systems are walked too: a library may declare a package of its own, and an
application that depends on that library needs the package for the same reason.

The walk is by hand rather than through ASDF:REQUIRED-COMPONENTS because that one
makes a plan, and planning re-checks whether each system's definition is still
current -- which asks to re-read the defining file. A system defined by calling
DEFSYSTEM (a test, a REPL) has no file to re-read, so planning it is an error
where walking it is not. Nothing here needs an operation anyway: the question is
what the system declares, not what would be done about it."
  (let ((seen '())
        (out '()))
    (labels ((walk-component (c)
               (cond ((typep c 'nuget-package) (push c out))
                     ;; SYSTEM is a MODULE, so this reaches nested (:module ...)
                     ;; components and the system's own children alike.
                     ((typep c 'asdf:module)
                      (mapc #'walk-component (asdf:component-children c)))))
             (walk-system (s)
               (unless (member s seen :test #'eq)
                 (push s seen)
                 (dolist (d (asdf:system-depends-on s))
                   (let ((ds (ignore-errors
                              (asdf/find-component:resolve-dependency-spec s d))))
                     (when ds (walk-system ds))))
                 (walk-component s))))
      (walk-system (if (typep system 'asdf:system) system (asdf:find-system system))))
    (nreverse out)))

(defun %preamble-form (c)
  "The source text of the NUGET:REQUIRE call for component C.

Written with FIND-SYMBOL rather than NUGET:REQUIRE so the form can be read in an
image where the NUGET package does not exist yet -- which is the situation in the
concatenated file, where the (REQUIRE \"dotcl-nuget\") that creates it is part of
the same form. The module is named by the contrib's own name: the old name
\"nuget\" is refused with a message pointing at the new one, so a preamble that
spelled it that way failed at the start of every built program."
  (with-output-to-string (s)
    (format s "(eval-when (:compile-toplevel :load-toplevel :execute)~%")
    (format s "  (cl:require \"dotcl-nuget\")~%")
    (format s "  (funcall (find-symbol \"REQUIRE\" \"NUGET\") ~S" (asdf:component-name c))
    (loop for (k v) on (%require-arguments c) by #'cddr
          do (format s " ~S ~S" k v))
    (format s "))")))

(defun resolve-system-for-rid (system rid)
  "Lay out every package SYSTEM declares for RID, and report what would not lay out.

Returns a list of (PACKAGE-NAMES . MESSAGE) when they failed, empty when they
worked. The packages are resolved together, as a load would, so one failure is
reported for the set. A failure is not fatal: a package can legitimately have
nothing for a platform, and the answer to that is to ship without the layout and
let it resolve on the target -- not to refuse to build for that platform.

A component that names its own :RID is left alone. It asked for a specific
platform's assets, and packaging for a different one does not change that."
  (let ((cs (remove-if #'nuget-rid (system-nuget-components system))))
    (when cs
      (handler-case (progn (%declare cs :rid rid) '())
        (error (e)
          (list (cons (format nil "~{~A~^, ~}" (mapcar #'asdf:component-name cs))
                      (princ-to-string e))))))))

(defun system-nuget-preamble (system)
  "The forms that make a built artifact resolve SYSTEM's declared NuGet packages,
as one string, or NIL when the system declares none.

Order follows the components: a system that declares two packages asks for them
in the order it wrote them, as LOAD-OP would."
  (let ((cs (system-nuget-components system)))
    (when cs
      (format nil "~{~A~%~}" (mapcar #'%preamble-form cs)))))

;;; --- Shipping a system as a NuGet package --------------------------------
;;;
;;; The other direction: a system's .asd and sources go into a .nupkg under
;;; dotcl/<system>/, so that another system can name the package in a (:nuget ...)
;;; component and load the system with ASDF. Resolving the package copies that
;;; tree into the layout and puts it on ASDF's central registry (see the nuget
;;; contrib), and the order, the dependencies and the conditionals stay ASDF's
;;; business: no manifest of our own.
;;;
;;; Not under lib/: that folder means "reference these assemblies", and a .NET
;;; project that referenced the package would then be handed compiled Lisp as
;;; if it were a library for it. Under dotcl/, NuGet treats the package as one
;;; with no assets for a .NET project, and only dotcl looks inside.
;;;
;;; The package's own NuGet dependencies are the system's (:nuget ...)
;;; components, so NuGet brings a library's packages -- including other Lisp
;;; libraries -- along with it.

(defun %xml (s) (nuget::%xml-escape (princ-to-string s)))

(defun %nuspec (id version authors description dependencies)
  (format nil "<?xml version=\"1.0\" encoding=\"utf-8\"?>~%~:
<package xmlns=\"http://schemas.microsoft.com/packaging/2013/05/nuspec.xsd\">~%~:
  <metadata>~%~:
    <id>~A</id>~%~:
    <version>~A</version>~%~:
    <authors>~A</authors>~%~:
    <description>~A</description>~%~:
    <dependencies>~%~:
      <group>~%~:
~{        <dependency id=\"~A\" version=\"~A\" />~%~}~:
      </group>~%~:
    </dependencies>~%~:
  </metadata>~%~:
</package>~%"
          (%xml id) (%xml version) (%xml authors) (%xml description)
          (loop for (i . v) in dependencies append (list (%xml i) (%xml v)))))

(defun %provided-by-dotcl-p (system)
  "True for a system dotcl itself supplies -- ASDF and UIOP, and the contribs that
REQUIRE loads -- which a package therefore neither needs nor should carry."
  (or (typep system 'asdf:require-system)
      (member (asdf:primary-system-name system) '("asdf" "uiop" "asdf-package-system")
              :test #'string-equal)
      (null (asdf:system-source-file system))))

(defun %closure (roots)
  "ROOTS and every system they depend on (:depends-on and :defsystem-depends-on),
leaving out what dotcl supplies; dependencies first."
  (let ((seen '()) (out '()))
    (labels ((walk (s)
               (unless (or (member s seen :test #'eq) (%provided-by-dotcl-p s))
                 (push s seen)
                 (dolist (d (append (asdf:system-defsystem-depends-on s)
                                    (asdf:system-depends-on s)))
                   (let ((ds (ignore-errors
                              (asdf/find-component:resolve-dependency-spec s d))))
                     (when (typep ds 'asdf:system) (walk ds))))
                 (push s out))))
      (mapc #'walk roots))
    (nreverse out)))

(defun %groups (systems)
  "SYSTEMS grouped by the directory their .asd is in, as (name directory . systems).
A group is what lands under dotcl/<name>/; NAME is the primary system name of the
first system defined there, made unique."
  (let ((groups '()) (names '()))
    (dolist (s systems)
      (let* ((dir (namestring (asdf:system-source-directory s)))
             (g (find dir groups :key #'second :test #'string=)))
        (if g
            (setf (cddr g) (append (cddr g) (list s)))
            (let* ((base (asdf:primary-system-name s))
                   (name (loop for i from 1
                               for n = (if (= i 1) base (format nil "~A-~D" base i))
                               unless (member n names :test #'string-equal) return n)))
              (push name names)
              (setf groups (append groups (list (list* name dir (list s)))))))))
    groups))

(defun %relative (file dir what)
  (unless (and (> (length file) (length dir)) (string= dir file :end2 (length dir)))
    (error "~A: ~A is outside the directory ~A, so it has no place in the package."
           what file dir))
  (subseq file (length dir)))

(defun %source-files (system)
  (let ((out '()))
    (labels ((walk (c)
               (cond ((typep c 'asdf:module) (mapc #'walk (asdf:component-children c)))
                     ((typep c 'asdf:file-component) (push c out)))))
      (walk system))
    (nreverse out)))

(defun %tree-files (dir skip)
  "Every file under DIR, leaving out names starting with a dot (.git and the like),
compiled files, and the directories in SKIP."
  (let ((out '()))
    (labels ((walk (d)
               (dolist (f (nuget::%dir-files d "*"))
                 (let ((name (dotnet:static "System.IO.Path" "GetFileName" f)))
                   (unless (or (char= (char name 0) #\.)
                               (search ".fasl" name))
                     (push f out))))
               (dolist (sub (nuget::%subdirs d))
                 (let ((name (dotnet:static "System.IO.Path" "GetFileName" sub))
                       (sub/ (concatenate 'string (substitute #\/ #\\ sub) "/")))
                   (unless (or (char= (char name 0) #\.)
                               (member sub/ skip :test #'string=))
                     (walk sub))))))
      (walk dir))
    (nreverse out)))

(defun %group-files (dir systems skip)
  "The files a group ships, as (absolute-path . relative-path): the whole tree under
DIR but for the directories in SKIP (other groups'). A .asd can read anything
beside it -- :version (:read-file-form \"data/version-string.sexp\") is common --
so the files the systems name as components are not enough."
  (mapcar (lambda (f) (cons f (%relative (substitute #\/ #\\ f) dir
                                         (asdf:component-name (first systems)))))
          (%tree-files dir skip)))

(defun %fasl-directory (fasl)
  "The directory FASL is in, as a namestring that keeps its device (the drive on
Windows). DIRECTORY-NAMESTRING leaves the device out, so the search for siblings
ran on the current drive, and a sibling found there had a shorter name than FASL."
  (namestring (make-pathname :name nil :type nil :version nil :defaults (pathname fasl))))

(defun %sibling-entry (rel fasl sib)
  "The relative path SIB, an R2R sibling of FASL, ships under when FASL ships as
REL: REL followed by what SIB's file name adds to FASL's."
  (let ((fasl-name (dotnet:static "System.IO.Path" "GetFileName" fasl))
        (sib-name (dotnet:static "System.IO.Path" "GetFileName" sib)))
    (concatenate 'string rel (subseq sib-name (length fasl-name)))))

(defun %group-fasls (dir systems)
  "The compiled files of SYSTEMS (already compiled), as (values ENTRIES PAIRS).
ENTRIES are (absolute-path . relative-path): each source's fasl ships as
<source>.fasl beside it, and an R2R sibling ASDF left beside the fasl as
<source>.fasl.r2r-<rid>, the name the loader looks for. PAIRS are
(fasl-relative-path . source-relative-path), for the consumer to place them."
  (let ((entries '()) (pairs '()))
    (dolist (s systems)
      (dolist (c (%source-files s))
        (when (typep c 'asdf:cl-source-file)
          (let* ((fasl (namestring (first (asdf:output-files 'asdf:compile-op c))))
                 (src-rel (%relative (namestring (asdf:component-pathname c)) dir
                                     (asdf:component-name s)))
                 (rel (concatenate 'string src-rel ".fasl")))
            (when (probe-file fasl)
              (push (cons fasl rel) entries)
              (push (cons rel src-rel) pairs)
              (dolist (sib (nuget::%dir-files
                            (%fasl-directory fasl)
                            (format nil "~A.r2r-*"
                                    (dotnet:static "System.IO.Path" "GetFileName" fasl))))
                ;; A sibling older than its fasl was made from an earlier
                ;; compile; the loader would ignore it, and shipping it
                ;; re-dated would make it be used.
                (unless (nuget::%newer-p fasl sib)
                  (push (cons sib (%sibling-entry rel fasl sib))
                        entries))))))))
    (values (nreverse entries) (nreverse pairs))))

(defun %write-r2r-siblings (systems)
  "Give every compiled file of SYSTEMS a ReadyToRun sibling for this platform,
unless it has a fresh one. Returns how many were written."
  (let ((n 0))
    (dolist (s systems n)
      (dolist (c (%source-files s))
        (when (typep c 'asdf:cl-source-file)
          (let ((fasl (namestring (first (asdf:output-files 'asdf:compile-op c)))))
            (when (and (probe-file fasl)
                       (null (remove-if (lambda (sib) (nuget::%newer-p fasl sib))
                                        (nuget::%dir-files
                                         (%fasl-directory fasl)
                                         (format nil "~A.r2r-*"
                                                 (dotnet:static "System.IO.Path" "GetFileName" fasl))))))
              (when (dotcl:write-r2r-sibling fasl) (incf n)))))))))

(defun %build-record (pairs)
  "What a group's fasls were built by -- they are used only by the same compiler --
and which source each belongs to."
  (with-standard-io-syntax
    (prin1-to-string (list :dotcl-version (lisp-implementation-version)
                           :core (funcall (find-symbol "%CORE-GENERATION" "DOTCL"))
                           :fasls pairs))))

(defun %package-dependencies (systems)
  "The (:nuget ...) components of SYSTEMS as nuspec dependencies. A dependency in a
package states a lower bound or a range; a floating version cannot be written
there, and no version at all would mean any version, so both are refused."
  (let ((out '()))
    (dolist (s systems (nreverse out))
      (dolist (c (asdf:component-children s))
        (when (typep c 'nuget-package)
          (let ((v (nuget-version c)))
            (when (or (null v) (find #\* v))
              (error "~A: the package declares ~A with ~:[no version~;~:*~S~]. A package's ~
dependencies need a version or a range; floating is something the consumer's lock ~
file decides." (asdf:component-name s) (asdf:component-name c) v))
            (unless (assoc (asdf:component-name c) out :test #'string-equal)
              (push (cons (asdf:component-name c) v) out))))))))

(defun %zip-add (zip from rel)
  ;; A zip time has no zone and NuGet reads it as UTC, so write the UTC wall
  ;; clock: local time would extract hours off.
  (let ((e (dotnet:invoke zip "CreateEntry" rel)))
    (dotnet:invoke e "set_LastWriteTime"
                   (dotnet:new "System.DateTimeOffset"
                               (dotnet:static "System.IO.File" "GetLastWriteTimeUtc" from)
                               (dotnet:static "System.TimeSpan" "Zero")))
    (let ((to (dotnet:invoke e "Open"))
          (in (dotnet:static "System.IO.File" "OpenRead" from)))
      (unwind-protect (dotnet:invoke in "CopyTo" to)
        (dotnet:invoke in "Dispose")
        (dotnet:invoke to "Dispose")))))

(defun %pack-library (systems output-directory
                      &key id version authors description (fasl t) (dependencies t) r2r)
  "Write SYSTEMS (a system or a list) as one NuGet package into OUTPUT-DIRECTORY
and return its path.

With DEPENDENCIES (the default) the package also carries every system they
depend on that dotcl does not supply, so it loads with nothing else installed.
Systems are grouped by the directory of their .asd, each group under
dotcl/<name>/, and dotcl/<id>.systems names SYSTEMS -- what a consumer's
(:nuget ...) component loads. The (:nuget ...) components of the packed systems
become the package's own dependencies.

ID defaults to the first system's name, VERSION to its version, AUTHORS and
DESCRIPTION to its :author and :description.

With FASL (the default) the systems are loaded first, which compiles them, and
each group's fasls -- and any R2R siblings beside them -- go in next to the
sources, with dotcl-build.sexp saying which compiler made them. A consumer
running that compiler loads them without compiling; any other compiles the
sources, which are always there.

With R2R, each fasl first gets a ReadyToRun sibling for this platform, compiled
against the running dotcl -- the same dotcl the consumer has to run to use the
fasls at all. Siblings for other platforms are the command line's business
(`dotcl pack --library --r2r --from`)."
  (let* ((roots (mapcar (lambda (s) (if (typep s 'asdf:system) s (asdf:find-system s)))
                        (if (listp systems) systems (list systems))))
         (first-root (first roots))
         (name (asdf:component-name first-root))
         (id (or id name))
         (version (or version (asdf:component-version first-root)
                      (error "~A: no :version, and none given." name)))
         (authors (or authors (asdf:system-author first-root) "unknown"))
         (description (or description (asdf:system-description first-root) name))
         (out (namestring (ensure-directories-exist
                           (uiop:ensure-directory-pathname output-directory))))
         (path (nuget::%combine out (format nil "~(~A~).~A.nupkg" id version)))
         (temp (nuget::%temp-project-dir))
         (nuspec-path (nuget::%combine temp (format nil "~A.nuspec" id)))
         (roots-path (nuget::%combine temp "systems"))
         (packed (if dependencies (%closure roots) roots)))
    (when fasl (mapc #'asdf:load-system roots))
    (when (and fasl r2r)
      (%write-r2r-siblings packed))
    (nuget::%write-text nuspec-path
                        (%nuspec id version authors description
                                 (%package-dependencies packed)))
    (nuget::%write-text roots-path
                        (with-standard-io-syntax
                          (prin1-to-string
                           (list :systems (mapcar #'asdf:component-name roots)))))
    (when (probe-file path) (delete-file path))
    (let ((zip (dotnet:static "System.IO.Compression.ZipFile" "Open" path
                              (dotnet:static "System.IO.Compression.ZipArchiveMode" "Create"))))
      (unwind-protect
           (progn
             (%zip-add zip nuspec-path (format nil "~A.nuspec" id))
             (%zip-add zip roots-path (format nil "dotcl/~(~A~).systems" id))
             (loop with groups = (%groups packed)
                   for (gname dir . group) in groups
                   for prefix = (format nil "dotcl/~A/" gname)
                   ;; other groups' directories, and the output directory: a
                   ;; package written into the project would otherwise carry
                   ;; the packages written there before it
                   for skip = (cons (namestring (truename out)) (remove dir (mapcar #'second groups) :test #'string=))
                   do (loop for (from . rel) in (%group-files dir group skip)
                            do (%zip-add zip from (concatenate 'string prefix
                                                               (substitute #\/ #\\ rel))))
                      (when fasl
                        (multiple-value-bind (entries pairs) (%group-fasls dir group)
                          (when entries
                            (loop for (from . rel) in entries
                                  do (%zip-add zip from (concatenate 'string prefix
                                                                     (substitute #\/ #\\ rel))))
                            (let ((record (nuget::%combine temp (format nil "~A.record" gname))))
                              (nuget::%write-text record (%build-record pairs))
                              (%zip-add zip record
                                        (concatenate 'string prefix "dotcl-build.sexp"))))))))
        (dotnet:invoke zip "Dispose")))
    path))

(provide "dotcl-nuget-asdf")
