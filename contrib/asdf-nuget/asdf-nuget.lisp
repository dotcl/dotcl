;;; asdf-nuget.lisp -- declare a NuGet dependency in a system definition.
;;;
;;; Usage:
;;;
;;;   (defsystem "my-app"
;;;     :defsystem-depends-on ("asdf-nuget")
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
;;; follows the project's lock file (see the header of the dotnet-nuget contrib): an exact
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
;;;
;;; The same module packs a system as a NuGet package (`dotcl pack --library`,
;;; below), and checks whether the fasls such a package ships can be right on
;;; another machine (CHECK-PORTABILITY-OP, at the end of this file):
;;;
;;;   (asdf:operate 'asdf-nuget:check-portability-op "my-lib")

(require "asdf")
(require "dotnet-nuget")

(defpackage :asdf-nuget
  (:use :cl)
  (:export #:nuget-package
           #:nuget-version #:nuget-source #:nuget-prerelease
           #:nuget-rid #:nuget-tfm
           #:system-nuget-components #:system-nuget-preamble
           #:resolve-system-for-rid
           #:check-portability-op #:check-portability #:report-portability
           #:portability-finding #:finding-file #:finding-line #:finding-kind
           #:finding-detail #:blocking-kind-p
           #:fasl-portability-warning #:fasl-portability-warning-systems
           #:fasl-portability-warning-findings
           #:*portability-targets* #:*portability-base-features*
           #:*portability-limitations*))

(in-package :asdf-nuget)

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
               (let ((*read-eval* nil) (*package* (find-package "ASDF-NUGET")))
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

;;; A fasl a package shipped, placed where ASDF looks (see NUGET::%PLACE-FASLS),
;;; was compiled by whichever dotcl made the package. When loading it fails --
;;; a runtime member it calls is gone, it references a newer DotCL.Runtime than
;;; this one, .NET refuses the assembly -- the source beside it is compiled and
;;; loaded instead, as if no fasl had been shipped.
(defmethod asdf:perform :around ((o asdf:load-op) (c asdf:cl-source-file))
  (let ((fasl (first (asdf:input-files o c))))
    (if (not (nuget::%placed-fasl-p fasl))
        (call-next-method)
        (handler-case (call-next-method)
          (error (e)
            (let ((why (princ-to-string e)))
              (format *error-output* "~&; nuget: the shipped fasl for ~A did not load (~A); ~
compiling its source instead~%"
                      (namestring (asdf:component-pathname c))
                      (subseq why 0 (or (position #\Newline why) (length why)))))
            (nuget::%remove-placed (namestring fasl))
            (asdf:perform (asdf:make-operation 'asdf:compile-op) c)
            (call-next-method))))))

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
concatenated file, where the (REQUIRE \"dotnet-nuget\") that creates it is part of
the same form. The module is named by the contrib's own name: the old name
\"nuget\" is refused with a message pointing at the new one, so a preamble that
spelled it that way failed at the start of every built program."
  (with-output-to-string (s)
    (format s "(eval-when (:compile-toplevel :load-toplevel :execute)~%")
    (format s "  (cl:require \"dotnet-nuget\")~%")
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
  "What a group's fasls were built by and on -- the dotcl, its compiler, and the
OS and CPU features -- and which source each belongs to."
  (with-standard-io-syntax
    (prin1-to-string (list :dotcl-version (lisp-implementation-version)
                           :core (funcall (find-symbol "%CORE-GENERATION" "DOTCL"))
                           :platform (nuget::%platform-features)
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

With FASL (the default) the systems are loaded first, which compiles them, their
sources are checked as CHECK-PORTABILITY-OP does (a FASL-PORTABILITY-WARNING when
something is found; the package is written all the same), and each group's fasls -- and any R2R siblings beside them -- go in next to the
sources, with dotcl-build.sexp saying which dotcl made them and on which OS and
CPU. A consumer on the same OS and CPU loads them without compiling, also under
another dotcl (after a NUGET:FASL-VERSION-MISMATCH warning); on another platform
it compiles the sources, which are always there.

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
    (when fasl
      (mapc #'asdf:load-system roots)
      (%warn-unportable packed))
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

;;; --- Checking that a system's fasls can travel -----------------------------
;;;
;;; A fasl is .NET IL and runs on any OS and CPU, but what goes into it is
;;; decided before the IL: reader conditionals (#+windows, #-arm64) are decided
;;; when the source is read, and code that runs at compile time (EVAL-WHEN
;;; :COMPILE-TOPLEVEL, macro expanders, #., DEFCONSTANT) can look at the machine
;;; and leave the answer in the fasl as a constant. CHECK-PORTABILITY-OP reads a
;;; system's source files -- the ones ASDF lists as its components -- as text and
;;; reports those places, with file and line.
;;;
;;; It reads; it does not load, compile or run anything, so it cannot see
;;; everything. What it does not see is in *PORTABILITY-LIMITATIONS* and printed
;;; with every report. docs/portable-fasls.md has the guidelines it checks against.

;;; ------------------------------------------------------------------
;;; Targets: the platforms one fasl is expected to run on.

(defparameter *portability-base-features*
  '(:dotcl :common-lisp :net :unicode :package-local-nicknames :thread-support
    :dotcl-emit :64-bit :little-endian
    :asdf :asdf2 :asdf3 :asdf3.1 :asdf3.2 :asdf3.3)
  "Features every target has: what dotcl puts in *FEATURES* on all the
platforms below, plus the ones ASDF adds. Features a library pushes itself are
not known and are taken to be absent.")

(defparameter *portability-targets*
  '(("win-x64" :windows :win32 :os-windows :x86-64)
    ("win-arm64" :windows :win32 :os-windows :arm64)
    ("linux-x64" :linux :unix :os-unix :x86-64)
    ("linux-arm64" :linux :unix :os-unix :arm64)
    ("osx-x64" :macos :darwin :unix :bsd :os-unix :os-macosx :x86-64)
    ("osx-arm64" :macos :darwin :unix :bsd :os-unix :os-macosx :arm64))
  "Each entry: a .NET runtime identifier and the OS and CPU features dotcl
(and UIOP) have on it. A reader conditional is platform-dependent when it
evaluates differently on two of these.")

(defparameter *platform-feature-names*
  '("WINDOWS" "WIN32" "WIN64" "MSWINDOWS" "MINGW32" "MINGW64" "CYGWIN"
    "DARWIN" "MACOS" "MACOSX" "OS-MACOSX" "LINUX" "UNIX" "BSD" "FREEBSD"
    "OPENBSD" "NETBSD" "DRAGONFLY" "SUNOS" "SOLARIS" "ANDROID" "IOS" "HAIKU"
    "OS-WINDOWS" "OS-UNIX" "OS-HAIKU"
    "X86-64" "X86_64" "AMD64" "X86" "X8664" "I386" "I686" "PC386"
    "ARM64" "AARCH64" "ARM" "ARMV7" "PPC" "PPC64" "POWERPC" "SPARC" "MIPS"
    "RISCV" "RISCV64" "LOONGARCH64" "S390X"
    "LITTLE-ENDIAN" "BIG-ENDIAN" "64-BIT" "32-BIT" "X86-64-TARGET"
    "X8664-TARGET" "X8632-TARGET" "ARM-TARGET" "ARM64-TARGET" "DARWIN-TARGET"
    "LINUX-TARGET" "WINDOWS-TARGET" "WIN64-TARGET" "FREEBSD-TARGET"
    "DARWINPPC-TARGET" "LINUX-HOST" "DARWIN-HOST" "WINDOWS-HOST")
  "Names that say something about the OS or CPU. Used to tell a reader
conditional that mentions the platform but reads the same on every target
(#+64-bit, #+(and sbcl darwin)) from one that does not mention it at all.")

(defparameter *portability-limitations*
  '("Sources are read as text. Nothing is loaded or run, so code reached only through a function a macro calls, a reader macro, or a library's own feature that it pushes at compile time is not followed (the push itself is reported)."
    "Features pushed by libraries are taken to be absent on every target, so #+my-lib-windows after a compile-time push is not reported; the push is."
    "Custom read syntax (#?, #/, @annotations, ...) is skipped character by character and can throw the reader off for the rest of a form."
    "Files are the system's components as ASDF has them: the .asd was read on this machine, so a component it chooses with its own reader conditional (#+windows inside :components) is seen as it reads here. :IF-FEATURE is evaluated for every target."
    "A compile-time probe inside EVAL-WHEN is reported even when the value is computed again at load time; whether anything depends on the compile-time value is not known.")
  "What the check cannot see. Printed with every report.")

(defun %feature-name (x)
  (string-upcase (if (symbolp x) (symbol-name x) x)))

(defparameter *all-mask* (1- (ash 1 (length *portability-targets*))))

(defun %target-has-p (target name)
  (or (member name *portability-base-features* :key #'%feature-name :test #'string=)
      (member name (cdr target) :key #'%feature-name :test #'string=)))

(defun %platform-name-p (name)
  (member name *platform-feature-names* :test #'string=))

;;; ------------------------------------------------------------------
;;; A reader of source text into a tree of nodes. It does not intern
;;; symbols, so any package prefix and any symbol reads.

(defstruct (node (:constructor %make-node))
  kind        ; :list :token :string :prefix :readeval :other
  (start 0)
  name        ; :token -- upcased name without package prefix
  text        ; :prefix -- the prefix ("'" "`" "," ",@" "#'"); :string -- contents
  children    ; :list -- list of nodes; :prefix/:readeval -- one-element list
  (mask 0))   ; targets on which this datum is read

(defstruct (portability-finding (:conc-name finding-) (:constructor make-finding))
  file line kind detail)

(defvar *text* "")
(defvar *pos* 0)
(defvar *len* 0)
(defvar *findings* nil)
(defvar *file* nil)
(defvar *line-starts* nil)

(defun %line-of (pos)
  (let ((lo 0) (hi (1- (length *line-starts*))))
    (loop while (< lo hi)
          do (let ((mid (ceiling (+ lo hi) 2)))
               (if (<= (aref *line-starts* mid) pos) (setf lo mid) (setf hi (1- mid)))))
    (1+ lo)))

(defun %note (pos kind detail)
  (let ((line (%line-of pos)) (last (first *findings*)))
    (unless (and last (eql (finding-line last) line) (eq (finding-kind last) kind)
                 (equal (finding-detail last) detail))
      (push (make-finding :file *file* :line line :kind kind :detail detail)
            *findings*))))

(defun %peek (&optional (k 0))
  (let ((p (+ *pos* k)))
    (and (< p *len*) (char *text* p))))

(defun %whitespacep (c)
  (member c '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed)))

(defun %terminatingp (c)
  (or (%whitespacep c) (member c '(#\( #\) #\" #\' #\` #\, #\;))))

(defun %skip-block-comment ()
  ;; *pos* is just past "#|"
  (let ((depth 1))
    (loop while (and (< *pos* *len*) (> depth 0))
          do (cond ((and (eql (%peek) #\|) (eql (%peek 1) #\#)) (decf depth) (incf *pos* 2))
                   ((and (eql (%peek) #\#) (eql (%peek 1) #\|)) (incf depth) (incf *pos* 2))
                   (t (incf *pos*))))))

(defun %skip-blank ()
  (loop
    (let ((c (%peek)))
      (cond ((null c) (return))
            ((%whitespacep c) (incf *pos*))
            ((char= c #\;)
             (loop while (and (< *pos* *len*) (char/= (char *text* *pos*) #\Newline))
                   do (incf *pos*)))
            ((and (char= c #\#) (eql (%peek 1) #\|)) (incf *pos* 2) (%skip-block-comment))
            (t (return))))))

(defun %read-token-name ()
  "Read a token at *POS*. Return its upcased name with any package prefix removed."
  (let ((out (make-string-output-stream)) (colon nil))
    (loop
      (let ((c (%peek)))
        (cond ((or (null c) (%terminatingp c)) (return))
              ((char= c #\\) (incf *pos*)
               (when (%peek) (write-char (%peek) out) (incf *pos*)))
              ((char= c #\|) (incf *pos*)
               (loop while (and (%peek) (char/= (%peek) #\|))
                     do (when (char= (%peek) #\\) (incf *pos*))
                        (when (%peek) (write-char (%peek) out) (incf *pos*)))
               (incf *pos*))
              ((char= c #\:) (setf colon t) (incf *pos*)
               ;; drop the package prefix, keep reading the name
               (let ((so-far (get-output-stream-string out)))
                 (declare (ignore so-far))))
              (t (write-char (char-upcase c) out) (incf *pos*)))))
    (values (get-output-stream-string out) colon)))

(defun %read-string ()
  (incf *pos*)
  (let ((out (make-string-output-stream)))
    (loop
      (let ((c (%peek)))
        (cond ((null c) (return))
              ((char= c #\\) (incf *pos*) (when (%peek) (write-char (%peek) out) (incf *pos*)))
              ((char= c #\") (incf *pos*) (return))
              (t (write-char c out) (incf *pos*)))))
    (get-output-stream-string out)))

(defun %eval-feature (node target)
  (case (node-kind node)
    (:token (and (%target-has-p target (node-name node)) t))
    (:list
     (let* ((kids (node-children node))
            (head (and kids (eq (node-kind (first kids)) :token) (node-name (first kids)))))
       (cond ((equal head "OR") (some (lambda (k) (%eval-feature k target)) (rest kids)))
             ((equal head "AND") (every (lambda (k) (%eval-feature k target)) (rest kids)))
             ((equal head "NOT") (and (second kids) (not (%eval-feature (second kids) target))))
             (t nil))))
    (t nil)))

(defun %feature-mask (node)
  (let ((m 0))
    (loop for target in *portability-targets* for bit from 0
          when (%eval-feature node target) do (setf m (logior m (ash 1 bit))))
    m))

(defun %mentions-platform-p (node)
  (case (node-kind node)
    (:token (%platform-name-p (node-name node)))
    ((:list :prefix :readeval :other) (some #'%mentions-platform-p (node-children node)))
    (t nil)))

(defun %node-source (node)
  (let* ((start (node-start node))
         (end (min *len* (+ start 60)))
         (s (subseq *text* start end))
         (nl (position #\Newline s)))
    (if nl (subseq s 0 nl) s)))

(defun %clip (s)
  (let ((s (substitute #\Space #\Newline s)))
    (if (> (length s) 70) (concatenate 'string (subseq s 0 67) "...") s)))

(defun %mask-targets (mask)
  (loop for target in *portability-targets* for bit from 0
        when (logbitp bit mask) collect (first target)))

(defun %read-datum (mask)
  "Read one datum read on the targets in MASK. Returns a node, :CLOSE at a
closing parenthesis, or NIL at the end of the text."
  (%skip-blank)
  (let ((start *pos*) (c (%peek)))
    (flet ((make (kind &rest args) (apply #'%make-node :kind kind :start start :mask mask args)))
      (cond
        ((null c) nil)
        ((char= c #\() (incf *pos*) (make :list :children (%read-list mask)))
        ((char= c #\)) (incf *pos*) :close)
        ((char= c #\") (make :string :text (%read-string)))
        ((member c '(#\' #\`))
         (incf *pos*)
         (make :prefix :text (string c) :children (%child mask)))
        ((char= c #\,)
         (incf *pos*)
         (let ((p (if (member (%peek) '(#\@ #\.)) (prog1 (format nil ",~C" (%peek)) (incf *pos*)) ",")))
           (make :prefix :text p :children (%child mask))))
        ((char= c #\#) (%read-dispatch start mask))
        (t (make :token :name (%read-token-name)))))))

(defun %child (mask)
  (let ((d (%read-datum mask)))
    (if (or (null d) (eq d :close)) nil (list d))))

(defun %read-list (mask)
  (let ((items '()))
    (loop
      (let ((d (%read-datum mask)))
        (cond ((null d) (return))
              ((eq d :close) (return))
              (t (push d items)))))
    (nreverse items)))

(defun %read-dispatch (start mask)
  (incf *pos*)                          ; the #
  (loop while (and (%peek) (digit-char-p (%peek))) do (incf *pos*))
  (let ((c (%peek)))
    (flet ((make (kind &rest args) (apply #'%make-node :kind kind :start start :mask mask args)))
      (when c (incf *pos*))
      (case c
        ((nil) nil)
        (#\\ (when (%peek) (incf *pos*))
         (loop while (and (%peek) (not (%terminatingp (%peek)))) do (incf *pos*))
         (make :other))
        (#\' (make :prefix :text "#'" :children (%child mask)))
        (#\( (make :list :children (%read-list mask)))
        ((#\+ #\-)
         (let* ((expr (%read-datum *all-mask*))
                (expr (if (or (null expr) (eq expr :close)) nil expr))
                (expr-text (if expr (%clip (subseq *text* (node-start expr) *pos*)) ""))
                (m (if expr (%feature-mask expr) 0))
                (m (if (char= c #\+) m (logxor m *all-mask*)))
                (new (logand mask m)))
           (when (and expr (/= mask 0))
             (cond ((and (/= new mask) (/= new 0))
                    (%note start :read-platform
                           (format nil "#~C~A is true on ~{~A~^ ~} only"
                                   c expr-text (%mask-targets new))))
                   ((%mentions-platform-p expr)
                    (%note start :platform-constant
                           (format nil "#~C~A reads the same on every target" c expr-text)))))
           (let ((d (%read-datum new)))
             (cond ((or (null d) (eq d :close)) d)
                   ((= new 0) (%read-datum mask)) ; skipped: what follows takes its place
                   (t d)))))
        (#\. (make :readeval :children (%child mask)))
        (#\: (make :token :name (%read-token-name)))
        ((#\* #\x #\X #\b #\B #\o #\O #\r #\R)
         (%read-token-name) (make :other))
        ((#\c #\C #\a #\A #\s #\S #\p #\P #\=)
         (make :other :children (%child mask)))
        (#\# (make :other))
        (t (make :other))))))

;;; ------------------------------------------------------------------
;;; Compile-time probes.

(defparameter *machine-probes*
  '("MACHINE-TYPE" "MACHINE-VERSION" "SOFTWARE-TYPE" "SOFTWARE-VERSION"
    "OS-WINDOWS-P" "OS-UNIX-P" "OS-MACOSX-P" "OS-GENERA-P" "OS-HAIKU-P"
    "OS-MEZZANO-P" "OS-OLDMAC-P" "ARCHITECTURE" "OPERATING-SYSTEM"
    "IMPLEMENTATION-IDENTIFIER" "DETECT-OS"
    "FOREIGN-TYPE-SIZE" "FOREIGN-TYPE-ALIGNMENT")
  "Calls that ask which OS or CPU this is.")

(defparameter *feature-probes* '("*FEATURES*" "FEATUREP")
  "Reported as machine probes only when the same top-level form names a
platform feature; pushing a library's own feature is common and harmless.")

(defparameter *environment-probes*
  '("MACHINE-INSTANCE" "SHORT-SITE-NAME" "LONG-SITE-NAME" "GETENV"
    "POSIX-GETENV" "USER-HOMEDIR-PATHNAME"
    "*COMPILE-FILE-TRUENAME*" "*COMPILE-FILE-PATHNAME*" "*LOAD-TRUENAME*"
    "*LOAD-PATHNAME*" "SYSTEM-SOURCE-DIRECTORY" "SYSTEM-RELATIVE-PATHNAME"
    "CURRENT-LISP-FILE-PATHNAME")
  "Calls that ask about the machine's files and environment rather than its
OS or CPU. A fasl that keeps their compile-time answer breaks when the package
is unpacked somewhere else, even on the same platform.")

(defparameter *function-forms*
  '("DEFUN" "DEFMETHOD" "DEFGENERIC" "LAMBDA" "FLET" "LABELS" "DEFINE-CONDITION"
    "DEFCLASS" "DEFSTRUCT"))

(defparameter *expander-forms*
  '("DEFMACRO" "DEFINE-COMPILER-MACRO" "DEFTYPE" "DEFINE-SETF-EXPANDER"))

(defparameter *c-layout-forms* '("DEFCSTRUCT" "DEFCUNION" "DEFCTYPE"))
(defparameter *c-platform-types* '("LONG" "UNSIGNED-LONG" "ULONG"))
(defparameter *c-fixed-types*
  '("CHAR" "UNSIGNED-CHAR" "UCHAR" "SHORT" "UNSIGNED-SHORT" "USHORT" "INT"
    "UNSIGNED-INT" "UINT" "LONG-LONG" "UNSIGNED-LONG-LONG" "LLONG" "ULLONG"
    "INT8" "UINT8" "INT16" "UINT16" "INT32" "UINT32" "INT64" "UINT64"
    "FLOAT" "DOUBLE" "POINTER" "SIZE" "SSIZE" "INTPTR" "UINTPTR" "PTRDIFF")
  "CFFI types whose size and alignment are the same on every target.")

(defun %token-name (node)
  (and node (eq (node-kind node) :token) (node-name node)))

(defun %tokens-in (node)
  (case (node-kind node)
    (:token (list (node-name node)))
    ((:list :prefix :readeval :other) (mapcan #'%tokens-in (node-children node)))))

(defvar *toplevel* nil)
(defvar *toplevel-platform* :unknown)

(defun %toplevel-mentions-platform-p ()
  (when (eq *toplevel-platform* :unknown)
    (setf *toplevel-platform*
          (and *toplevel*
               (some (lambda (n) (%platform-name-p n)) (%tokens-in *toplevel*))
               t)))
  *toplevel-platform*)

(defun %walk (node why qdepth quoted in-function)
  "WHY is NIL at run time, or a string saying why this code runs at compile time."
  (when (and node (/= (node-mask node) 0))
    (case (node-kind node)
      (:token
       (let ((name (node-name node))
             (compile-time (and why (<= qdepth 0) (not quoted) (not in-function))))
         (cond ((and compile-time (member name *machine-probes* :test #'string=))
                (%note (node-start node) :compile-probe (format nil "~A in ~A" name why)))
               ((and compile-time (member name *feature-probes* :test #'string=)
                     (%toplevel-mentions-platform-p))
                (%note (node-start node) :compile-probe (format nil "~A (with platform features) in ~A" name why)))
               ((and compile-time (member name *environment-probes* :test #'string=))
                (%note (node-start node) :compile-environment (format nil "~A in ~A" name why)))
               ((and (not quoted) (or (not why) (> qdepth 0)))
                (when (member name '("OS-WINDOWS-P" "OS-UNIX-P" "OS-MACOSX-P" "OS-COND" "FEATUREP")
                              :test #'string=)
                  (%note (node-start node) :runtime-check name))))))
      (:prefix
       (let ((p (node-text node)) (child (first (node-children node))))
         (cond ((string= p "'") (%walk child why qdepth t in-function))
               ((string= p "`") (%walk child why (1+ qdepth) quoted in-function))
               ((char= (char p 0) #\,) (%walk child why (1- qdepth) quoted in-function))
               (t (%walk child why qdepth quoted in-function)))))
      (:readeval
       (%walk (first (node-children node)) "#." 0 nil nil))
      (:other (dolist (c (node-children node)) (%walk c why qdepth quoted in-function)))
      (:list (%walk-list node why qdepth quoted in-function)))))

(defun %walk-list (node why qdepth quoted in-function)
  (let* ((kids (node-children node))
         (head (%token-name (first kids)))
         (live (and (not quoted) (<= qdepth 0))))
    (flet ((walk-all (list w q qu f) (dolist (k list) (%walk k w q qu f)))
           (walk-body (list w)
             ;; Each form of a compile-time body is its own unit for the
             ;; "names a platform feature" test of *FEATURES* and FEATUREP.
             (dolist (k list)
               (let ((*toplevel* k) (*toplevel-platform* :unknown))
                 (%walk k w 0 nil nil)))))
      (cond
        ((not live) (walk-all kids why qdepth quoted in-function))
        ((equal head "EVAL-WHEN")
         (let ((situations (mapcar #'node-name
                                   (remove-if-not (lambda (n) (eq (node-kind n) :token))
                                                  (and (second kids) (node-children (second kids)))))))
           (if (or (member "COMPILE-TOPLEVEL" situations :test #'string=)
                   (member "COMPILE" situations :test #'string=))
               (walk-body (cddr kids) (or why "EVAL-WHEN (:COMPILE-TOPLEVEL)"))
               (walk-all (cddr kids) why qdepth quoted in-function))))
        ((member head *expander-forms* :test #'equal)
         (walk-body (cddr kids) (format nil "~A ~A" head (or (%token-name (second kids)) ""))))
        ((equal head "MACROLET")
         (when (second kids)
           (dolist (def (node-children (second kids)))
             (when (eq (node-kind def) :list)
               (walk-body (cdr (node-children def)) "MACROLET"))))
         (walk-all (cddr kids) why qdepth quoted in-function))
        ((member head '("DEFCONSTANT" "DEFINE-CONSTANT") :test #'equal)
         (when (third kids)
           (%walk (third kids) (or why (format nil "~A ~A" head (or (%token-name (second kids)) ""))) 0 nil nil))
         (walk-all (cdddr kids) why qdepth quoted in-function))
        ((and (member head '("FOREIGN-TYPE-SIZE" "FOREIGN-TYPE-ALIGNMENT") :test #'equal)
              (%token-name (second kids))
              (member (%token-name (second kids)) *c-fixed-types* :test #'string=))
         ;; The same size on every target: not a question about the platform.
         nil)
        ((member head *c-layout-forms* :test #'equal)
         (when (intersection (%tokens-in node) *c-platform-types* :test #'string=)
           (%note (node-start node) :c-layout
                  (format nil "~A ~A uses C long, whose size differs between Windows and the rest"
                          head (%node-source (or (second kids) node)))))
         (walk-all (cdr kids) why qdepth quoted in-function))
        ((and why (member head *function-forms* :test #'equal))
         ;; A function defined at compile time runs when something calls it. Inside
         ;; EVAL-WHEN that is not known; inside a macro expander it is the expander.
         (walk-all (cdr kids) why qdepth quoted (or in-function (string= why "EVAL-WHEN (:COMPILE-TOPLEVEL)"))))
        (t (walk-all kids why qdepth quoted in-function))))))

;;; ------------------------------------------------------------------
;;; Files.

(defun %read-file-text (path)
  (flet ((try (format)
           (with-open-file (s path :direction :input :external-format format)
             (let* ((buf (make-string (file-length s)))
                    (n (read-sequence buf s)))
               (subseq buf 0 n)))))
    (handler-case (try :utf-8)
      (error () (handler-case (try :latin-1) (error () nil))))))

(defun %parse-text (text)
  (setf *text* text *pos* 0 *len* (length text))
  (setf *line-starts*
        (coerce (cons 0 (loop for i from 0 below *len*
                              when (char= (char text i) #\Newline) collect (1+ i)))
                'vector))
  (let ((forms '()))
    (loop
      (let ((d (%read-datum *all-mask*)))
        (cond ((null d) (return))
              ((eq d :close))
              (t (push d forms)))))
    (nreverse forms)))

(defun %check-text (text file)
  "Findings for the source TEXT, attributed to FILE."
  (let ((*findings* '()) (*file* file) (*text* "") (*pos* 0) (*len* 0) (*line-starts* nil))
    (dolist (form (%parse-text text))
      (let ((*toplevel* form) (*toplevel-platform* :unknown))
        (%walk form nil 0 nil nil)))
    (stable-sort (nreverse *findings*) #'< :key #'finding-line)))

(defun check-file (path)
  "Return the findings for the Lisp source file PATH."
  (let ((text (%read-file-text path)))
    (if text
        (%check-text text (namestring path))
        (list (make-finding :file (namestring path) :line 0 :kind :unreadable
                            :detail "could not read the file")))))

;;; ------------------------------------------------------------------
;;; Which files: the system's components, as ASDF has them.

(defparameter *c-components*
  '("CFFI-GROVEL-FILE" "CFFI-WRAPPER-FILE" "GROVEL-FILE" "WRAPPER-FILE"
    "C-SOURCE-FILE" "C-FILE" "CFFI-C-FILE" "STATIC-C-FILE")
  "Component classes that compile or run C on the build machine.")

(defun %sexp-feature-mask (expr)
  "The targets on which the feature expression EXPR (as :IF-FEATURE holds it) is true."
  (labels ((ev (x target)
             (cond ((or (symbolp x) (stringp x)) (and (%target-has-p target (%feature-name x)) t))
                   ((and (consp x) (symbolp (car x)))
                    (let ((op (symbol-name (car x))))
                      (cond ((string= op "OR") (some (lambda (k) (ev k target)) (cdr x)))
                            ((string= op "AND") (every (lambda (k) (ev k target)) (cdr x)))
                            ((string= op "NOT") (not (ev (second x) target))))))
                   (t nil))))
    (let ((m 0))
      (loop for target in *portability-targets* for bit from 0
            when (ev expr target) do (setf m (logior m (ash 1 bit))))
      m)))

(defun %c-component-p (c)
  (member (symbol-name (class-name (class-of c))) *c-components* :test #'string=))

(defun %portability-systems (system)
  "SYSTEM and the systems of the same primary name it depends on -- the
package-inferred parts of a library, or a library/sub system it names. Other
libraries are theirs to check."
  (let* ((root (if (typep system 'asdf:system) system (asdf:find-system system)))
         (primary (asdf:primary-system-name root))
         (seen '()))
    (labels ((walk (s)
               (unless (member s seen :test #'eq)
                 (push s seen)
                 (dolist (d (asdf:system-depends-on s))
                   (let ((ds (ignore-errors
                              (asdf/find-component:resolve-dependency-spec s d))))
                     (when (and (typep ds 'asdf:system)
                                (string-equal (asdf:primary-system-name ds) primary))
                       (walk ds)))))))
      (walk root))
    (nreverse seen)))

(defun %portability-files (systems)
  "The Lisp source files of SYSTEMS that some target reads, and the findings
about the components themselves (C components, a choice made by :IF-FEATURE)."
  (let ((files '()) (findings '()))
    (labels ((note (c kind detail)
               (push (make-finding :file (namestring (or (asdf:system-source-file
                                                          (asdf:component-system c))
                                                         ""))
                                   :line 0 :kind kind :detail detail)
                     findings))
             (walk (c mask)
               (let* ((feature (asdf/component:component-if-feature c))
                      (here (if feature (logand mask (%sexp-feature-mask feature)) mask)))
                 (when (and feature (/= here 0) (/= here mask))
                   (note c :asd-platform-component
                         (format nil "~(~A~) ~S :if-feature is true on ~{~A~^ ~} only"
                                 (class-name (class-of c)) (asdf:component-name c)
                                 (%mask-targets here))))
                 (cond ((= here 0))
                       ((%c-component-p c)
                        (note c :c-probe (format nil "~(~A~) ~S runs C on the build machine"
                                                 (class-name (class-of c)) (asdf:component-name c))))
                       ((typep c 'asdf:parent-component)
                        (dolist (k (asdf:component-children c)) (walk k here)))
                       ((typep c 'asdf:cl-source-file)
                        (let ((p (asdf:component-pathname c)))
                          (when (and p (probe-file p))
                            (pushnew (namestring p) files :test #'string=))))))))
      (dolist (s systems) (walk s *all-mask*)))
    (values (nreverse files) (nreverse findings))))

(defun check-portability (system)
  "Check the sources of SYSTEM (a system or its name) for what would make their
fasls depend on the OS or CPU they are compiled on. Reads; loads and runs
nothing. Returns the findings, and as a second value the files read."
  (multiple-value-bind (files findings) (%portability-files (%portability-systems system))
    (values (append findings (loop for f in files append (check-file f)))
            files)))

;;; ------------------------------------------------------------------
;;; Reports.

(defparameter *kind-descriptions*
  '((:read-platform . "reader conditional that reads differently by OS or CPU")
    (:compile-probe . "OS or CPU looked at by code that runs at compile time")
    (:c-probe . "C compiled or run on the build machine (groveller, wrapper)")
    (:c-layout . "C type whose size differs by OS, in a struct or type definition")
    (:compile-environment . "build machine's paths or environment read at compile time")
    (:asd-platform-component . "OS or CPU tested in the .asd, which the consumer reads itself")
    (:platform-constant . "platform feature that reads the same on every target")
    (:runtime-check . "OS checked at run time (the portable way)")
    (:unreadable . "file could not be read")))

(defun blocking-kind-p (kind)
  "True for the kinds of finding that make a fasl depend on the OS or CPU."
  (member kind '(:read-platform :compile-probe :c-probe :c-layout)))

(defun %shown-p (finding)
  "Findings a report prints by default and a pack warns about: the blocking
kinds, and the build machine's paths, which tie a fasl to where it was built."
  (or (blocking-kind-p (finding-kind finding))
      (eq (finding-kind finding) :compile-environment)))

(defun %finding-line (f)
  (if (and (finding-line f) (> (finding-line f) 0))
      (format nil "~A:~D: ~(~A~): ~A" (finding-file f) (finding-line f) (finding-kind f) (finding-detail f))
      (format nil "~A: ~(~A~): ~A" (finding-file f) (finding-kind f) (finding-detail f))))

(defun summarize (findings)
  "An alist (KIND . COUNT), in the order of *KIND-DESCRIPTIONS*."
  (loop for (kind) in *kind-descriptions*
        for n = (count kind findings :key #'finding-kind)
        when (> n 0) collect (cons kind n)))

(defun report-portability (findings &key (stream *standard-output*) (verbose nil))
  "Print FINDINGS: one line each (the informational kinds only when VERBOSE),
then counts, then what the check cannot see. Return true when no finding
blocks a portable fasl."
  (dolist (f findings)
    (when (or verbose (%shown-p f))
      (format stream "~A~%" (%finding-line f))))
  (format stream "~&~%Counts:~%")
  (let ((sum (summarize findings)))
    (if sum
        (loop for (kind . n) in sum
              do (format stream "  ~5D  ~(~A~)  -- ~A~%" n kind
                         (cdr (assoc kind *kind-descriptions*))))
        (format stream "  nothing found~%")))
  (let ((ok (notany (lambda (f) (blocking-kind-p (finding-kind f))) findings)))
    (format stream "~%~:[One fasl may not be correct on every platform: see the lines above.~;~
Nothing found that ties a fasl to the platform it was compiled on.~]~%" ok)
    (format stream "~%Targets: ~{~A~^ ~}.~%This check reads the sources; it cannot prove a fasl portable:~%"
            (mapcar #'first *portability-targets*))
    (dolist (l *portability-limitations*) (format stream "  - ~A~%" l))
    ok))

;;; ------------------------------------------------------------------
;;; The operation.

(defclass check-portability-op (asdf:non-propagating-operation) ()
  (:documentation
   "Report what in a system's sources would make its fasls depend on the OS or
CPU they are compiled on: (asdf:operate 'asdf-nuget:check-portability-op \"my-lib\").
The system is read, not loaded or compiled."))

(defmethod asdf:perform ((o check-portability-op) (c asdf:component))
  nil)

(defmethod asdf:perform ((o check-portability-op) (s asdf:system))
  (report-portability (check-portability s)))

(defmethod asdf:operation-done-p ((o check-portability-op) (c asdf:component))
  "A check is never done: it reports on the sources as they are now."
  nil)

;;; `dotcl pack --library` ships fasls compiled here. A consumer on another OS or
;;; CPU compiles the sources instead, so a platform question answered at compile
;;; time costs it a compile rather than giving it wrong code; a build machine
;;; path read at compile time is wrong on every machine that loads the fasl. The
;;; pack runs the same check as CHECK-PORTABILITY-OP, warns, and goes on.

(define-condition fasl-portability-warning (warning)
  ((systems :initarg :systems :reader fasl-portability-warning-systems)
   (findings :initarg :findings :reader fasl-portability-warning-findings))
  (:report
   (lambda (c s)
     (let ((fs (fasl-portability-warning-findings c)))
       (format s "~D place~:P in the sources of ~{~A~^, ~} may tie the packed fasls ~
to the machine that compiled them; (asdf:operate 'asdf-nuget:check-portability-op ~S) ~
gives the full report.~{~%  ~A~}~:[~;~%  ...~]"
               (length fs)
               (fasl-portability-warning-systems c)
               (first (fasl-portability-warning-systems c))
               (mapcar #'%finding-line (subseq fs 0 (min 10 (length fs))))
               (> (length fs) 10))))))

(defun %warn-unportable (systems)
  "Run the portability check over SYSTEMS and warn when it finds something. A
check that fails is reported and does not stop the pack."
  (handler-case
      (multiple-value-bind (files findings) (%portability-files systems)
        (let ((shown (remove-if-not #'%shown-p
                                    (append findings (loop for f in files append (check-file f))))))
          (when shown
            (warn 'fasl-portability-warning
                  :systems (mapcar #'asdf:component-name systems)
                  :findings shown))))
    (error (e)
      (format *error-output* "~&; asdf-nuget: the portability check did not run (~A)~%" e))))

(provide "asdf-nuget")
