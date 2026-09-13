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
;;; Ordering is the system's business, as with any other component: put the
;;; :nuget component before the files that need it and mark the system :serial t,
;;; or name it in a :depends-on.

(require "asdf")
(require "nuget")

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
  (apply #'nuget:require (asdf:component-name c) (%require-arguments c)))

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
concatenated file, where the (REQUIRE \"nuget\") that creates it is part of the
same form."
  (with-output-to-string (s)
    (format s "(eval-when (:compile-toplevel :load-toplevel :execute)~%")
    (format s "  (cl:require \"nuget\")~%")
    (format s "  (funcall (find-symbol \"REQUIRE\" \"NUGET\") ~S" (asdf:component-name c))
    (loop for (k v) on (%require-arguments c) by #'cddr
          do (format s " ~S ~S" k v))
    (format s "))")))

(defun %plist-without (plist key)
  (loop for (k v) on plist by #'cddr
        unless (eq k key) append (list k v)))

(defun resolve-system-for-rid (system rid)
  "Lay out every package SYSTEM declares for RID, and report what would not lay out.

Returns a list of (PACKAGE-NAME . MESSAGE) for the ones that failed, empty when
all of them worked. A failure is not fatal: a package can legitimately have
nothing for a platform, and the answer to that is to ship the other packages and
let this one resolve on the target -- not to refuse to build for that platform.

A component that names its own :RID is left alone. It asked for a specific
platform's assets, and packaging for a different one does not change that."
  (let ((failures '()))
    (dolist (c (system-nuget-components system) (nreverse failures))
      (unless (nuget-rid c)
        (let ((args (%require-arguments c)))
          (handler-case
              ;; REQUIRE, not RESOLVE: the point is to have the layout on hand
              ;; afterwards, and it is REQUIRE that records where it went (RESOLVE
              ;; always does the work and remembers nothing). Recording it is what
              ;; NUGET:STAGE-BUNDLE then reads.
              (apply #'nuget:require (asdf:component-name c) :rid rid
                     (%plist-without args :rid))
            (error (e)
              (push (cons (asdf:component-name c) (princ-to-string e)) failures))))))))

(defun system-nuget-preamble (system)
  "The forms that make a built artifact resolve SYSTEM's declared NuGet packages,
as one string, or NIL when the system declares none.

Order follows the components: a system that declares two packages asks for them
in the order it wrote them, as LOAD-OP would."
  (let ((cs (system-nuget-components system)))
    (when cs
      (format nil "~{~A~%~}" (mapcar #'%preamble-form cs)))))

(provide "dotcl-nuget-asdf")
