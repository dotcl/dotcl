;;; decompiler.lisp — decompile a .NET assembly back to readable C#, from Lisp.
;;;
;;; Wraps ICSharpCode.Decompiler (the ILSpy engine) so you can recover readable
;;; C# for a type or a single method whose source you don't have — a production
;;; assembly, a NuGet dependency, or something a live process is running. The
;;; "read" half of the probe/patch loop (its patch companion is `advice`):
;;;
;;;   (require "decompiler")
;;;   (decompiler:type-source   "MyApp.OrderService")            ; whole type -> C#
;;;   (decompiler:method-source "MyApp.OrderService" "Process")  ; one method -> C#
;;;
;;; The target is located by name through dotcl's own type resolver, so anything
;;; `(dotnet:resolve-type ...)` can find — a loaded assembly, or one probed from
;;; the app base directory — is decompilable from its on-disk file.
;;;
;;; Decompilation is pure static analysis (no runtime codegen): the engine ships
;;; as a single netstandard2.0 assembly and references no Reflection.Emit, so the
;;; decompiler itself is emit-free/AOT-safe. Two deployment notes for a locked-down
;;; emit-free host (IL2CPP / NativeAOT / iOS), where this contrib otherwise works:
;;;   - `(require "nuget")` can't run there (no SDK to shell `dotnet build` out to),
;;;     so bundle ICSharpCode.Decompiler.dll alongside the runtime and let `ensure`
;;;     load it by name instead of resolving it at run time.
;;;   - precompile this file to a fasl ahead of time; an emit-free host can't
;;;     compile the .lisp source at load.
;;; Trimming an IL2CPP/AOT build may still strip types the engine reflects over —
;;; verify against a real player build before relying on it in that mode.

;; Load nuget before any `nuget:` symbol below is read (forms are read+evaluated
;; one at a time, so this must precede the defuns that reference the package).
(require "nuget")

(defpackage :decompiler
  (:use :cl)
  (:export #:type-source #:method-source #:ensure
           #:*package-name* #:*package-version*))

(in-package :decompiler)

(defvar *package-name* "ICSharpCode.Decompiler"
  "NuGet package providing the decompiler engine (the ILSpy backend).")

(defvar *package-version* "*"
  "Version of *package-name* to resolve. \"*\" = latest stable; pin a string
for reproducibility.")

(defvar *ready* nil "T once the decompiler assembly is resolvable.")

(defun ensure ()
  "Idempotently make ICSharpCode.Decompiler resolvable.

Fast path: if the assembly is already loadable — the host app references it, or
a prior call resolved it — just load it, no `dotnet build`. Only when absent
(e.g. the bare `dotcl` tool) fall back to resolving it from NuGet at run time,
which shells out to `dotnet build` (~seconds, needs the SDK + network)."
  (unless *ready*
    (or (ignore-errors (dotnet:load-assembly "ICSharpCode.Decompiler"))
        (progn
          (nuget:require *package-name* :version *package-version*)
          (dotnet:load-assembly "ICSharpCode.Decompiler")))
    (setf *ready* t)))

(defun %decompiler-for-type (type-name)
  "Build a CSharpDecompiler over the on-disk file of the assembly that defines
TYPE-NAME (resolved through dotcl's type resolver)."
  (let* ((ty (dotnet:resolve-type type-name))
         (asm (dotnet:invoke ty "get_Assembly"))
         (loc (dotnet:invoke asm "get_Location")))
    (when (or (null loc) (string= loc ""))
      (error "decompiler: no on-disk location for the assembly of ~A ~
(dynamic or single-file-embedded assemblies cannot be decompiled)." type-name))
    (values
     (dotnet:new "ICSharpCode.Decompiler.CSharp.CSharpDecompiler"
                 loc
                 (dotnet:new "ICSharpCode.Decompiler.DecompilerSettings"))
     ty)))

(defun type-source (type-name)
  "Return the decompiled C# source of TYPE-NAME as a string."
  (ensure)
  (let ((dc  (%decompiler-for-type type-name))
        (ftn (dotnet:new "ICSharpCode.Decompiler.TypeSystem.FullTypeName" type-name)))
    (dotnet:invoke dc "DecompileTypeAsString" ftn)))

(defun method-source (type-name method-name)
  "Return the decompiled C# source of TYPE-NAME's METHOD-NAME as a string.
Uses the loaded method's metadata token, so an overloaded name resolves to the
CLR's default pick — name a distinct method for now."
  (ensure)
  (multiple-value-bind (dc ty) (%decompiler-for-type type-name)
    (let* ((mi (dotnet:invoke ty "GetMethod" method-name))
           (_  (when (null mi)
                 (error "decompiler: ~A has no method ~A" type-name method-name)))
           (token  (dotnet:invoke mi "get_MetadataToken"))
           (handle (dotnet:static
                    "System.Reflection.Metadata.Ecma335.MetadataTokens"
                    "EntityHandle" token)))
      (declare (ignore _))
      (dotnet:invoke dc "DecompileAsString" handle))))

(provide "decompiler")
