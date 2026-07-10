;;; advice.lisp — interactive advice on live .NET methods, from Lisp.
;;;
;;; Wraps Lib.Harmony (github.com/pardeike/Harmony) so you can attach CLOS-style
;;; :after advice to *any* .NET method at run time, no restart:
;;;
;;;   (require "advice")
;;;   (advice:watch "MyApp.OrderService" "Process"
;;;     (lambda (instance args result)
;;;       (format t "~&Process~s => ~s~%" args result)))
;;;   ...
;;;   (advice:unwatch "MyApp.OrderService" "Process")
;;;
;;; The Lisp closure runs after each call to the target and receives the
;;; instance (NIL for static methods), the argument list, and the return value.
;;; It is read-only — its value is ignored (changing the result is a later
;;; `patch`).
;;;
;;; The backend is HarmonyX (BepInEx's fork of Harmony) resolved from NuGet via
;;; (require "nuget"). It is a drop-in of Lib.Harmony (same HarmonyLib namespace,
;;; same 0Harmony.dll) but references MonoMod as external packages rather than
;;; ILMerging an old copy, so it picks up the MonoMod version whose detour
;;; backend supports ARM64 on .NET 10 — Lib.Harmony 2.4.2's bundled MonoMod
;;; 1.3.3 throws "Abi field is not set" on arm64 (Apple Silicon / Windows ARM).
;;; The universal postfix that bridges back into Lisp lives in the runtime
;;; (DotCL.MethodAdviceBridge) so no C# build step is needed here.

;; Load nuget before any `nuget:` symbol below is read (forms are read+evaluated
;; one at a time, so this must precede the defun that references the package).
(require "nuget")

(defpackage :advice
  (:use :cl)
  ;; trace / untrace shadow the CL debugging macros of the same name.
  (:shadow #:trace #:untrace)
  (:export #:watch #:unwatch #:patch #:unpatch #:trace #:untrace #:ensure
           #:*harmony-package* #:*harmony-version*))

(in-package :advice)

(defvar *harmony-package* "HarmonyX"
  "NuGet package for the patching backend. HarmonyX (BepInEx) is a drop-in of
Lib.Harmony with ARM64 .NET 10 support; use \"Lib.Harmony\" for the original.")

(defvar *harmony-version* "2.16.1"
  "Version of *harmony-package* to resolve.")

(defvar *harmony* nil "The HarmonyLib.Harmony instance (one shared id).")
(defvar *bridge-postfix* nil "MethodInfo of the read-only bridge postfix (watch).")
(defvar *bridge-patch* nil "MethodInfo of the result-rewriting bridge postfix (patch).")
(defvar *bridge-trace-prefix* nil "MethodInfo of the trace prefix (start stamp).")
(defvar *bridge-trace-postfix* nil "MethodInfo of the trace postfix (elapsed).")
(defvar *watched* (make-hash-table :test 'equal)
  "(type . method) string pair -> original MethodInfo, for unwatch.")
(defvar *patched* (make-hash-table :test 'equal)
  "(type . method) string pair -> original MethodInfo, for unpatch.")
(defvar *traced* (make-hash-table :test 'equal)
  "(type . method) string pair -> original MethodInfo, for untrace.")

(defun ensure ()
  "Idempotently resolve Harmony and build the shared instance + bridge handle.

Fast path: if 0Harmony is already resolvable — because the host app's csproj
carries a <PackageReference Include=\"HarmonyX\"> (project-core build) or a prior
nuget:require registered it — just load it, no `dotnet build`. Only when it is
absent (e.g. the bare `dotcl` tool with no host project) fall back to resolving
from NuGet at run time, which shells out to `dotnet build` (~seconds, needs the
SDK + network). So an embedding app gets instant `(require \"advice\")` by
declaring the dependency at build time; the contrib needs no change either way."
  (unless *harmony*
    (or (ignore-errors (dotnet:load-assembly "0Harmony"))
        (progn
          (nuget:require *harmony-package* :version *harmony-version*)
          (dotnet:load-assembly "0Harmony")))
    (setf *harmony* (dotnet:new "HarmonyLib.Harmony" "dotcl.harmony"))
    (let ((bridge (dotnet:resolve-type "DotCL.MethodAdviceBridge")))
      (setf *bridge-postfix*      (dotnet:invoke bridge "GetMethod" "Postfix")
            *bridge-patch*        (dotnet:invoke bridge "GetMethod" "PostfixReplace")
            *bridge-trace-prefix* (dotnet:invoke bridge "GetMethod" "TracePrefix")
            *bridge-trace-postfix* (dotnet:invoke bridge "GetMethod" "TracePostfix"))))
  *harmony*)

(defun %resolve-method (type-name method-name)
  (let ((ty (or (dotnet:resolve-type type-name)
                (error "advice: type not found: ~A" type-name))))
    (or (dotnet:invoke ty "GetMethod" method-name)
        (error "advice: method not found: ~A.~A" type-name method-name))))

(defun watch (type-name method-name fn)
  "Run FN (instance args-list result) after each call to TYPE-NAME.METHOD-NAME.
Re-watching the same method replaces the closure."
  (ensure)
  (let ((orig (%resolve-method type-name method-name))
        (key (cons type-name method-name)))
    ;; Register the closure first, then instrument (so no call can race in
    ;; between and hit a patched method with no handler).
    (dotnet:static "DotCL.MethodAdviceBridge" "RegisterPostfix" orig fn)
    (unless (gethash key *watched*)
      (dotnet:invoke *harmony* "Patch"
                     orig nil (dotnet:new "HarmonyLib.HarmonyMethod" *bridge-postfix*)))
    (setf (gethash key *watched*) orig)
    t))

(defun unwatch (type-name method-name)
  "Remove the observer from TYPE-NAME.METHOD-NAME and restore the original method."
  (ensure)
  (let* ((key (cons type-name method-name))
         (orig (gethash key *watched*)))
    (when orig
      (dotnet:invoke *harmony* "Unpatch" orig *bridge-postfix*)
      (dotnet:static "DotCL.MethodAdviceBridge" "UnregisterPostfix" orig)
      (remhash key *watched*)
      t)))

(defun patch (type-name method-name fn)
  "Replace the return value of TYPE-NAME.METHOD-NAME with the value of
FN (instance args-list result). FN sees the original result and returns the
value the caller will receive instead — this is an in-place fix, no restart.
Re-patching the same method replaces the closure."
  (ensure)
  (let ((orig (%resolve-method type-name method-name))
        (key (cons type-name method-name)))
    (dotnet:static "DotCL.MethodAdviceBridge" "RegisterPatch" orig fn)
    (unless (gethash key *patched*)
      (dotnet:invoke *harmony* "Patch"
                     orig nil (dotnet:new "HarmonyLib.HarmonyMethod" *bridge-patch*)))
    (setf (gethash key *patched*) orig)
    t))

(defun unpatch (type-name method-name)
  "Remove the return-value rewrite from TYPE-NAME.METHOD-NAME."
  (ensure)
  (let* ((key (cons type-name method-name))
         (orig (gethash key *patched*)))
    (when orig
      (dotnet:invoke *harmony* "Unpatch" orig *bridge-patch*)
      (dotnet:static "DotCL.MethodAdviceBridge" "UnregisterPatch" orig)
      (remhash key *patched*)
      t)))

(defun %default-trace-printer (type-name method-name)
  "A trace closure that prints one line per call: name, args and elapsed ms."
  (lambda (instance args result seconds)
    (declare (ignore instance result))
    (format t "~&[trace] ~A.~A~S  ~,3Fms~%"
            type-name method-name args (* seconds 1000.0))))

(defun trace (type-name method-name &optional fn)
  "Time every call to TYPE-NAME.METHOD-NAME. FN receives
(instance args-list result elapsed-seconds); when omitted, a default printer
logs the call and its wall time. Re-tracing replaces the closure."
  (ensure)
  (let ((orig (%resolve-method type-name method-name))
        (key (cons type-name method-name)))
    (dotnet:static "DotCL.MethodAdviceBridge" "RegisterTrace" orig
                   (or fn (%default-trace-printer type-name method-name)))
    (unless (gethash key *traced*)
      ;; One Patch installs both halves: prefix stamps the start, postfix reports.
      (dotnet:invoke *harmony* "Patch"
                     orig
                     (dotnet:new "HarmonyLib.HarmonyMethod" *bridge-trace-prefix*)
                     (dotnet:new "HarmonyLib.HarmonyMethod" *bridge-trace-postfix*)))
    (setf (gethash key *traced*) orig)
    t))

(defun untrace (type-name method-name)
  "Remove timing from TYPE-NAME.METHOD-NAME."
  (ensure)
  (let* ((key (cons type-name method-name))
         (orig (gethash key *traced*)))
    (when orig
      (dotnet:invoke *harmony* "Unpatch" orig *bridge-trace-prefix*)
      (dotnet:invoke *harmony* "Unpatch" orig *bridge-trace-postfix*)
      (dotnet:static "DotCL.MethodAdviceBridge" "UnregisterTrace" orig)
      (remhash key *traced*)
      t)))
