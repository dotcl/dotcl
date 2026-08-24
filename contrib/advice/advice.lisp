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
;;; The universal patch methods that bridge back into Lisp are emitted from
;;; Lisp itself (dotnet:define-class), so this contrib is pure Lisp with no C#
;;; build step and nothing of its own in the runtime.

;; Load nuget before any `nuget:` symbol below is read (forms are read+evaluated
;; one at a time, so this must precede the defun that references the package).
(require "nuget")
;; dotnet:define-class / dotnet:deref are read below, so the contrib that
;; exports them has to be loaded before those forms are read.
(require "dotnet-class")

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
(defvar *bridge-postfix* nil "MethodInfo of the read-only postfix (watch).")
(defvar *bridge-patch* nil "MethodInfo of the result-rewriting postfix (patch).")
(defvar *bridge-trace-prefix* nil "MethodInfo of the trace prefix (start stamp).")
(defvar *bridge-trace-postfix* nil "MethodInfo of the trace postfix (elapsed).")
(defvar *watched* (make-hash-table :test 'equal)
  "(type . method) string pair -> original MethodInfo, for unwatch.")
(defvar *patched* (make-hash-table :test 'equal)
  "(type . method) string pair -> original MethodInfo, for unpatch.")
(defvar *traced* (make-hash-table :test 'equal)
  "(type . method) string pair -> original MethodInfo, for untrace.")

;;; ---------------------------------------------------------------------------
;;; The Harmony patch methods, emitted from Lisp.
;;;
;;; Harmony calls a patch by reflection and fills its parameters BY NAME, so
;;; what it needs is a static method with the agreed spelling: __originalMethod,
;;; __instance, __args, __result, __state. dotnet:define-class emits exactly
;;; that, which is why these live here rather than in a C# file inside the
;;; runtime -- the contrib is Lisp all the way down and needs no build step.
;;;
;;; __result (for patch) and __state (for trace) are `ref' parameters: the
;;; caller reads back what the body writes. They arrive as cells, written with
;;; (setf (dotnet:deref cell) v).

(defvar *patch-type* "DotCL.Advice.Patches")

(defvar *post-handlers* (make-hash-table :test 'equal)
  "method key -> closure, for watch.")
(defvar *patch-handlers* (make-hash-table :test 'equal)
  "method key -> closure, for patch.")
(defvar *trace-handlers* (make-hash-table :test 'equal)
  "method key -> closure, for trace.")

(defun %method-key (method)
  "A key that is EQUAL for two MethodBase objects describing the same method.
Reflection may hand back a different wrapper each lookup, so the identity of
the object cannot be relied on; its declaring type and signature can."
  (format nil "~A|~A"
          (dotnet:invoke (dotnet:invoke method "DeclaringType") "FullName")
          (dotnet:invoke method "ToString")))

(defun %args->list (args)
  "Harmony's boxed argument vector as a Lisp list, in order."
  (when args
    (loop for i from 0 below (dotnet:invoke args "Length")
          collect (dotnet:invoke args "GetValue" i))))

(defun %ticks () (dotnet:static "System.Diagnostics.Stopwatch" "GetTimestamp"))
(defun %tick-frequency () (dotnet:static "System.Diagnostics.Stopwatch" "Frequency"))

(defun %define-patch-methods ()
  "Emit the four patch methods Harmony will call. Idempotent by *patch-type*."
  (dotnet:define-class "DotCL.Advice.Patches" ()
    (:methods
     ;; watch: read-only observer. The closure's value is ignored.
     ("Postfix" ((|__originalMethod| "System.Reflection.MethodBase")
                 (|__instance| "System.Object")
                 (|__args| "System.Object[]")
                 (|__result| "System.Object"))
       :returns "System.Void" :static t
       (let ((fn (gethash (%method-key |__originalMethod|) *post-handlers*)))
         (when fn
           (funcall fn |__instance| (%args->list |__args|) |__result|)))
       nil)

     ;; patch: the closure's value becomes the method's result.
     ("PostfixReplace" ((|__originalMethod| "System.Reflection.MethodBase")
                        (|__instance| "System.Object")
                        (|__args| "System.Object[]")
                        (|__result| "System.Object&"))
       :returns "System.Void" :static t
       (let ((fn (gethash (%method-key |__originalMethod|) *patch-handlers*)))
         (when fn
           (setf (dotnet:deref |__result|)
                 (funcall fn |__instance| (%args->list |__args|)
                          (dotnet:deref |__result|)))))
       nil)

     ;; trace, first half: stamp the start into __state, which Harmony threads
     ;; through to the postfix for the same call (so recursion nests correctly).
     ;; Stamped only when a closure is registered, so an untraced method pays
     ;; nothing beyond the lookup.
     ("TracePrefix" ((|__originalMethod| "System.Reflection.MethodBase")
                     (|__state| "System.Object&"))
       :returns "System.Void" :static t
       (when (gethash (%method-key |__originalMethod|) *trace-handlers*)
         (setf (dotnet:deref |__state|) (%ticks)))
       nil)

     ;; trace, second half: elapsed wall time from the prefix's stamp.
     ("TracePostfix" ((|__originalMethod| "System.Reflection.MethodBase")
                      (|__instance| "System.Object")
                      (|__args| "System.Object[]")
                      (|__result| "System.Object")
                      (|__state| "System.Object"))
       :returns "System.Void" :static t
       (let ((fn (gethash (%method-key |__originalMethod|) *trace-handlers*)))
         (when (and fn (integerp |__state|))
           (funcall fn |__instance| (%args->list |__args|) |__result|
                    (/ (float (- (%ticks) |__state|) 1.0d0)
                       (%tick-frequency)))))
       nil)))
  (let ((ty (dotnet:resolve-type *patch-type*)))
    (setf *bridge-postfix*       (dotnet:invoke ty "GetMethod" "Postfix")
          *bridge-patch*         (dotnet:invoke ty "GetMethod" "PostfixReplace")
          *bridge-trace-prefix*  (dotnet:invoke ty "GetMethod" "TracePrefix")
          *bridge-trace-postfix* (dotnet:invoke ty "GetMethod" "TracePostfix"))))

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
    (%define-patch-methods))
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
    (setf (gethash (%method-key orig) *post-handlers*) fn)
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
      (remhash (%method-key orig) *post-handlers*)
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
    (setf (gethash (%method-key orig) *patch-handlers*) fn)
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
      (remhash (%method-key orig) *patch-handlers*)
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
    (setf (gethash (%method-key orig) *trace-handlers*)
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
      (remhash (%method-key orig) *trace-handlers*)
      (remhash key *traced*)
      t)))
