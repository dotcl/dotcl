;;;; Build-time only. Produces the two STABLE-NAMED .NET IL assemblies the
;;;; NativeAOT image links at build time and boots emit-free at run time:
;;;;   appfasl.dll   — this app's Lisp (compile-file :module-name "appfasl")
;;;;   dotclcore.dll — the runtime core (compiler+stdlib), converted from the
;;;;                   dev SIL core (sil-to-fasl ... "dotclcore")
;;;;
;;;; Why stable names matter: build-time-link references each
;;;; fasl as a fixed-name assembly, and the AOT/ILC toolchain resolves assemblies
;;;; by SIMPLE NAME via the file name. The default compile-file/sil-to-fasl name
;;;; carries a per-build guid (app_1e23687b…), which would change every build and
;;;; cannot be referenced. :module-name / the 3rd sil-to-fasl arg pin a stable
;;;; name, and FILE NAME == INTERNAL ASSEMBLY NAME (appfasl.dll / dotclcore.dll)
;;;; is required so ILC's GetModuleForSimpleName finds them.
;;;;
;;;; The dotcl compiler used here itself uses Reflection.Emit — that is the
;;;; dev/build side. The shipped native binary never emits. Paths are relative to
;;;; the build working directory ($(MSBuildProjectDirectory)).
(compile-file "app.lisp" :output-file "appfasl.dll" :module-name "appfasl")
(dotcl:sil-to-fasl "../../compiler/cil-out.sil" "dotclcore.dll" "dotclcore")
(dotcl:quit 0)
