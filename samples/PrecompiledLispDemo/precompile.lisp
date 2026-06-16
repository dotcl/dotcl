;;;; Build-time only. Produces the two artifacts the shipped app loads, both as
;;;; precompiled .NET IL assemblies (loaded at run time without code generation):
;;;;   app.fasl   — this app's Lisp, compiled from app.lisp
;;;;   dotcl.core — the runtime core (compiler+stdlib), converted from the dev
;;;;                SIL core so the app boots emit-less too (the SIL core would
;;;;                emit on load; the FASL core loads via ModuleInit).
;;;; The dotcl compiler used here itself uses Reflection.Emit — that is the
;;;; dev/build side. Paths are relative to the build working directory.
(compile-file "app.lisp" :output-file "app.fasl")
(dotcl:sil-to-fasl "../../compiler/cil-out.sil" "dotcl.core")
(dotcl:quit 0)
